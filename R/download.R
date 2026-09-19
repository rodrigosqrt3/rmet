#' Download INMET historical data archives
#'
#' Downloads one or more annual ZIP files from INMET's historical data portal.
#' Complete archives already in the cache are reused. Interrupted downloads are
#' kept in a `.part` file and resumed on the next attempt.
#'
#' @param years Integer vector of years to download. Available years start in
#'   2000. Defaults to the current year.
#' @param dest_dir Character. Cache directory. It is created when necessary.
#' @param max_tries Positive integer. Maximum attempts per year.
#' @param quiet Logical. Suppress progress messages.
#' @param force Logical. Download again even when a valid archive is cached.
#'
#' @return Invisibly, a named character vector containing the paths of all
#'   successfully downloaded archives.
#'
#' @details
#' Files are first written to `<year>.zip.part`. A partial file is never exposed
#' as a complete ZIP. After an integrity check succeeds, the file is atomically
#' moved to `<year>.zip` whenever the operating system permits it.
#'
#' @examples
#' \donttest{
#' paths <- inmet_download(2023, dest_dir = tempdir())
#' }
#' @seealso [inmet_read()], [inmet_get()], [inmet_extract()]
#' @export
inmet_download <- function(
    years = as.integer(format(Sys.Date(), "%Y")),
    dest_dir = tools::R_user_dir("rmet", "cache"),
    max_tries = 15L,
    quiet = FALSE,
    force = FALSE
) {
  years <- .validate_years(years)
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  max_tries <- .validate_positive_integer(max_tries, "max_tries")
  quiet <- .validate_flag(quiet, "quiet")
  force <- .validate_flag(force, "force")

  if (!dir.exists(dest_dir) &&
      !dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create `dest_dir`: ", dest_dir, call. = FALSE)
  }

  results <- character()

  for (yr in unique(years)) {
    path <- file.path(dest_dir, paste0(yr, ".zip"))
    part <- paste0(path, ".part")

    if (force) {
      existing <- c(path, part)
      existing <- existing[file.exists(existing)]
      if (length(existing) && !quiet) {
        message("Removing existing file(s) for ", yr, ".")
      }
      if (length(existing) && any(!file.remove(existing))) {
        warning("Could not remove every existing file for ", yr, ".", call. = FALSE)
        next
      }
    }

    if (.is_valid_zip(path)) {
      if (!quiet) message("Year ", yr, " already cached and valid.")
      results[as.character(yr)] <- path
      next
    }

    # Preserve invalid/partial files created by rmet <= 0.1.0.
    if (file.exists(path) && !file.exists(part)) {
      file.rename(path, part)
    } else if (file.exists(path)) {
      file.remove(path)
    }

    ok <- .download_one_year(
      year = yr,
      destfile = path,
      partfile = part,
      max_tries = max_tries,
      quiet = quiet
    )
    if (ok) results[as.character(yr)] <- path
  }

  invisible(results)
}

.download_one_year <- function(year, destfile, partfile, max_tries, quiet) {
  url <- paste0(
    "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
    year,
    ".zip"
  )

  if (.is_valid_zip(partfile)) {
    return(.finalize_download(partfile, destfile, year, quiet))
  }

  for (attempt in seq_len(max_tries)) {
    already <- if (file.exists(partfile)) file.size(partfile) else 0
    if (is.na(already)) already <- 0

    handle <- curl::new_handle()
    curl::handle_setopt(
      handle,
      useragent = .rmet_user_agent(),
      referer = "https://portal.inmet.gov.br/dadoshistoricos",
      resume_from = already,
      connecttimeout = 30L,
      timeout = 600L,
      low_speed_limit = 1000L,
      low_speed_time = 30L,
      followlocation = TRUE,
      failonerror = TRUE
    )

    error_message <- NULL
    con <- file(partfile, open = "ab")
    tryCatch(
      curl::curl_fetch_stream(
        url,
        fun = function(bytes) writeBin(bytes, con),
        handle = handle
      ),
      error = function(e) {
        error_message <<- conditionMessage(e)
      },
      finally = close(con)
    )

    if (!is.null(error_message) && !quiet) {
      message("  Attempt ", attempt, " failed: ", error_message)
    }

    size <- if (file.exists(partfile)) file.size(partfile) else 0
    if (!quiet) message(sprintf("  Total on disk: %.1f MB", size / 1e6))

    if (.is_valid_zip(partfile)) {
      return(.finalize_download(partfile, destfile, year, quiet))
    }

    # Some servers reject byte-range requests. Restart cleanly in that case.
    if (!is.null(error_message) &&
        grepl("range|resume|416", error_message, ignore.case = TRUE) &&
        file.exists(partfile)) {
      file.remove(partfile)
    }

    if (attempt < max_tries) Sys.sleep(min(5, attempt))
  }

  warning(
    sprintf("Failed to download year %d after %d attempt(s).", year, max_tries),
    call. = FALSE
  )
  FALSE
}

.finalize_download <- function(partfile, destfile, year, quiet) {
  if (file.exists(destfile)) file.remove(destfile)
  moved <- file.rename(partfile, destfile)
  if (!moved) {
    moved <- file.copy(partfile, destfile, overwrite = TRUE)
    if (moved) file.remove(partfile)
  }
  if (!moved || !.is_valid_zip(destfile)) {
    warning("Downloaded archive could not be finalized for year ", year, ".", call. = FALSE)
    return(FALSE)
  }
  if (!quiet) {
    entries <- utils::unzip(destfile, list = TRUE)
    message("  Year ", year, " OK - ", nrow(entries), " file(s) in archive.")
  }
  TRUE
}
