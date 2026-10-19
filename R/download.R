#' Download INMET historical data archives
#'
#' Downloads annual ZIP archives (one per year, containing every automatic
#' station) from INMET's historical data portal into a local cache.
#'
#' @param years Integer vector of years to download. Archives start in 2000.
#'   Defaults to the current year.
#' @param dest_dir Cache directory. It is created when necessary.
#' @param max_tries Maximum attempts per year.
#' @param quiet Suppress progress messages.
#' @param force Download again even when a valid archive is cached.
#' @param check_updates For cached archives of the current and previous year,
#'   ask the server whether a newer version exists and download it if so.
#'   INMET keeps appending data to these archives. Older years are never
#'   re-checked. Requires a (very small) request per recent year.
#'
#' @return Invisibly, a named character vector with the paths of the archives
#'   available after the call.
#'
#' @details
#' Archives are written to `<year>.zip.part` and only renamed to `<year>.zip`
#' after their size has been checked against the size reported by the server
#' and the ZIP structure has been validated, so a partial file is never
#' mistaken for a complete one. Interrupted downloads resume from where they
#' stopped on the next call. A small `<year>.zip.meta` file records the
#' server's version information.
#'
#' The INMET server sends one archive at a time per client, so years are
#' downloaded sequentially. Typical archives for recent years are 90 to 110 MB.
#'
#' @examples
#' \donttest{
#' paths <- inmet_download(2023, dest_dir = file.path(tempdir(), "rmet"))
#' }
#' @seealso [inmet_get()], [inmet_read()], [inmet_cache_status()]
#' @export
inmet_download <- function(
    years = .current_year(),
    dest_dir = tools::R_user_dir("rmet", "cache"),
    max_tries = 15L,
    quiet = FALSE,
    force = FALSE,
    check_updates = TRUE
) {
  years <- .validate_years(years)
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  max_tries <- .validate_positive_integer(max_tries, "max_tries")
  quiet <- .validate_flag(quiet, "quiet")
  force <- .validate_flag(force, "force")
  check_updates <- .validate_flag(check_updates, "check_updates")

  if (!dir.exists(dest_dir) && !dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create `dest_dir`: ", dest_dir, call. = FALSE)
  }

  results <- character()
  for (year in years) {
    path <- file.path(dest_dir, paste0(year, ".zip"))
    part <- paste0(path, ".part")

    if (force) {
      existing <- c(path, part, .meta_path(path))
      existing <- existing[file.exists(existing)]
      if (length(existing) && !quiet) message("Removing existing file(s) for ", year, ".")
      if (length(existing) && any(!file.remove(existing))) {
        warning("Could not remove every existing file for ", year, ".", call. = FALSE)
        next
      }
    }

    if (.is_valid_zip(path)) {
      if (check_updates && year >= .current_year() - 1L && .server_has_update(year, path)) {
        if (!quiet) message("Year ", year, " was updated on the server; downloading new version.")
        if (file.exists(part)) file.remove(part)
        if (.download_one_year(year, path, part, max_tries, quiet)) {
          results[as.character(year)] <- path
        } else {
          warning("Keeping the previously cached archive for ", year, ".", call. = FALSE)
          results[as.character(year)] <- path
        }
        next
      }
      if (!quiet) message("Year ", year, " already cached and valid.")
      results[as.character(year)] <- path
      next
    }

    # An invalid <year>.zip (e.g. left by rmet <= 0.1.0) is resumed as a partial file.
    if (file.exists(path) && !file.exists(part)) {
      file.rename(path, part)
    } else if (file.exists(path)) {
      file.remove(path)
    }

    if (.download_one_year(year, path, part, max_tries, quiet)) {
      results[as.character(year)] <- path
    }
  }
  invisible(results)
}

.archive_url <- function(year) {
  paste0("https://portal.inmet.gov.br/uploads/dadoshistoricos/", year, ".zip")
}

.download_one_year <- function(year, destfile, partfile, max_tries, quiet) {
  url <- .archive_url(year)
  if (.is_valid_zip(partfile)) return(.finalize_download(partfile, destfile, year, quiet))

  for (attempt in seq_len(max_tries)) {
    already <- if (file.exists(partfile)) file.size(partfile) else 0
    if (is.na(already)) already <- 0

    handle <- curl::new_handle()
    curl::handle_setopt(
      handle,
      useragent = .rmet_user_agent(),
      referer = "https://portal.inmet.gov.br/dadoshistoricos",
      resume_from = already,
      connecttimeout = 60L,
      low_speed_limit = 1000L,
      low_speed_time = 60L,
      followlocation = TRUE,
      failonerror = TRUE
    )

    error_message <- NULL
    con <- file(partfile, open = "ab")
    response <- tryCatch(
      curl::curl_fetch_stream(url, fun = function(bytes) writeBin(bytes, con), handle = handle),
      error = function(e) {
        error_message <<- conditionMessage(e)
        NULL
      },
      finally = close(con)
    )
    if (!is.null(error_message) && !quiet) {
      message("  Attempt ", attempt, " failed: ", error_message)
    }

    status <- .response_status(response)
    headers <- .response_headers(response)
    # A server that ignores the byte range answers 200 with the whole file,
    # which was appended after the old partial bytes: drop those bytes.
    if (identical(status, 200L) && already > 0) .drop_prefix(partfile, already)
    expected <- .expected_size(status, headers, already)

    size <- if (file.exists(partfile)) file.size(partfile) else 0
    if (!quiet) {
      if (is.na(expected)) {
        message(sprintf("  Total on disk: %.1f MB", size / 1e6))
      } else {
        message(sprintf("  Total on disk: %.1f of %.1f MB", size / 1e6, expected / 1e6))
      }
    }

    if ((is.na(expected) || isTRUE(size == expected)) && .is_valid_zip(partfile)) {
      .write_meta(destfile, headers, size)
      return(.finalize_download(partfile, destfile, year, quiet))
    }

    restart <- isTRUE(size > expected) ||
      (!is.null(error_message) && grepl("range|resume|416", error_message, ignore.case = TRUE))
    if (restart && file.exists(partfile)) file.remove(partfile)

    if (attempt < max_tries) Sys.sleep(min(5, attempt))
  }

  warning(sprintf("Failed to download year %d after %d attempt(s).", year, max_tries),
          call. = FALSE)
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
    message("  Year ", year, " OK - ", nrow(utils::unzip(destfile, list = TRUE)),
            " file(s) in archive.")
  }
  TRUE
}

# --- HTTP helpers ----------------------------------------------------------

.response_status <- function(response) {
  if (is.list(response) && length(response$status_code)) as.integer(response$status_code) else NA_integer_
}

.response_headers <- function(response) {
  if (!is.list(response) || !length(response$headers)) return(list())
  headers <- tryCatch(curl::parse_headers_list(response$headers), error = function(e) list())
  stats::setNames(headers, tolower(names(headers)))
}

# Total size of the complete file implied by a response, or NA when unknown.
.expected_size <- function(status, headers, already = 0) {
  if (identical(status, 206L) && length(headers[["content-range"]])) {
    total <- sub(".*/", "", headers[["content-range"]])
    return(suppressWarnings(as.numeric(total)))
  }
  if (identical(status, 200L) && length(headers[["content-length"]])) {
    return(suppressWarnings(as.numeric(headers[["content-length"]])))
  }
  NA_real_
}

# Removes the first `n` bytes of a file, copying in chunks.
.drop_prefix <- function(path, n) {
  tmp <- paste0(path, ".tmp")
  input <- file(path, "rb")
  output <- file(tmp, "wb")
  seek(input, where = n)
  repeat {
    chunk <- readBin(input, what = "raw", n = 8e6)
    if (!length(chunk)) break
    writeBin(chunk, output)
  }
  close(input)
  close(output)
  file.remove(path)
  file.rename(tmp, path)
}

# Server metadata of an archive from a HEAD request; NULL when unavailable.
.remote_metadata <- function(year) {
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle, nobody = TRUE, useragent = .rmet_user_agent(),
    connecttimeout = 20L, timeout = 30L, followlocation = TRUE, failonerror = TRUE
  )
  response <- tryCatch(curl::curl_fetch_memory(.archive_url(year), handle = handle),
                       error = function(e) NULL)
  if (is.null(response)) return(NULL)
  headers <- .response_headers(response)
  list(
    size = suppressWarnings(as.numeric(headers[["content-length"]] %||% NA)),
    etag = headers[["etag"]] %||% NA_character_,
    last_modified = headers[["last-modified"]] %||% NA_character_
  )
}

.server_has_update <- function(year, path) {
  remote <- .remote_metadata(year)
  if (is.null(remote)) return(FALSE)
  local <- .read_meta(path)
  if (!is.null(local) && !is.na(local$etag) && !is.na(remote$etag)) {
    return(!identical(local$etag, remote$etag))
  }
  !is.na(remote$size) && !isTRUE(file.size(path) == remote$size)
}

# --- version sidecar (<year>.zip.meta) -----------------------------------------

.meta_path <- function(path) paste0(path, ".meta")

.write_meta <- function(destfile, headers, size) {
  meta <- data.frame(
    size = size,
    etag = headers[["etag"]] %||% NA_character_,
    last_modified = headers[["last-modified"]] %||% NA_character_,
    downloaded = format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    stringsAsFactors = FALSE
  )
  try(write.dcf(meta, .meta_path(destfile)), silent = TRUE)
}

.read_meta <- function(path) {
  meta_file <- .meta_path(path)
  if (!file.exists(meta_file)) return(NULL)
  meta <- tryCatch(read.dcf(meta_file), error = function(e) NULL)
  if (is.null(meta) || !nrow(meta)) return(NULL)
  value <- function(field) if (field %in% colnames(meta)) unname(meta[1, field]) else NA_character_
  list(size = as.numeric(value("size")), etag = value("etag"),
       last_modified = value("last_modified"))
}

`%||%` <- function(x, y) if (is.null(x) || !length(x)) y else x
