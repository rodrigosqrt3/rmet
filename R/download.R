#' Download INMET historical data ZIPs with resume support
#'
#' Downloads one or more annual ZIP files from INMET's historical data portal.
#' Downloads are resumable: if a partial file is already on disk (e.g., from a
#' previous interrupted session), the function picks up where it left off
#' instead of restarting.
#'
#' @param years Integer vector of years to download. Available years start from
#'   2000. Defaults to the current year.
#' @param dest_dir Character. Directory where ZIP files will be saved.
#'   Created automatically if it does not exist. Defaults to a persistent
#'   cache directory under `tools::R_user_dir("rmet", "cache")`.
#' @param max_tries Integer. Maximum number of download attempts per year
#'   before giving up. Defaults to `15`.
#' @param quiet Logical. If `TRUE`, suppresses progress messages.
#'   Defaults to `FALSE`.
#' @param force Logical. If `TRUE`, deletes any existing (possibly partial)
#'   ZIP before downloading. Defaults to `FALSE`.
#'
#' @return Invisibly returns a named character vector (class `character`) of
#'   local file paths to the downloaded ZIP files. Names correspond to the
#'   requested years (e.g., `"2023"`). Years that failed to download are
#'   excluded. If all downloads fail, an empty named character vector is
#'   returned.
#'
#' @details
#' INMET's server sometimes drops connections mid-transfer. The function
#' handles this by using `curl`'s `CURLOPT_RESUME_FROM` to append to the
#' partial file on each retry, then validates the result with
#' [utils::unzip()]. A `Sys.sleep(5)` back-off separates retries.
#'
#' ZIP files are **not** extracted by this function. Use [inmet_read()] to
#' parse the contents directly from the ZIP, or [inmet_extract()] to unzip
#' to a directory.
#'
#' @examples
#' \donttest{
#' paths <- inmet_download(years = 2000, dest_dir = tempdir())
#' }
#' @seealso [inmet_read()], [inmet_extract()]
#' @export
inmet_download <- function(
    years    = as.integer(format(Sys.Date(), "%Y")),
    dest_dir = tools::R_user_dir("rmet", "cache"),
    max_tries = 15L,
    quiet    = FALSE,
    force    = FALSE
) {
  # ---- input validation -------------------------------------------------------
  years <- as.integer(years)
  if (any(is.na(years))) stop("`years` must be a vector of integers.", call. = FALSE)
  if (any(years < 2000L)) stop("INMET data is only available from 2000 onwards.", call. = FALSE)
  if (any(years > as.integer(format(Sys.Date(), "%Y")))) {
    stop("Cannot request years in the future.", call. = FALSE)
  }
  max_tries <- as.integer(max_tries)
  if (length(max_tries) != 1L || is.na(max_tries) || max_tries < 1L) {
    stop("`max_tries` must be a positive integer.", call. = FALSE)
  }

  if (!dir.exists(dest_dir)) dir.create(dest_dir, recursive = TRUE)

  results <- character(0)

  for (yr in years) {
    path <- file.path(dest_dir, paste0(yr, ".zip"))

    if (force && file.exists(path)) {
      if (!quiet) message("Removing existing file: ", path)
      file.remove(path)
    }

    ok <- .download_one_year(yr, destfile = path, max_tries = max_tries, quiet = quiet)
    if (ok) results[as.character(yr)] <- path
  }

  invisible(results)
}


# ------------------------------------------------------------------------------
# Internal: download (with resume) a single year
# ------------------------------------------------------------------------------
.download_one_year <- function(year, destfile, max_tries, quiet) {
  url <- paste0(
    "https://portal.inmet.gov.br/uploads/dadoshistoricos/",
    year, ".zip"
  )

  for (i in seq_len(max_tries)) {
    already <- if (file.exists(destfile)) file.size(destfile) else 0L

    h <- curl::new_handle()
    curl::handle_setopt(
      h,
      useragent      = paste0(
        "Mozilla/5.0 (compatible; rmet/0.1.0; ",
        "+https://github.com/yourgh/rmet)"
      ),
      referer        = "https://portal.inmet.gov.br/dadoshistoricos",
      resume_from    = already,
      connecttimeout = 30L,
      low_speed_limit = 1000L,   # bytes/s
      low_speed_time  = 30L      # abort if below limit for 30 s
    )

    con <- file(destfile, open = "ab")
    tryCatch(
      curl::curl_fetch_stream(url, fun = function(bytes) writeBin(bytes, con), handle = h),
      error = function(e) {
        if (!quiet) message("  Connection dropped: ", conditionMessage(e))
      }
    )
    close(con)

    size <- file.size(destfile)
    if (!quiet) message(sprintf("  Total on disk: %.1f MB", size / 1e6))

    test <- tryCatch(utils::unzip(destfile, list = TRUE), error = function(e) data.frame(Name = character(0)))
    n_files_in_zip <- nrow(test)

    if (n_files_in_zip > 0L) {
      if (!quiet) {
        message(sprintf(
          "  Year %d OK - ZIP valid, %d station files.",
          year, n_files_in_zip
        ))
      }
      return(TRUE)
    }

    if (i < max_tries) Sys.sleep(5)
  }

  warning(
    sprintf("Failed to download year %d after %d attempts.", year, max_tries),
    call. = FALSE
  )
  FALSE
}

.safe_rbind <- function(list_df) {
  if (length(list_df) == 0L) return(NULL)
  all_cols <- unique(unlist(lapply(list_df, names)))
  list_df <- lapply(list_df, function(d) {
    missing_cols <- setdiff(all_cols, names(d))
    if (length(missing_cols) > 0L) {
      d[missing_cols] <- NA
    }
    d[all_cols]
  })
  do.call(rbind, list_df)
}
