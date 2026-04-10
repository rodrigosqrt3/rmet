#' rmet: Access Historical Weather Data from INMET
#'
#' @description
#' The `rmet` package provides tools to download, cache, and parse
#' historical hourly weather data from Brazil's National Institute of
#' Meteorology (INMET).
#'
#' ## Main functions
#'
#' * [inmet_download()] — Download annual ZIP files with resume support.
#' * [inmet_read()] — Parse downloaded ZIPs into a tidy `data.frame`.
#' * [inmet_extract()] — Unzip CSVs to a directory.
#' * [inmet_stations()] — Browse the bundled station catalogue.
#'
#' @section Data source:
#' Data is retrieved from
#' "portal.inmet.gov.br/dadoshistoricos". Please consult INMET's
#' terms of use before redistributing the raw data.
#'
#' @keywords internal
"_PACKAGE"


#' Check which years have already been downloaded
#'
#' Convenience function that scans the cache directory and reports which
#' annual ZIPs are present and valid.
#'
#' @param dest_dir Character. Cache directory (same as [inmet_download()]).
#'
#' @return A `data.frame` with columns `year` (integer), `path` (character),
#'   `size_mb` (numeric), and `valid` (logical, whether the ZIP passes
#'   integrity check).
#'
#' @examples
#' \donttest{
#' inmet_cache_status()
#' }
#' @export
inmet_cache_status <- function(dest_dir = tools::R_user_dir("rmet", "cache")) {
  zips <- list.files(dest_dir, pattern = "^\\d{4}\\.zip$", full.names = TRUE)

  if (length(zips) == 0L) {
    message("No downloaded ZIPs found in: ", dest_dir)
    return(invisible(data.frame(year = integer(), path = character(),
                                size_mb = numeric(), valid = logical())))
  }

  years <- as.integer(sub("\\.zip$", "", basename(zips)))
  size  <- file.size(zips) / 1e6

  valid <- vapply(zips, function(z) {
    test <- tryCatch(utils::unzip(z, list = TRUE), error = function(e) NULL)
    !is.null(test) && nrow(test) > 0L
  }, logical(1))

  out <- data.frame(
    year    = years,
    path    = zips,
    size_mb = round(size, 1),
    valid   = valid,
    stringsAsFactors = FALSE
  )
  out[order(out$year), ]
}


#' Clear the rmet download cache
#'
#' Deletes downloaded ZIP files from the cache directory.
#'
#' @param years Integer vector of years to remove, or `NULL` to remove all.
#' @param dest_dir Character. Cache directory.
#' @param ask Logical. If `TRUE` (default in interactive sessions), prompt
#'   before deleting.
#'
#' @return Invisible `NULL`.
#' @export
inmet_cache_clear <- function(
    years    = NULL,
    dest_dir = tools::R_user_dir("rmet", "cache"),
    ask      = interactive()
) {
  if (!dir.exists(dest_dir)) {
    message("Cache directory does not exist: ", dest_dir)
    return(invisible(NULL))
  }

  if (is.null(years)) {
    targets <- list.files(dest_dir, pattern = "^\\d{4}\\.zip$", full.names = TRUE)
  } else {
    targets <- file.path(dest_dir, paste0(as.integer(years), ".zip"))
    targets <- targets[file.exists(targets)]
  }

  if (length(targets) == 0L) {
    message("Nothing to delete.")
    return(invisible(NULL))
  }

  if (ask) {
    msg <- sprintf("Delete %d file(s) from '%s'? [y/N] ", length(targets), dest_dir)
    ans <- readline(msg)
    if (!grepl("^[Yy]", ans)) {
      message("Aborted.")
      return(invisible(NULL))
    }
  }

  removed <- file.remove(targets)
  message(sprintf("Deleted %d/%d file(s).", sum(removed), length(targets)))
  invisible(NULL)
}
