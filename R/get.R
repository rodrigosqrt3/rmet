#' Download and read INMET data in one call
#'
#' The recommended high-level interface: downloads missing annual archives
#' with [inmet_download()], reuses valid cached files, and returns the result
#' of [inmet_read()].
#'
#' @inheritParams inmet_read
#' @inheritParams inmet_download
#'
#' @return A data frame as documented in [inmet_read()].
#'
#' @examples
#' \donttest{
#' weather <- inmet_get(
#'   years = 2023,
#'   stations = "A801",
#'   variables = c("temp_dry_c", "precip_mm"),
#'   dest_dir = file.path(tempdir(), "rmet")
#' )
#' daily <- inmet_daily(weather)
#' }
#' @seealso [inmet_daily()], [inmet_stations()], [inmet_nearest()]
#' @export
inmet_get <- function(
    years,
    stations = NULL,
    dest_dir = inmet_cache_dir(),
    tz = "UTC",
    variables = NULL,
    start_date = NULL,
    end_date = NULL,
    drop_empty = TRUE,
    cache = FALSE,
    max_tries = 15L,
    force = FALSE,
    check_updates = TRUE,
    quiet = FALSE
) {
  years <- .validate_years(years)
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  inmet_download(
    years = years, dest_dir = dest_dir, max_tries = max_tries, quiet = quiet,
    force = force, check_updates = check_updates
  )
  available <- vapply(file.path(dest_dir, paste0(years, ".zip")), .is_valid_zip,
                      logical(1), USE.NAMES = FALSE)
  if (!all(available)) {
    stop("Could not obtain valid archive(s) for: ",
         paste(years[!available], collapse = ", "), call. = FALSE)
  }
  inmet_read(
    years = years, stations = stations, dest_dir = dest_dir, tz = tz,
    variables = variables, start_date = start_date, end_date = end_date,
    drop_empty = drop_empty, cache = cache, quiet = quiet
  )
}
