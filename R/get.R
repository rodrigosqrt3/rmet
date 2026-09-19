#' Download and read INMET data in one call
#'
#' This is the recommended high-level interface. It downloads missing annual
#' archives, reuses valid cached files, and returns the result of [inmet_read()].
#'
#' @inheritParams inmet_read
#' @param max_tries Maximum download attempts per year.
#' @param force Download archives again even if they are valid.
#'
#' @return A data frame as documented in [inmet_read()].
#'
#' @examples
#' \donttest{
#' weather <- inmet_get(
#'   years = 2023,
#'   stations = "A801",
#'   variables = c("temp_dry_c", "precip_mm"),
#'   dest_dir = tempdir()
#' )
#' }
#' @export
inmet_get <- function(
    years,
    stations = NULL,
    dest_dir = tools::R_user_dir("rmet", "cache"),
    tz = "UTC",
    variables = NULL,
    start_date = NULL,
    end_date = NULL,
    max_tries = 15L,
    force = FALSE,
    quiet = FALSE
) {
  years <- .validate_years(years)
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  inmet_download(
    years = years,
    dest_dir = dest_dir,
    max_tries = max_tries,
    quiet = quiet,
    force = force
  )
  missing <- years[!vapply(
    file.path(dest_dir, paste0(years, ".zip")),
    .is_valid_zip,
    logical(1)
  )]
  if (length(missing)) {
    stop("Could not obtain valid archive(s) for: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  inmet_read(
    years = years,
    stations = stations,
    dest_dir = dest_dir,
    tz = tz,
    variables = variables,
    start_date = start_date,
    end_date = end_date,
    quiet = quiet
  )
}
