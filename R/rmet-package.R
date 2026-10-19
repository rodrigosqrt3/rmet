#' rmet: Access Historical Weather Data from INMET
#'
#' Tools to find stations and to download, cache, parse, and summarise
#' historical hourly weather data from Brazil's National Institute of
#' Meteorology (INMET).
#'
#' @section Main functions:
#' * [inmet_get()] downloads and reads data in one call.
#' * [inmet_stations()] and [inmet_nearest()] search the official station
#'   catalogue.
#' * [inmet_qc()] flags implausible values and [inmet_completeness()]
#'   reports missing data.
#' * [inmet_daily()] builds daily summaries in local time
#'   ([inmet_timezone()]).
#' * [inmet_as_sf()] converts stations or data to spatial points.
#' * [inmet_download()], [inmet_read()], and [inmet_extract()] give
#'   step-by-step control.
#' * [inmet_cache_status()] and [inmet_cache_clear()] manage the local cache.
#'
#' @keywords internal
"_PACKAGE"
