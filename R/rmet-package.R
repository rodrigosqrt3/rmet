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
#' * [inmet_monthly()], [inmet_et0()] and [inmet_derive()] add monthly values,
#'   evapotranspiration and derived variables; [inmet_pet_thornthwaite()] and
#'   [inmet_water_balance()] give the water balance.
#' * [inmet_indices()], [inmet_spi()], [inmet_normals()] and [inmet_anomaly()]
#'   describe the climate; [inmet_fill()] fills short gaps.
#' * [inmet_nearest()], [inmet_stations_within()] and [inmet_interpolate()]
#'   relate stations to locations and areas.
#' * [inmet_availability()] shows which stations are in the cached archives.
#' * [inmet_as_sf()] converts stations or data to spatial points.
#' * [inmet_download()], [inmet_read()], and [inmet_extract()] give
#'   step-by-step control.
#' * [inmet_cache_status()], [inmet_cache_clear()] and [inmet_cache_dir()] manage the local cache.
#'
#' @keywords internal
#' @useDynLib rmet, .registration = TRUE, .fixes = "C_"
"_PACKAGE"
