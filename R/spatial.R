#' Convert stations or data to spatial points
#'
#' Turns any data frame with `latitude` and `longitude` columns, such as the
#' output of [inmet_stations()], [inmet_nearest()], [inmet_read()] or
#' [inmet_daily()], into an `sf` object for mapping and spatial joins.
#' Requires the \pkg{sf} package.
#'
#' @param x A data frame with numeric `latitude` and `longitude` columns.
#' @param remove Drop the coordinate columns from the attributes.
#'
#' @return An `sf` data frame with point geometries in WGS 84 (EPSG:4326).
#'   Rows with missing coordinates are dropped with a warning.
#'
#' @examples
#' if (requireNamespace("sf", quietly = TRUE)) {
#'   rs <- inmet_stations(state = "RS", source = "bundled")
#'   points <- inmet_as_sf(rs)
#'   plot(points["elevation"])
#' }
#' @export
inmet_as_sf <- function(x, remove = FALSE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("Package 'sf' is required: install.packages(\"sf\").", call. = FALSE)
  }
  if (!is.data.frame(x) || !all(c("latitude", "longitude") %in% names(x))) {
    stop("`x` must be a data frame with `latitude` and `longitude` columns.", call. = FALSE)
  }
  remove <- .validate_flag(remove, "remove")
  missing <- is.na(x$latitude) | is.na(x$longitude)
  if (any(missing)) {
    warning(sum(missing), " row(s) without coordinates were dropped.", call. = FALSE)
    x <- x[!missing, , drop = FALSE]
  }
  sf::st_as_sf(as.data.frame(x), coords = c("longitude", "latitude"), crs = 4326,
               remove = remove)
}
