#' Interpolate station values to other locations
#'
#' Estimates a variable at any location (for example, the centroids of
#' municipalities or the points of a grid) from the values observed at the
#' stations, by inverse-distance weighting of the nearest stations.
#'
#' @param x A data frame with one row per station: `latitude`, `longitude`
#'   and the column named in `variable`, such as monthly or daily values of a
#'   single period. Stations with a missing value are ignored.
#' @param targets A data frame with `latitude` and `longitude` columns.
#' @param variable Name of the numeric column of `x` to interpolate.
#' @param power Exponent of the distance in the weights (2 by default).
#' @param k Number of nearest stations used for each target.
#' @param max_distance_km Stations farther than this from a target are not
#'   used; targets without any station in range get `NA`.
#'
#' @return `targets` with the interpolated column (named as `variable`),
#'   `n_stations` (stations used) and `nearest_km` (distance to the closest
#'   one).
#'
#' @details
#' A target that coincides with a station gets that station's value. Inverse
#' distance weighting is simple and makes no assumption about the spatial
#' structure, but it cannot extrapolate beyond the observed values, ignores
#' elevation, and is poor for rainfall in sparse networks; use it for a first
#' estimate or for filling gaps, not for precise surfaces.
#'
#' @examples
#' stations <- inmet_stations(state = "RS", source = "bundled")
#' stations$elevation_obs <- stations$elevation
#' targets <- data.frame(latitude = c(-30, -29), longitude = c(-51, -53))
#' inmet_interpolate(stations, targets, "elevation_obs", k = 3)
#' @seealso [inmet_nearest()], [inmet_monthly()]
#' @export
inmet_interpolate <- function(x, targets, variable, power = 2, k = 5L,
                              max_distance_km = Inf) {
  if (!is.data.frame(x) || !all(c("latitude", "longitude") %in% names(x))) {
    stop("`x` must be a data frame with `latitude` and `longitude` columns.",
         call. = FALSE)
  }
  if (!is.data.frame(targets) || !all(c("latitude", "longitude") %in% names(targets))) {
    stop("`targets` must be a data frame with `latitude` and `longitude` columns.",
         call. = FALSE)
  }
  if (!is.character(variable) || length(variable) != 1L || !is.numeric(x[[variable]])) {
    stop("`variable` must name one numeric column of `x`.", call. = FALSE)
  }
  power <- .validate_number(power, "power", 0.1, 20)
  k <- .validate_positive_integer(k, "k")
  if (!is.numeric(max_distance_km) || length(max_distance_km) != 1L ||
      is.na(max_distance_km) || max_distance_km < 0) {
    stop("`max_distance_km` must be one non-negative number (or Inf).", call. = FALSE)
  }
  target_lat <- .validate_coordinates(targets$latitude, "latitude", 90)
  target_lon <- .validate_coordinates(targets$longitude, "longitude", 180)

  usable <- !is.na(x[[variable]]) & !is.na(x$latitude) & !is.na(x$longitude)
  lat <- as.numeric(x$latitude[usable])
  lon <- as.numeric(x$longitude[usable])
  value <- as.numeric(x[[variable]][usable])

  estimate <- numeric(length(target_lat))
  used <- integer(length(target_lat))
  nearest <- numeric(length(target_lat))
  for (i in seq_along(target_lat)) {
    distance <- .haversine_km(target_lat[[i]], target_lon[[i]], lat, lon)
    picked <- order(distance)[seq_len(min(k, length(distance)))]
    picked <- picked[distance[picked] <= max_distance_km]
    used[[i]] <- length(picked)
    if (!length(picked)) {
      estimate[[i]] <- NA_real_
      nearest[[i]] <- if (length(distance)) min(distance) else NA_real_
      next
    }
    d <- distance[picked]
    nearest[[i]] <- d[[1]]
    estimate[[i]] <- if (d[[1]] < 1e-6) {
      value[picked[[1]]]
    } else {
      weight <- 1 / d^power
      sum(weight * value[picked]) / sum(weight)
    }
  }
  out <- targets
  out[[variable]] <- estimate
  out$n_stations <- used
  out$nearest_km <- nearest
  out
}
