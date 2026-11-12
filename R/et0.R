#' Reference evapotranspiration
#'
#' Adds the daily grass reference evapotranspiration `et0_mm` to the output of
#' [inmet_daily()]. The default is the FAO Irrigation and Drainage Paper 56
#' Penman-Monteith equation; the Hargreaves-Samani equation needs only
#' temperature.
#'
#' @param x A data frame as returned by [inmet_daily()]. All methods need
#'   `date`, `latitude`, `temp_max_c` and `temp_min_c`. Penman-Monteith also
#'   needs `elevation`, `wind_speed_mean_ms`, either `temp_dew_mean_c` or
#'   `humid_rel_mean_pct`, and `radiation_mj_m2` (unless
#'   `estimate_radiation = TRUE`).
#' @param method `"penman-monteith"` (default) or `"hargreaves"`.
#' @param estimate_radiation For Penman-Monteith: use the radiation estimated
#'   from the daily temperature range (Hargreaves) on days when the measured
#'   `radiation_mj_m2` is missing, or in every day when the column is absent.
#' @param coastal Use the coastal coefficient (0.19) instead of the interior
#'   one (0.16) when estimating radiation.
#'
#' @return `x` with the new column `et0_mm` (mm/day); `NA` on days when any
#'   input is missing.
#'
#' @details
#' **Penman-Monteith** follows FAO-56: mean temperature as the average of the
#' daily extremes, actual vapour pressure from the mean dew point (or, if it is
#' not available, from the mean relative humidity), net radiation from the
#' measured global radiation with the clear-sky radiation of the station's
#' latitude and elevation, no soil heat flux, and pressure from the
#' elevation. INMET wind sensors are at 10 m, so wind speed is converted to
#' 2 m with the logarithmic profile.
#'
#' **Hargreaves-Samani** is `0.0023 * (Tmean + 17.8) * sqrt(Tmax - Tmin) * Ra`
#' with the extraterrestrial radiation `Ra` in mm/day. It is less accurate but
#' does not depend on radiation, wind or humidity, which are often missing.
#'
#' Daily radiation is the sum of the hourly values, and so is an
#' underestimate on days with missing daytime hours; use the `min_hours` rule
#' of [inmet_daily()] and check the result before using it in applications
#' such as irrigation scheduling.
#'
#' @examples
#' daily <- inmet_daily(rmet_example)
#' daily <- inmet_et0(daily)
#' summary(daily$et0_mm)
#'
#' # temperature-only alternative
#' hs <- inmet_et0(daily, method = "hargreaves")
#' @seealso [inmet_daily()], [inmet_monthly()], [inmet_design_et0()]
#' @export
inmet_et0 <- function(x, method = c("penman-monteith", "hargreaves"),
                      estimate_radiation = FALSE, coastal = FALSE) {
  if (.is_no_data(x)) return(x)
  method <- match.arg(method)
  estimate_radiation <- .validate_flag(estimate_radiation, "estimate_radiation")
  coastal <- .validate_flag(coastal, "coastal")
  penman <- method == "penman-monteith"

  needed <- c("date", "latitude", "temp_max_c", "temp_min_c")
  if (penman) {
    needed <- c(needed, "elevation", "wind_speed_mean_ms",
                if (!estimate_radiation) "radiation_mj_m2")
  }
  missing <- setdiff(needed, names(x))
  if (!is.data.frame(x) || length(missing)) {
    stop("`x` must be the output of inmet_daily() with the columns: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  if (penman && !any(c("temp_dew_mean_c", "humid_rel_mean_pct") %in% names(x))) {
    stop("`x` needs `temp_dew_mean_c` or `humid_rel_mean_pct`.", call. = FALSE)
  }

  tmax <- as.numeric(x$temp_max_c)
  tmin <- as.numeric(x$temp_min_c)
  tmean <- (tmax + tmin) / 2
  lat <- as.numeric(x$latitude) * pi / 180
  doy <- as.numeric(format(x$date, "%j"))
  ra <- .extraterrestrial_radiation(lat, doy)

  if (!penman) {
    x$et0_mm <- 0.0023 * (tmean + 17.8) * sqrt(pmax(tmax - tmin, 0)) * ra * 0.408
    return(x)
  }

  rs <- if ("radiation_mj_m2" %in% names(x)) {
    as.numeric(x$radiation_mj_m2)
  } else {
    rep(NA_real_, nrow(x))
  }
  if (estimate_radiation) {
    estimated <- (if (coastal) 0.19 else 0.16) * sqrt(pmax(tmax - tmin, 0)) * ra
    rs[is.na(rs)] <- estimated[is.na(rs)]
  }
  z <- as.numeric(x$elevation)
  u2 <- as.numeric(x$wind_speed_mean_ms) * 4.87 / log(67.8 * 10 - 5.42)

  es <- (.saturation_vapor_pressure(tmax) + .saturation_vapor_pressure(tmin)) / 2
  ea <- if ("temp_dew_mean_c" %in% names(x)) {
    .saturation_vapor_pressure(as.numeric(x$temp_dew_mean_c))
  } else {
    .saturation_vapor_pressure(tmean) * as.numeric(x$humid_rel_mean_pct) / 100
  }
  if ("temp_dew_mean_c" %in% names(x) && "humid_rel_mean_pct" %in% names(x)) {
    # a day without dew point falls back to relative humidity
    fallback <- is.na(ea)
    ea[fallback] <- (.saturation_vapor_pressure(tmean) *
                       as.numeric(x$humid_rel_mean_pct) / 100)[fallback]
  }

  rso <- (0.75 + 2e-5 * z) * ra
  ratio <- pmin(1, rs / rso)
  rns <- 0.77 * rs
  sigma <- 4.903e-9
  rnl <- sigma * ((tmax + 273.16)^4 + (tmin + 273.16)^4) / 2 *
    (0.34 - 0.14 * sqrt(ea)) * (1.35 * ratio - 0.35)
  rn <- rns - rnl

  pressure <- 101.3 * ((293 - 0.0065 * z) / 293)^5.26
  gamma <- 0.000665 * pressure
  delta <- 4098 * .saturation_vapor_pressure(tmean) / (tmean + 237.3)^2
  et0 <- (0.408 * delta * rn + gamma * 900 / (tmean + 273) * u2 * (es - ea)) /
    (delta + gamma * (1 + 0.34 * u2))
  x$et0_mm <- pmax(et0, 0)
  x
}

# Extraterrestrial radiation (MJ/m2/day) at latitude `lat` (radians) on day of
# the year `doy` (FAO-56, eq. 21).
.extraterrestrial_radiation <- function(lat, doy) {
  dr <- 1 + 0.033 * cos(2 * pi * doy / 365)
  declination <- 0.409 * sin(2 * pi * doy / 365 - 1.39)
  sunset <- acos(pmin(1, pmax(-1, -tan(lat) * tan(declination))))
  (24 * 60 / pi) * 0.0820 * dr *
    (sunset * sin(lat) * sin(declination) +
       cos(lat) * cos(declination) * sin(sunset))
}

#' Design reference evapotranspiration
#'
#' Computes the reference evapotranspiration that is exceeded only on a given
#' fraction of days, a common design value for irrigation systems.
#'
#' @param x A data frame with `et0_mm`, such as the output of [inmet_et0()].
#' @param percentile Percentile of the daily values, between 0 and 1. The
#'   default 0.8 is the value not exceeded on 80 % of the days.
#' @param by Optional names of columns that define groups, for example
#'   `"station_code"`. The default is a single group.
#'
#' @return A data frame with one row per group, the grouping columns,
#'   `n_days` (days with a value) and `et0_design_mm`.
#'
#' @examples
#' daily <- inmet_et0(inmet_daily(rmet_example))
#' inmet_design_et0(daily, by = "station_code")
#' @seealso [inmet_et0()]
#' @export
inmet_design_et0 <- function(x, percentile = 0.8, by = NULL) {
  if (.is_no_data(x)) return(data.frame())
  if (!is.data.frame(x) || !"et0_mm" %in% names(x)) {
    stop("`x` must have an `et0_mm` column; see inmet_et0().", call. = FALSE)
  }
  percentile <- .validate_number(percentile, "percentile", 0, 1)
  by <- .validate_character_filter(by, "by")
  unknown <- setdiff(by, names(x))
  if (length(unknown)) {
    stop("Unknown grouping column(s): ", paste(unknown, collapse = ", "), ".",
         call. = FALSE)
  }
  key <- if (is.null(by)) rep("all", nrow(x)) else do.call(paste, c(unname(as.list(x[by])), sep = "\r"))
  group <- match(key, unique(key))
  first_row <- match(seq_along(unique(key)), group)
  out <- if (is.null(by)) data.frame(row.names = seq_along(first_row)) else x[first_row, by, drop = FALSE]
  out <- as.data.frame(out)
  values <- split(as.numeric(x$et0_mm), group)
  out$n_days <- vapply(values, function(v) sum(!is.na(v)), integer(1), USE.NAMES = FALSE)
  out$et0_design_mm <- vapply(values, function(v) {
    v <- v[!is.na(v)]
    if (length(v)) unname(stats::quantile(v, percentile, names = FALSE)) else NA_real_
  }, numeric(1), USE.NAMES = FALSE)
  rownames(out) <- NULL
  out
}
