#' Derived hourly variables
#'
#' Adds commonly needed variables computed from the hourly measurements:
#' vapour pressure, vapour pressure deficit, wind components and heat index.
#' Values flagged by [inmet_qc()] are ignored.
#'
#' @param x A data frame as returned by [inmet_read()].
#' @param variables Which variables to add; any of `"vapor_pressure_kpa"`,
#'   `"vpd_kpa"`, `"wind_u_ms"`, `"wind_v_ms"` and `"heat_index_c"`. Variables
#'   that cannot be computed from the columns of `x` are skipped with a
#'   message.
#'
#' @return `x` with the new columns.
#'
#' @section Variables:
#' | Column | Definition | Needs |
#' |---|---|---|
#' | `vapor_pressure_kpa` | Actual vapour pressure: saturation pressure at the dew point, or at the air temperature times relative humidity | `temp_dew_c`, or `temp_dry_c` and `humid_rel_pct` |
#' | `vpd_kpa` | Vapour pressure deficit: saturation pressure at the air temperature minus `vapor_pressure_kpa` (at least 0) | `temp_dry_c`, and the variables above |
#' | `wind_u_ms`, `wind_v_ms` | Eastward and northward wind components, with the meteorological convention that the direction is where the wind comes *from* | `wind_speed_ms`, `wind_dir_deg` |
#' | `heat_index_c` | Apparent temperature from the US National Weather Service formula (Rothfusz regression with the usual adjustments, simple Steadman formula below 26.7 degC) | `temp_dry_c`, `humid_rel_pct` |
#'
#' Saturation vapour pressure uses the FAO-56 formula
#' `0.6108 * exp(17.27 * T / (T + 237.3))`.
#'
#' @examples
#' hourly <- inmet_derive(rmet_example)
#' summary(hourly$vpd_kpa)
#' @seealso [inmet_et0()], [inmet_daily()]
#' @export
inmet_derive <- function(
    x,
    variables = c("vapor_pressure_kpa", "vpd_kpa", "wind_u_ms", "wind_v_ms",
                  "heat_index_c")
) {
  if (.is_no_data(x)) return(x)
  if (!is.data.frame(x)) stop("`x` must be a data frame as returned by inmet_read().", call. = FALSE)
  variables <- match.arg(variables, several.ok = TRUE)

  temp <- .clean_values(x, "temp_dry_c")
  dew <- .clean_values(x, "temp_dew_c")
  humid <- .clean_values(x, "humid_rel_pct")
  speed <- .clean_values(x, "wind_speed_ms")
  direction <- .clean_values(x, "wind_dir_deg")

  actual <- if (!is.null(dew)) {
    .saturation_vapor_pressure(dew)
  } else if (!is.null(temp) && !is.null(humid)) {
    .saturation_vapor_pressure(temp) * humid / 100
  }
  skipped <- function(name, needs) {
    message("`", name, "` skipped: needs ", needs, ".")
  }

  if ("vapor_pressure_kpa" %in% variables) {
    if (is.null(actual)) skipped("vapor_pressure_kpa", "`temp_dew_c`, or `temp_dry_c` and `humid_rel_pct`")
    else x$vapor_pressure_kpa <- actual
  }
  if ("vpd_kpa" %in% variables) {
    if (is.null(actual) || is.null(temp)) {
      skipped("vpd_kpa", "`temp_dry_c` and a humidity variable")
    } else {
      x$vpd_kpa <- pmax(.saturation_vapor_pressure(temp) - actual, 0)
    }
  }
  if (any(c("wind_u_ms", "wind_v_ms") %in% variables)) {
    if (is.null(speed) || is.null(direction)) {
      skipped("wind_u_ms/wind_v_ms", "`wind_speed_ms` and `wind_dir_deg`")
    } else {
      radians <- direction * pi / 180
      if ("wind_u_ms" %in% variables) x$wind_u_ms <- -speed * sin(radians)
      if ("wind_v_ms" %in% variables) x$wind_v_ms <- -speed * cos(radians)
    }
  }
  if ("heat_index_c" %in% variables) {
    if (is.null(temp) || is.null(humid)) {
      skipped("heat_index_c", "`temp_dry_c` and `humid_rel_pct`")
    } else {
      x$heat_index_c <- .heat_index_c(temp, humid)
    }
  }
  x
}

# Numeric column with the values flagged by inmet_qc() set to NA; NULL when
# the column does not exist.
.clean_values <- function(x, name) {
  if (!name %in% names(x)) return(NULL)
  values <- as.numeric(x[[name]])
  flag <- x[[paste0(name, "_flag")]]
  if (!is.null(flag)) values[!is.na(flag)] <- NA_real_
  values
}

# Saturation vapour pressure in kPa at temperature `t` in degC (FAO-56, eq. 11).
.saturation_vapor_pressure <- function(t) 0.6108 * exp(17.27 * t / (t + 237.3))

# NWS heat index (degC) from air temperature (degC) and relative humidity (%).
.heat_index_c <- function(temp_c, rh) {
  t <- temp_c * 9 / 5 + 32
  simple <- 0.5 * (t + 61 + (t - 68) * 1.2 + rh * 0.094)
  full <- -42.379 + 2.04901523 * t + 10.14333127 * rh - 0.22475541 * t * rh -
    0.00683783 * t^2 - 0.05481717 * rh^2 + 0.00122874 * t^2 * rh +
    0.00085282 * t * rh^2 - 0.00000199 * t^2 * rh^2
  dry <- !is.na(rh) & rh < 13 & !is.na(t) & t >= 80 & t <= 112
  full[dry] <- full[dry] - ((13 - rh[dry]) / 4) * sqrt((17 - abs(t[dry] - 95)) / 17)
  humid <- !is.na(rh) & rh > 85 & !is.na(t) & t >= 80 & t <= 87
  full[humid] <- full[humid] + ((rh[humid] - 85) / 10) * ((87 - t[humid]) / 5)
  hot <- !is.na(simple) & simple >= 80
  index <- ifelse(hot, full, simple)
  (index - 32) * 5 / 9
}

