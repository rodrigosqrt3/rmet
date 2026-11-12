#' Monthly summaries of daily INMET data
#'
#' Aggregates the output of [inmet_daily()] into one row per station and
#' calendar month, with an explicit rule for incomplete months.
#'
#' @param x A data frame as returned by [inmet_daily()] (with a `date`
#'   column), optionally with `et0_mm` from [inmet_et0()].
#' @param min_days Minimum number of days with a valid value that a variable
#'   needs in a month; otherwise its monthly value is `NA`. Missing daily
#'   values (for example, days left out by the `min_hours` rule of
#'   [inmet_daily()]) do not count.
#'
#' @return A data frame with one row per station and month, sorted by station
#'   and month, with the station identifiers, `month` (the first day of the
#'   month, as a `Date`), `n_days` (days present in `x`) and the monthly
#'   variables that can be computed from the columns present:
#'
#' | Column | Definition |
#' |---|---|
#' | `temp_max_mean_c`, `temp_min_mean_c` | Mean of the daily maximum and minimum temperature |
#' | `temp_mean_c` | Mean of the daily mean temperature |
#' | `temp_max_abs_c`, `temp_min_abs_c` | Highest daily maximum and lowest daily minimum |
#' | `humid_rel_mean_pct` | Mean of the daily mean relative humidity |
#' | `precip_mm` | Sum of the daily precipitation |
#' | `rain_days` | Number of days with at least 1 mm of precipitation |
#' | `wind_speed_mean_ms` | Mean of the daily mean wind speed |
#' | `wind_gust_max_ms` | Highest daily gust |
#' | `radiation_mean_mj_m2` | Mean daily global radiation |
#' | `et0_mm` | Sum of the daily reference evapotranspiration |
#'
#' @details
#' Months are the calendar months of the `date` column, which [inmet_daily()]
#' builds in each station's local time. Sums (`precip_mm`, `rain_days`,
#' `et0_mm`) are not scaled up for missing days, so they are only comparable
#' between months when `min_days` is close to the month length. Use
#' [inmet_completeness()] to see how complete the hourly data are.
#'
#' @examples
#' daily <- inmet_daily(rmet_example)
#' monthly <- inmet_monthly(daily)
#' monthly[, c("month", "n_days", "temp_mean_c", "precip_mm", "rain_days")]
#' @seealso [inmet_daily()], [inmet_et0()]
#' @export
inmet_monthly <- function(x, min_days = 20L) {
  if (.is_no_data(x)) return(data.frame())
  if (!is.data.frame(x) || !"date" %in% names(x) || !inherits(x$date, "Date")) {
    stop("`x` must be a data frame with a `date` column, as returned by ",
         "inmet_daily(). Use inmet_daily() on hourly data first.", call. = FALSE)
  }
  min_days <- .validate_positive_integer(min_days, "min_days")
  if (!nrow(x)) return(data.frame())

  station <- if ("station_code" %in% names(x)) x$station_code else rep(NA_character_, nrow(x))
  month <- as.Date(format(x$date, "%Y-%m-01"))
  key <- paste(station, unclass(month))
  group <- match(key, unique(key))
  n_groups <- max(group)
  first_row <- match(seq_len(n_groups), group)

  id_cols <- intersect(c("station_code", "station_name", "region", "state",
                         "latitude", "longitude", "elevation"), names(x))
  out <- lapply(id_cols, function(col) x[[col]][first_row])
  names(out) <- id_cols
  out$month <- month[first_row]
  out$n_days <- tabulate(group, nbins = n_groups)

  column <- function(name) if (name %in% names(x)) as.numeric(x[[name]]) else NULL
  rain <- column("precip_mm")
  rules <- list(
    temp_max_mean_c = list(column("temp_max_c"), "mean"),
    temp_min_mean_c = list(column("temp_min_c"), "mean"),
    temp_mean_c = list(column("temp_mean_c"), "mean"),
    temp_max_abs_c = list(column("temp_max_c"), "max"),
    temp_min_abs_c = list(column("temp_min_c"), "min"),
    humid_rel_mean_pct = list(column("humid_rel_mean_pct"), "mean"),
    precip_mm = list(rain, "sum"),
    rain_days = list(if (!is.null(rain)) as.numeric(rain >= 1), "sum"),
    wind_speed_mean_ms = list(column("wind_speed_mean_ms"), "mean"),
    wind_gust_max_ms = list(column("wind_gust_max_ms"), "max"),
    radiation_mean_mj_m2 = list(column("radiation_mj_m2"), "mean"),
    et0_mm = list(column("et0_mm"), "sum")
  )
  for (name in names(rules)) {
    values <- rules[[name]][[1]]
    if (is.null(values)) next
    stat <- .group_stat(values, group, n_groups, rules[[name]][[2]])
    stat$value[stat$count < min_days] <- NA_real_
    out[[name]] <- stat$value
  }

  out <- .new_data_frame(out, n_groups)
  out <- out[order(out$station_code %||% rep(NA, n_groups), out$month), , drop = FALSE]
  rownames(out) <- NULL
  .as_rmet(out, "rmet_monthly")
}
