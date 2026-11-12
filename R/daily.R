#' Daily summaries of hourly INMET data
#'
#' Aggregates the hourly output of [inmet_read()] or [inmet_get()] into one row
#' per station and local calendar day, handling the conventions of the source
#' data correctly.
#'
#' @param x A data frame as returned by [inmet_read()], with at least a
#'   `datetime` column.
#' @param tz Time zone defining the calendar day. `"auto"` (default) uses each
#'   station's own local time, from its state and longitude (see
#'   [inmet_timezone()]); this needs the `state` column. Otherwise, any name in
#'   [OlsonNames()], for example `"America/Sao_Paulo"`, or `"UTC"` for UTC
#'   days.
#' @param min_hours Minimum number of non-missing hourly values a variable
#'   needs on a given day; otherwise its daily value is `NA`. Local days have
#'   24 hours (23 or 25 on daylight-saving transitions, which Brazil observed
#'   until 2019).
#'
#' @return A data frame with one row per station and day, sorted by station
#'   and date, containing the station identifiers, `date`, `time_zone`,
#'   `n_hours` (number of hourly records for that day) and the daily variables
#'   that can be computed from the columns present:
#'
#' | Column | Definition |
#' |---|---|
#' | `temp_max_c` | Maximum of `temp_max_prev_c` (or of `temp_dry_c` if absent) |
#' | `temp_min_c` | Minimum of `temp_min_prev_c` (or of `temp_dry_c` if absent) |
#' | `temp_mean_c` | Mean of `temp_dry_c` |
#' | `temp_dew_mean_c` | Mean of `temp_dew_c` |
#' | `humid_rel_mean_pct` | Mean of `humid_rel_pct` |
#' | `humid_rel_min_pct` | Minimum of `humid_rel_min_pct` (or of `humid_rel_pct`) |
#' | `precip_mm` | Sum of `precip_mm` |
#' | `pressure_station_mean_hpa` | Mean of `pressure_station_hpa` |
#' | `wind_speed_mean_ms` | Mean of `wind_speed_ms` |
#' | `wind_gust_max_ms` | Maximum of `wind_gust_ms` |
#' | `radiation_mj_m2` | Sum of positive `radiation_kjm2`, in MJ/m^2 |
#'
#' @details
#' INMET stamps every record with the end of the hour it describes, in UTC.
#' A record is therefore assigned to the local day containing the *middle* of
#' its hour (`datetime` minus 30 minutes): the value stamped 03:00 UTC, for
#' example, describes 23:00 to 00:00 Brasilia time and belongs to the earlier
#' day. Grouping with `as.Date(datetime)` instead would use UTC days, which in
#' Brazil end at 21:00 or 22:00 local time.
#'
#' Radiation is usually missing at night, so `radiation_mj_m2` does not use
#' `min_hours`. It is the sum of the available values on days with at least
#' `min_hours` hourly records, and `NA` otherwise; daytime gaps make it an
#' underestimate.
#'
#' The function does not itself check data quality, but values flagged by
#' [inmet_qc()] (non-missing `<variable>_flag` columns) are left out of the
#' daily statistics.
#'
#' @examples
#' daily <- inmet_daily(rmet_example)
#' head(daily[, c("date", "n_hours", "temp_max_c", "temp_min_c", "precip_mm")])
#'
#' # after quality control, flagged hourly values are ignored
#' daily_qc <- inmet_daily(inmet_qc(rmet_example))
#' @export
inmet_daily <- function(x, tz = "auto", min_hours = 20L) {
  if (.is_no_data(x)) return(data.frame())
  if (!is.data.frame(x) || !"datetime" %in% names(x) || !inherits(x$datetime, "POSIXct")) {
    stop("`x` must be a data frame with a POSIXct `datetime` column, as returned by ",
         "inmet_read().", call. = FALSE)
  }
  min_hours <- .validate_positive_integer(min_hours, "min_hours")
  zone <- .row_time_zones(x, tz)
  if (!nrow(x)) return(data.frame())

  station <- if ("station_code" %in% names(x)) x$station_code else rep(NA_character_, nrow(x))
  shifted <- x$datetime - 1800
  day <- structure(rep(NA_real_, nrow(x)), class = "Date")
  for (z in unique(zone)) {
    rows <- zone == z
    day[rows] <- as.Date(shifted[rows], tz = z)
  }
  key <- paste(station, unclass(day))
  group <- match(key, unique(key))
  n_groups <- max(group)
  first_row <- match(seq_len(n_groups), group)

  id_cols <- intersect(c("station_code", "station_name", "region", "state",
                         "latitude", "longitude", "elevation"), names(x))
  out <- lapply(id_cols, function(col) x[[col]][first_row])
  names(out) <- id_cols
  out$date <- day[first_row]
  out$time_zone <- zone[first_row]
  n_hours <- tabulate(group, nbins = n_groups)
  out$n_hours <- n_hours

  # first available column, with values flagged by inmet_qc() set to NA
  pick <- function(...) {
    found <- intersect(c(...), names(x))
    if (!length(found)) return(NULL)
    values <- as.numeric(x[[found[[1]]]])
    flag <- x[[paste0(found[[1]], "_flag")]]
    if (!is.null(flag)) values[!is.na(flag)] <- NA_real_
    values
  }
  rules <- list(
    temp_max_c = list(pick("temp_max_prev_c", "temp_dry_c"), "max"),
    temp_min_c = list(pick("temp_min_prev_c", "temp_dry_c"), "min"),
    temp_mean_c = list(pick("temp_dry_c"), "mean"),
    temp_dew_mean_c = list(pick("temp_dew_c"), "mean"),
    humid_rel_mean_pct = list(pick("humid_rel_pct"), "mean"),
    humid_rel_min_pct = list(pick("humid_rel_min_pct", "humid_rel_pct"), "min"),
    precip_mm = list(pick("precip_mm"), "sum"),
    pressure_station_mean_hpa = list(pick("pressure_station_hpa"), "mean"),
    wind_speed_mean_ms = list(pick("wind_speed_ms"), "mean"),
    wind_gust_max_ms = list(pick("wind_gust_ms"), "max")
  )
  for (name in names(rules)) {
    values <- rules[[name]][[1]]
    if (is.null(values)) next
    stat <- .group_stat(values, group, n_groups, rules[[name]][[2]])
    stat$value[stat$count < min_hours] <- NA_real_
    out[[name]] <- stat$value
  }
  if ("radiation_kjm2" %in% names(x)) {
    radiation <- pmax(pick("radiation_kjm2"), 0)
    stat <- .group_stat(radiation, group, n_groups, "sum")
    value <- stat$value / 1000
    value[stat$count == 0L | n_hours < min_hours] <- NA_real_
    out$radiation_mj_m2 <- value
  }

  out <- .new_data_frame(out, n_groups)
  ord <- order(out$station_code %||% rep(NA, n_groups), out$date)
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  .as_rmet(out, "rmet_daily")
}

# Time zone of every row: one fixed zone, or each station's own zone when
# tz = "auto".
.row_time_zones <- function(x, tz) {
  if (!identical(tz, "auto")) return(rep(.validate_timezone(tz), nrow(x)))
  if (!"state" %in% names(x)) {
    stop("`tz = \"auto\"` needs a `state` column; set `tz` explicitly.", call. = FALSE)
  }
  longitude <- if ("longitude" %in% names(x)) x$longitude else NA_real_
  zone <- inmet_timezone(x$state, longitude)
  if (anyNA(zone)) {
    stop("Unknown state(s): ", paste(unique(x$state[is.na(zone)]), collapse = ", "),
         ". Set `tz` explicitly.", call. = FALSE)
  }
  zone
}

# Grouped statistic over non-missing values. Returns the statistic (NA for
# groups without data) and the number of values used in each group.
.group_stat <- function(values, group, n_groups, fun) {
  ok <- !is.na(values)
  count <- tabulate(group[ok], nbins = n_groups)
  value <- rep(NA_real_, n_groups)
  if (!any(ok)) return(list(value = value, count = count))
  v <- values[ok]
  g <- group[ok]
  if (fun %in% c("sum", "mean")) {
    sums <- rowsum(v, g, reorder = TRUE)
    value[as.integer(rownames(sums))] <- sums[, 1]
    if (fun == "mean") value <- value / count
  } else {
    o <- if (fun == "max") order(g, -v) else order(g, v)
    first <- !duplicated(g[o])
    value[g[o][first]] <- v[o][first]
  }
  value[count == 0L] <- NA_real_
  list(value = value, count = count)
}
