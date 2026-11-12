#' Climate indices from daily data
#'
#' Computes yearly or monthly climate indices from the output of
#' [inmet_daily()], in the spirit of the indices of the Expert Team on Climate
#' Change Detection and Indices (ETCCDI), plus growing degree days.
#'
#' @param x A data frame as returned by [inmet_daily()], with `date` and at
#'   least `precip_mm` or `temp_max_c` and `temp_min_c`.
#' @param by Period of each index: `"year"` (default) or `"month"`, calendar
#'   years or months in local time.
#' @param min_valid Minimum fraction of the days of the period that must have
#'   a value for an index to be computed; otherwise it is `NA`. Days absent
#'   from `x` count as missing.
#' @param base_temp Base temperature (degC) of the growing degree days.
#'
#' @return A data frame with one row per station and period: the station
#'   identifiers, `period` (first day of the period), `n_days` (rows present)
#'   and the indices that can be computed:
#'
#' | Column | Definition |
#' |---|---|
#' | `txx`, `tnn` | Highest daily maximum and lowest daily minimum temperature (degC) |
#' | `dtr` | Mean daily temperature range (degC) |
#' | `summer_days` | Days with maximum temperature above 25 degC |
#' | `tropical_nights` | Days with minimum temperature above 20 degC |
#' | `frost_days` | Days with minimum temperature below 0 degC |
#' | `gdd` | Growing degree days: sum of mean temperature above `base_temp` (degC day) |
#' | `prcptot` | Total precipitation on wet days (>= 1 mm) |
#' | `rain_days`, `r10mm`, `r20mm` | Days with at least 1, 10 and 20 mm |
#' | `sdii` | Mean precipitation on wet days (mm/day) |
#' | `rx1day`, `rx5day` | Highest 1-day and 5-day precipitation |
#' | `cdd`, `cwd` | Longest run of dry (< 1 mm) and wet (>= 1 mm) days |
#'
#' @details
#' Runs (`cdd`, `cwd`) are broken by missing days. `rx5day` uses five
#' consecutive calendar days without missing values.
#'
#' @examples
#' inmet_indices(inmet_daily(rmet_example))
#' @seealso [inmet_daily()], [inmet_spi()]
#' @export
inmet_indices <- function(x, by = c("year", "month"), min_valid = 0.8,
                          base_temp = 10) {
  if (.is_no_data(x)) return(data.frame())
  by <- match.arg(by)
  min_valid <- .validate_number(min_valid, "min_valid", 0, 1)
  base_temp <- .validate_number(base_temp, "base_temp", -Inf, Inf)
  if (!is.data.frame(x) || !inherits(x[["date"]], "Date")) {
    stop("`x` must be a data frame with a `date` column, as returned by ",
         "inmet_daily().", call. = FALSE)
  }
  has_temp <- all(c("temp_max_c", "temp_min_c") %in% names(x))
  has_rain <- "precip_mm" %in% names(x)
  if (!has_temp && !has_rain) {
    stop("`x` needs `precip_mm`, or `temp_max_c` and `temp_min_c`.", call. = FALSE)
  }

  station <- if ("station_code" %in% names(x)) x$station_code else rep("", nrow(x))
  start <- as.Date(format(x$date, if (by == "year") "%Y-01-01" else "%Y-%m-01"))
  key <- paste(station, unclass(start))
  group <- match(key, unique(key))
  first_row <- match(seq_along(unique(key)), group)

  rows <- lapply(split(seq_len(nrow(x)), group), function(i) {
    first <- start[i[[1]]]
    next_start <- seq(first, by = if (by == "year") "year" else "month", length.out = 2L)[[2]]
    days <- seq(first, next_start - 1L, by = "day")
    place <- match(days, x$date[i])
    pick <- function(column) as.numeric(x[[column]][i])[place]
    out <- list(n_days = length(i))
    if (has_temp) out <- c(out, .temperature_indices(pick("temp_max_c"), pick("temp_min_c"), min_valid, base_temp))
    if (has_rain) out <- c(out, .precipitation_indices(pick("precip_mm"), min_valid))
    out
  })

  id_cols <- intersect(c("station_code", "station_name", "region", "state",
                         "latitude", "longitude", "elevation"), names(x))
  out <- lapply(id_cols, function(col) x[[col]][first_row])
  names(out) <- id_cols
  out$period <- start[first_row]
  indices <- names(rows[[1]])
  for (name in indices) out[[name]] <- vapply(rows, function(r) r[[name]], numeric(1), USE.NAMES = FALSE)
  out$n_days <- as.integer(out$n_days)
  out <- .new_data_frame(out, length(first_row))
  out <- out[order(out$station_code %||% rep(NA, nrow(out)), out$period), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# NA unless enough of the days of the period have values.
.enough_valid <- function(values, min_valid) {
  any(!is.na(values)) && mean(!is.na(values)) >= min_valid
}

.temperature_indices <- function(tmax, tmin, min_valid, base_temp) {
  ok <- .enough_valid(tmax, min_valid) && .enough_valid(tmin, min_valid)
  index <- function(value) if (ok) value else NA_real_
  tmean <- (tmax + tmin) / 2
  list(
    txx = index(max(tmax, na.rm = TRUE)),
    tnn = index(min(tmin, na.rm = TRUE)),
    dtr = index(mean(tmax - tmin, na.rm = TRUE)),
    summer_days = index(sum(tmax > 25, na.rm = TRUE)),
    tropical_nights = index(sum(tmin > 20, na.rm = TRUE)),
    frost_days = index(sum(tmin < 0, na.rm = TRUE)),
    gdd = index(sum(pmax(tmean - base_temp, 0), na.rm = TRUE))
  )
}

.precipitation_indices <- function(rain, min_valid) {
  ok <- .enough_valid(rain, min_valid)
  index <- function(value) if (ok) value else NA_real_
  wet <- rain >= 1
  five_day <- as.numeric(stats::filter(rain, rep(1, 5), sides = 1))
  list(
    prcptot = index(sum(rain[wet], na.rm = TRUE)),
    rain_days = index(sum(wet, na.rm = TRUE)),
    r10mm = index(sum(rain >= 10, na.rm = TRUE)),
    r20mm = index(sum(rain >= 20, na.rm = TRUE)),
    sdii = index(if (any(wet, na.rm = TRUE)) mean(rain[wet], na.rm = TRUE) else 0),
    rx1day = index(max(rain, na.rm = TRUE)),
    rx5day = index(if (all(is.na(five_day))) NA_real_ else max(five_day, na.rm = TRUE)),
    cdd = index(.longest_run(!wet)),
    cwd = index(.longest_run(wet))
  )
}

# Longest run of TRUE; missing values break runs.
.longest_run <- function(flag) {
  flag[is.na(flag)] <- FALSE
  runs <- rle(flag)
  if (any(runs$values)) max(runs$lengths[runs$values]) else 0
}

#' Standardized Precipitation Index
#'
#' Computes the Standardized Precipitation Index (SPI) of McKee et al. (1993),
#' a measure of how wet or dry an accumulation of months is compared with the
#' station's own history, from the output of [inmet_monthly()].
#'
#' @param x A data frame as returned by [inmet_monthly()], with `month` and
#'   `precip_mm`.
#' @param scale Number of months accumulated (1, 3, 6, 12, ...).
#' @param min_years Minimum number of accumulations of the same calendar month
#'   that a station needs; otherwise its SPI is `NA`. WMO recommends 30 years.
#'
#' @return `x` with the new column `spi` (and the attribute `spi_scale`).
#'   Values near 0 are normal, below -1 are dry and above 1 are wet.
#'
#' @details
#' For each station and calendar month, a gamma distribution is fitted to the
#' accumulated precipitation by maximum likelihood (Thom's approximation), with
#' the probability of zero accumulation treated separately, and the cumulative
#' probability is transformed to a standard normal variable. Accumulations
#' with any missing month are `NA`; months absent from `x` count as missing.
#'
#' @examples
#' monthly <- inmet_monthly(inmet_daily(rmet_example))
#' # one year is too short for a meaningful SPI: use min_years = 1 only to see the output
#' inmet_spi(monthly, scale = 3, min_years = 1)[, c("month", "precip_mm", "spi")]
#' @seealso [inmet_monthly()], [inmet_indices()]
#' @export
inmet_spi <- function(x, scale = 3L, min_years = 20L) {
  if (.is_no_data(x)) return(x)
  scale <- .validate_positive_integer(scale, "scale")
  min_years <- .validate_positive_integer(min_years, "min_years")
  if (!is.data.frame(x) || !inherits(x[["month"]], "Date") || !"precip_mm" %in% names(x)) {
    stop("`x` must be the output of inmet_monthly(), with `month` and `precip_mm`.",
         call. = FALSE)
  }
  station <- if ("station_code" %in% names(x)) x$station_code else rep("", nrow(x))
  spi <- rep(NA_real_, nrow(x))

  for (rows in split(seq_len(nrow(x)), match(station, unique(station)))) {
    months <- x$month[rows]
    timeline <- seq(min(months), max(months), by = "month")
    rain <- rep(NA_real_, length(timeline))
    place <- match(months, timeline)
    rain[place] <- as.numeric(x$precip_mm[rows])
    accumulated <- as.numeric(stats::filter(rain, rep(1, scale), sides = 1))
    calendar <- as.integer(format(timeline, "%m"))
    index <- rep(NA_real_, length(timeline))
    for (m in 1:12) {
      here <- which(calendar == m & !is.na(accumulated))
      if (length(here) >= min_years) index[here] <- .spi_gamma(accumulated[here])
    }
    spi[rows] <- index[place]
  }
  x$spi <- spi
  attr(x, "spi_scale") <- scale
  x
}

# SPI of the values of one calendar month: gamma fit with a mass at zero.
.spi_gamma <- function(values) {
  zero <- values == 0
  positive <- values[!zero]
  q <- mean(zero)
  if (length(positive) < 2L) return(rep(NA_real_, length(values)))
  a <- log(mean(positive)) - mean(log(positive))
  if (a <= 1e-8) return(rep(NA_real_, length(values)))
  shape <- (1 + sqrt(1 + 4 * a / 3)) / (4 * a)
  rate <- shape / mean(positive)
  probability <- q + (1 - q) * stats::pgamma(values, shape = shape, rate = rate)
  eps <- 1e-6
  stats::qnorm(pmin(pmax(probability, eps), 1 - eps))
}
