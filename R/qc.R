#' Quality control of hourly INMET data
#'
#' Flags implausible hourly values with simple, transparent checks. INMET
#' publishes automatic-station data without consistency validation; these
#' checks catch the most common errors (impossible values, sensor spikes,
#' stuck sensors and internally inconsistent records) but are no substitute
#' for checks tailored to an analysis.
#'
#' @param x A data frame as returned by [inmet_read()].
#' @param action `"flag"` (default) adds a `<variable>_flag` column for each
#'   checked variable, holding the names of the failed checks (separated by
#'   commas) or `NA`. `"remove"` sets flagged values to `NA` instead.
#' @param checks Checks to run; any of `"range"`, `"consistency"`, `"spike"`,
#'   and `"persistence"` (see *Checks*).
#' @param persistence_hours Minimum number of consecutive hours with an
#'   identical value for the persistence check.
#' @param quiet Suppress the summary message.
#'
#' @return `x` with flag columns added (or flagged values removed). The
#'   attribute `qc_summary` is a data frame with the number and percentage of
#'   flagged values per variable and check.
#'
#' @section Checks:
#' * **range**: outside physical limits for Brazil, e.g. air temperature
#'   outside -15 to 48 degC, relative humidity outside 1 to 100 %, hourly
#'   precipitation outside 0 to 150 mm, station pressure outside 650 to
#'   1060 hPa, or wind direction outside 0 to 360 degrees.
#' * **consistency**: hourly maximum below minimum (temperature, humidity,
#'   pressure; both flagged); air temperature outside the extremes of the hour
#'   it ends; dew point above air temperature; gust below mean wind speed. A
#'   tolerance of 0.5 units allows for rounding.
#' * **spike**: a value that departs from *both* adjacent hours in the same
#'   direction by more than 8 degC (air and dew-point temperature), 5 hPa
#'   (pressure) or 45 % (relative humidity). Changes that persist, such as a
#'   cold front, are not flagged.
#' * **persistence**: the same value in at least `persistence_hours`
#'   consecutive hours, for temperatures, humidity, pressure, wind and
#'   precipitation. Zero wind speed, zero precipitation, humidity of 90 % or
#'   more (fog, persistent rain, sensors that saturate below 100 %), and wind
#'   direction during calm hours (speed below 0.5 m/s) are not checked.
#'
#' Neighbouring hours are only compared within a station and when exactly one
#' hour apart.
#'
#' @examples
#' checked <- inmet_qc(rmet_example)
#' attr(checked, "qc_summary")
#' table(checked$temp_dry_c_flag, useNA = "ifany")
#'
#' # daily values ignore flagged hours
#' daily <- inmet_daily(checked)
#' @seealso [inmet_daily()], [inmet_completeness()]
#' @export
inmet_qc <- function(
    x,
    action = c("flag", "remove"),
    checks = c("range", "consistency", "spike", "persistence"),
    persistence_hours = 12L,
    quiet = FALSE
) {
  if (!is.data.frame(x) || !"datetime" %in% names(x) || !inherits(x$datetime, "POSIXct")) {
    stop("`x` must be a data frame with a POSIXct `datetime` column, as returned by ",
         "inmet_read().", call. = FALSE)
  }
  action <- match.arg(action)
  checks <- match.arg(checks, several.ok = TRUE)
  persistence_hours <- .validate_positive_integer(persistence_hours, "persistence_hours")
  quiet <- .validate_flag(quiet, "quiet")

  variables <- intersect(names(.qc_limits()), names(x))
  n <- nrow(x)
  flags <- lapply(stats::setNames(variables, variables), function(v) {
    matrix(FALSE, n, 4L, dimnames = list(NULL, c("range", "consistency", "spike", "persistence")))
  })
  value <- function(v) as.numeric(x[[v]])
  value_or_false <- function(v, rule) if (v %in% variables) rule(value(v)) else FALSE
  add_flag <- function(v, check, rows) {
    if (v %in% variables) flags[[v]][, check] <<- flags[[v]][, check] | (!is.na(rows) & rows)
  }

  if ("range" %in% checks) {
    for (v in variables) {
      limits <- .qc_limits()[[v]]
      add_flag(v, "range", value(v) < limits[[1]] | value(v) > limits[[2]])
    }
  }

  if ("consistency" %in% checks) {
    tol <- 0.5
    has <- function(...) all(c(...) %in% variables)
    for (pair in list(c("temp_max_prev_c", "temp_min_prev_c"),
                      c("humid_rel_max_pct", "humid_rel_min_pct"),
                      c("pressure_max_hpa", "pressure_min_hpa"))) {
      if (has(pair)) {
        bad <- value(pair[[1]]) < value(pair[[2]]) - tol
        add_flag(pair[[1]], "consistency", bad)
        add_flag(pair[[2]], "consistency", bad)
      }
    }
    if (has("temp_dry_c", "temp_max_prev_c")) {
      add_flag("temp_dry_c", "consistency", value("temp_dry_c") > value("temp_max_prev_c") + tol)
    }
    if (has("temp_dry_c", "temp_min_prev_c")) {
      add_flag("temp_dry_c", "consistency", value("temp_dry_c") < value("temp_min_prev_c") - tol)
    }
    if (has("temp_dew_c", "temp_dry_c")) {
      add_flag("temp_dew_c", "consistency", value("temp_dew_c") > value("temp_dry_c") + tol)
    }
    if (has("wind_gust_ms", "wind_speed_ms")) {
      add_flag("wind_gust_ms", "consistency", value("wind_gust_ms") < value("wind_speed_ms") - tol)
    }
  }

  if (any(c("spike", "persistence") %in% checks)) {
    station <- if ("station_code" %in% names(x)) x$station_code else rep("", n)
    station_id <- match(station, unique(station))
    time <- as.numeric(x$datetime)
    ord <- order(station_id, time, method = "radix")
    # TRUE where row ord[i] directly follows row ord[i - 1] (same station, 1 h later)
    follows <- c(FALSE, diff(station_id[ord]) == 0 & diff(time[ord]) == 3600)
    follows[is.na(follows)] <- FALSE

    if ("spike" %in% checks) {
      thresholds <- c(temp_dry_c = 8, temp_dew_c = 8, pressure_station_hpa = 5,
                      humid_rel_pct = 45)
      for (v in intersect(names(thresholds), variables)) {
        add_flag(v, "spike", .qc_spike(value(v)[ord], follows, thresholds[[v]])[order(ord)])
      }
    }
    if ("persistence" %in% checks) {
      calm <- if ("wind_speed_ms" %in% variables) value("wind_speed_ms") < 0.5 else FALSE
      # values for which a long run is physically plausible are not checked
      exempt <- list(
        temp_dry_c = FALSE, temp_dew_c = FALSE, pressure_station_hpa = FALSE,
        humid_rel_pct = value_or_false("humid_rel_pct", function(v) v >= 90),
        wind_speed_ms = value_or_false("wind_speed_ms", function(v) v == 0),
        wind_dir_deg = rep_len(calm, n),
        precip_mm = value_or_false("precip_mm", function(v) v == 0)
      )
      for (v in intersect(names(exempt), variables)) {
        stuck <- .qc_persistence(value(v)[ord], follows, persistence_hours,
                                 rep_len(exempt[[v]], n)[ord])
        add_flag(v, "persistence", stuck[order(ord)])
      }
    }
  }

  summary <- do.call(rbind, lapply(variables, function(v) {
    checked <- sum(!is.na(x[[v]]))
    data.frame(variable = v, check = colnames(flags[[v]]),
               flagged = colSums(flags[[v]]), checked = checked,
               percent = if (checked) round(100 * colSums(flags[[v]]) / checked, 3) else NA_real_,
               stringsAsFactors = FALSE, row.names = NULL)
  }))
  if (!is.null(summary)) {
    summary <- summary[summary$check %in% checks, , drop = FALSE]
    rownames(summary) <- NULL
  }

  any_flag <- vapply(flags, function(f) rowSums(f) > 0, logical(n))
  any_flag <- matrix(any_flag, nrow = n)
  for (j in seq_along(variables)) {
    v <- variables[[j]]
    hit <- any_flag[, j]
    if (action == "remove") {
      x[[v]][hit] <- NA
    } else {
      labels <- rep(NA_character_, n)
      if (any(hit)) {
        f <- flags[[v]][hit, , drop = FALSE]
        labels[hit] <- apply(f, 1L, function(r) paste(colnames(f)[r], collapse = ","))
      }
      x[[paste0(v, "_flag")]] <- labels
    }
  }

  if (!quiet) {
    total <- sum(any_flag)
    message(sprintf("Flagged %d value(s) in %d variable(s)%s. See attr(x, \"qc_summary\").",
                    total, sum(colSums(any_flag) > 0),
                    if (action == "remove") " and set them to NA" else ""))
  }
  attr(x, "qc_summary") <- summary
  x
}

# Physical limits per variable (lower, upper).
.qc_limits <- function() {
  temp <- c(-15, 48)
  dew <- c(-40, 35)
  humid <- c(1, 100)
  pressure <- c(650, 1060)
  list(
    temp_dry_c = temp, temp_max_prev_c = temp, temp_min_prev_c = temp,
    temp_dew_c = dew, temp_dew_max_c = dew, temp_dew_min_c = dew,
    humid_rel_pct = humid, humid_rel_max_pct = humid, humid_rel_min_pct = humid,
    precip_mm = c(0, 150),
    pressure_station_hpa = pressure, pressure_max_hpa = pressure, pressure_min_hpa = pressure,
    wind_speed_ms = c(0, 50), wind_gust_ms = c(0, 80), wind_dir_deg = c(0, 360),
    radiation_kjm2 = c(-50, 5000)
  )
}

# Values (in station-time order) that depart from both adjacent hours in the
# same direction by more than `threshold`.
.qc_spike <- function(v, follows, threshold) {
  n <- length(v)
  prev <- c(NA, v[-n])
  nxt <- c(v[-1], NA)
  has_prev <- follows
  has_next <- c(follows[-1], FALSE)
  up <- v - prev
  down <- v - nxt
  out <- has_prev & has_next & abs(up) > threshold & abs(down) > threshold & sign(up) == sign(down)
  out[is.na(out)] <- FALSE
  out
}

# Runs of at least `min_hours` consecutive identical values (in station-time
# order); values where `exempt` is TRUE are never flagged.
.qc_persistence <- function(v, follows, min_hours, exempt = FALSE) {
  n <- length(v)
  same <- follows & c(FALSE, v[-1] == v[-n])
  same[is.na(same)] <- FALSE
  run <- cumsum(!same)
  run_length <- tabulate(run)[run]
  exempt[is.na(exempt)] <- FALSE
  out <- !is.na(v) & run_length >= min_hours & !exempt
  out[is.na(out)] <- FALSE
  out
}
