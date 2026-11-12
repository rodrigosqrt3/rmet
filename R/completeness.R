#' Data completeness by station and period
#'
#' Summarises how complete hourly data are, to choose stations and periods
#' before an analysis.
#'
#' @param x A data frame as returned by [inmet_read()].
#' @param by Period length: `"month"` (default) or `"year"`. Periods are UTC
#'   calendar months or years.
#' @param variables Measurement columns to assess. Defaults to all numeric
#'   measurement columns.
#'
#' @return A data frame with one row per station and period: `station_code`,
#'   `period` (`"YYYY-MM"` or `"YYYY"`), `hours_expected` (hours in the
#'   period), `hours_present` (hourly records in `x`), and, for each variable,
#'   the percentage of the expected hours with a valid value. Values flagged by
#'   [inmet_qc()] do not count as valid.
#'
#' @details
#' The expected number of hours is the full length of the period, so periods
#' only partly covered by `x` (for example, the current month, or a period cut
#' by `start_date` in [inmet_read()]) show low completeness. Global radiation
#' is missing at night at most stations, so its completeness is usually around
#' 50 %.
#'
#' @examples
#' inmet_completeness(rmet_example, variables = c("temp_dry_c", "precip_mm"))
#' @seealso [inmet_qc()], [inmet_daily()]
#' @export
inmet_completeness <- function(x, by = c("month", "year"), variables = NULL) {
  if (.is_no_data(x)) return(data.frame())
  if (!is.data.frame(x) || !"datetime" %in% names(x) || !inherits(x$datetime, "POSIXct")) {
    stop("`x` must be a data frame with a POSIXct `datetime` column, as returned by ",
         "inmet_read().", call. = FALSE)
  }
  by <- match.arg(by)
  measurement <- setdiff(names(x), c(.inmet_id_columns(), grep("_flag$", names(x), value = TRUE)))
  measurement <- measurement[vapply(x[measurement], is.numeric, logical(1))]
  variables <- .validate_character_filter(variables, "variables")
  if (is.null(variables)) {
    variables <- measurement
  } else if (length(unknown <- setdiff(variables, measurement))) {
    stop("Unknown variables: ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  if (!nrow(x)) return(data.frame())

  station <- if ("station_code" %in% names(x)) x$station_code else rep(NA_character_, nrow(x))
  format_string <- if (by == "month") "%Y-%m" else "%Y"
  period <- format(x$datetime, format_string, tz = "UTC")
  key <- paste(station, period)
  group <- match(key, unique(key))
  n_groups <- max(group)
  first_row <- match(seq_len(n_groups), group)

  out <- list(
    station_code = station[first_row],
    period = period[first_row],
    hours_expected = .hours_in_period(period[first_row], by),
    hours_present = tabulate(group, nbins = n_groups)
  )
  for (v in variables) {
    valid <- !is.na(x[[v]])
    flag <- x[[paste0(v, "_flag")]]
    if (!is.null(flag)) valid <- valid & is.na(flag)
    out[[v]] <- round(100 * tabulate(group[valid], nbins = n_groups) / out$hours_expected, 1)
  }
  out <- .new_data_frame(out, n_groups)
  out <- out[order(out$station_code, out$period), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.hours_in_period <- function(period, by) {
  start <- if (by == "month") paste0(period, "-01") else paste0(period, "-01-01")
  start <- as.Date(start)
  end <- if (by == "month") {
    as.Date(vapply(start, function(s) format(seq(s, by = "month", length.out = 2L)[2L]),
                   character(1)))
  } else {
    as.Date(paste0(as.integer(period) + 1L, "-01-01"))
  }
  as.integer(end - start) * 24L
}
