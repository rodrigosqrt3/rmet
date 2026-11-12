#' Classes of rmet data
#'
#' The data frames returned by [inmet_read()], [inmet_get()], [inmet_daily()]
#' and [inmet_monthly()] have the classes `rmet_hourly`, `rmet_daily` and
#' `rmet_monthly`, which all inherit from `rmet_data` and `data.frame`. They
#' behave as ordinary data frames in every respect (indexing, `merge()`,
#' `dplyr`, `as.data.frame()`), and add a compact `print()` and a `summary()`.
#'
#' @param x,object An object returned by one of the functions above.
#' @param n Number of rows to print.
#' @param row.names,optional Passed on to [as.data.frame()].
#' @param ... Further arguments, ignored.
#'
#' @return `print()` returns `x` invisibly. `summary()` returns a data frame
#'   with one row per station (class `summary.rmet_data`): number of rows,
#'   first and last time or date, and the percentage of non-missing values in
#'   the measurement columns. `as.data.frame()` removes the rmet classes.
#'
#' @examples
#' class(rmet_example)
#' rmet_example
#' summary(rmet_example)
#' @name rmet-classes
NULL

# Adds the rmet classes to a data frame; plain data frames stay plain when
# they are empty.
.as_rmet <- function(x, subclass) {
  class(x) <- c(subclass, "rmet_data", "data.frame")
  x
}

# Name of the column holding time or date, and the label used in headers.
.rmet_time_column <- function(x) {
  for (name in c("datetime", "date", "month")) if (name %in% names(x)) return(name)
  NULL
}

.rmet_measurement_columns <- function(x) {
  skip <- c(.inmet_id_columns(), "date", "month", "time_zone", "n_hours", "n_days")
  columns <- setdiff(names(x), skip)
  columns <- columns[!grepl("_flag$", columns)]
  columns[vapply(unclass(x)[columns], is.numeric, logical(1))]
}

#' @rdname rmet-classes
#' @export
print.rmet_data <- function(x, ..., n = 10L) {
  kind <- sub("^rmet_", "", class(x)[[1]])
  time_column <- .rmet_time_column(x)
  stations <- if ("station_code" %in% names(x)) length(unique(x$station_code)) else NA
  cat("<rmet ", kind, "> ", format(nrow(x), big.mark = ","), " rows x ",
      ncol(x), " columns", sep = "")
  if (!is.na(stations)) cat(", ", stations, if (stations == 1) " station" else " stations", sep = "")
  cat("\n")
  if (!is.null(time_column) && nrow(x) && !all(is.na(x[[time_column]]))) {
    range <- range(x[[time_column]], na.rm = TRUE)
    zone <- if (inherits(range, "POSIXct")) paste0(" (", attr(x[[time_column]], "tzone")[1] %||% "UTC", ")") else ""
    cat("Period: ", format(range[[1]]), " to ", format(range[[2]]), zone, "\n", sep = "")
  }
  flagged <- grep("_flag$", names(x), value = TRUE)
  if (length(flagged)) cat("Quality control: ", length(flagged), " flag column(s)\n", sep = "")

  shown <- as.data.frame(x)[seq_len(min(nrow(x), n)), , drop = FALSE]
  print(shown, ...)
  if (nrow(x) > n) {
    cat("... with ", format(nrow(x) - n, big.mark = ","), " more rows\n", sep = "")
  }
  invisible(x)
}

#' @rdname rmet-classes
#' @export
summary.rmet_data <- function(object, ...) {
  time_column <- .rmet_time_column(object)
  station <- if ("station_code" %in% names(object)) object$station_code else rep(NA_character_, nrow(object))
  group <- match(station, unique(station))
  first_row <- match(seq_along(unique(station)), group)
  measurement <- .rmet_measurement_columns(object)
  present <- if (length(measurement)) {
    valid <- vapply(measurement, function(v) !is.na(object[[v]]), logical(nrow(object)))
    valid <- matrix(valid, nrow = nrow(object))
    rowsum(rowMeans(valid) * 100, group, reorder = TRUE)[, 1]
  } else rep(NA_real_, length(first_row))
  out <- data.frame(
    station_code = station[first_row],
    n_rows = tabulate(group, nbins = length(first_row)),
    stringsAsFactors = FALSE
  )
  if (!is.null(time_column)) {
    times <- object[[time_column]]
    limit <- function(f) {
      value <- vapply(split(as.numeric(times), group), f, numeric(1), USE.NAMES = FALSE)
      value[is.infinite(value)] <- NA_real_
      if (inherits(times, "POSIXct")) .POSIXct(value, tz = attr(times, "tzone")[1] %||% "UTC")
      else structure(value, class = "Date")
    }
    out$first <- limit(function(v) suppressWarnings(min(v, na.rm = TRUE)))
    out$last <- limit(function(v) suppressWarnings(max(v, na.rm = TRUE)))
  }
  out$complete_pct <- round(unname(present) / tabulate(group, nbins = length(first_row)), 1)
  rownames(out) <- NULL
  structure(out, class = c("summary.rmet_data", "data.frame"),
            kind = sub("^rmet_", "", class(object)[[1]]))
}

#' @rdname rmet-classes
#' @export
print.summary.rmet_data <- function(x, ...) {
  kind <- attr(x, "kind")
  attr(x, "kind") <- NULL
  class(x) <- "data.frame"
  cat("<summary of rmet ", kind, " data>\n", sep = "")
  print(x, ...)
  cat("complete_pct: mean percentage of non-missing values in the measurement columns\n")
  invisible(x)
}

#' @rdname rmet-classes
#' @export
as.data.frame.rmet_data <- function(x, row.names = NULL, optional = FALSE, ...) {
  class(x) <- "data.frame"
  x
}

#' @param variable For `plot()`: name of the numeric column to draw. Default:
#'   the first of air temperature, mean temperature, precipitation, or the
#'   first measurement column.
#' @param station For `plot()`: station codes to draw. Default: all (at most
#'   eight are drawn when this is not given).
#' @param y,type,xlab,ylab For `plot()`: ignored `y`, and graphical arguments
#'   passed on; `...` goes to [graphics::plot()].
#' @rdname rmet-classes
#' @export
plot.rmet_data <- function(x, y = NULL, variable = NULL, station = NULL,
                           type = "l", xlab = "", ylab = NULL, ...) {
  time_column <- .rmet_time_column(x)
  if (is.null(time_column)) {
    stop("The data have no `datetime`, `date` or `month` column to plot.", call. = FALSE)
  }
  measurement <- .rmet_measurement_columns(x)
  if (is.null(variable)) {
    preferred <- intersect(c("temp_dry_c", "temp_mean_c", "precip_mm"), measurement)
    variable <- c(preferred, measurement)[1]
  }
  if (!is.character(variable) || length(variable) != 1L || !variable %in% measurement) {
    stop("`variable` must be one numeric measurement column of `x`.", call. = FALSE)
  }
  codes <- if ("station_code" %in% names(x)) x$station_code else rep("", nrow(x))
  chosen <- if (is.null(station)) utils::head(unique(codes), 8L) else toupper(station)
  keep <- codes %in% chosen
  if (!any(keep)) stop("No data for the requested station(s).", call. = FALSE)

  time <- x[[time_column]][keep]
  value <- as.numeric(x[[variable]][keep])
  code <- codes[keep]
  colours <- grDevices::hcl.colors(max(length(unique(code)), 2L), "Dark 3")
  graphics::plot(range(time, na.rm = TRUE), range(value, na.rm = TRUE), type = "n",
                 xlab = xlab, ylab = if (is.null(ylab)) variable else ylab, ...)
  for (i in seq_along(unique(code))) {
    rows <- code == unique(code)[[i]]
    ord <- order(time[rows])
    graphics::lines(time[rows][ord], value[rows][ord], type = type, col = colours[[i]])
  }
  if (length(unique(code)) > 1L) {
    graphics::legend("topright", legend = unique(code), col = colours[seq_along(unique(code))],
                     lty = 1, bty = "n", cex = 0.8)
  }
  invisible(x)
}