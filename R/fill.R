#' Fill short gaps in hourly or daily data
#'
#' Fills missing values by linear interpolation across short gaps and, for what
#' remains, with the typical value for that day of the year (or hour of the
#' day). Every filled value is marked in a `<variable>_filled` column, so the
#' result can always be traced back to measurements.
#'
#' @param x A data frame with a `datetime` column (hourly data from
#'   [inmet_read()]) or a `date` column (daily data from [inmet_daily()]).
#' @param variables Measurement columns to fill. By default every numeric
#'   measurement column except precipitation, wind direction and radiation.
#'   Precipitation and wind direction cannot be filled: interpolating rain
#'   invents events, and directions are circular.
#' @param max_gap Longest gap, in hours (hourly data) or days (daily data), that
#'   is filled by interpolation. A gap is the number of consecutive missing
#'   steps, including absent rows, between two valid values.
#' @param method `"both"` (default) interpolates and then uses the typical
#'   value; `"linear"` and `"climatology"` use only one of them.
#' @param min_years For the typical value: minimum number of valid values of
#'   the station for that day of the year (or hour of the day) that are needed
#'   to compute the mean.
#'
#' @return `x` with the missing values filled and one `<variable>_filled`
#'   column per filled variable, holding `"linear"`, `"climatology"` or `NA`.
#'   Rows that are absent from `x` are not created.
#'
#' @details
#' Each station is filled on its own. The typical value is the mean of the
#' valid values of the same station on the same month and day (daily data) or
#' month, day and UTC hour (hourly data), across all years in `x`; it needs
#' several years of data to be meaningful, and it smooths out extreme days.
#' Check how much was filled before an analysis:
#' `table(filled$temp_dry_c_filled, useNA = "ifany")`.
#'
#' @examples
#' hourly <- rmet_example
#' hourly$temp_dry_c[100:102] <- NA
#' filled <- inmet_fill(hourly, "temp_dry_c", method = "linear")
#' table(filled$temp_dry_c_filled, useNA = "ifany")
#' @seealso [inmet_completeness()], [inmet_qc()]
#' @export
inmet_fill <- function(x, variables = NULL, max_gap = 3L,
                       method = c("both", "linear", "climatology"),
                       min_years = 3L) {
  if (.is_no_data(x)) return(x)
  method <- match.arg(method)
  max_gap <- .validate_positive_integer(max_gap, "max_gap")
  min_years <- .validate_positive_integer(min_years, "min_years")
  hourly <- is.data.frame(x) && inherits(x[["datetime"]], "POSIXct")
  daily <- is.data.frame(x) && inherits(x[["date"]], "Date")
  if (!hourly && !daily) {
    stop("`x` must be a data frame with a POSIXct `datetime` column or a Date ",
         "`date` column.", call. = FALSE)
  }

  never <- c("precip_mm", "wind_dir_deg")
  available <- .rmet_measurement_columns(x)
  if (is.null(variables)) {
    variables <- setdiff(available, c(never, "radiation_kjm2", "radiation_mj_m2"))
  } else {
    variables <- .validate_character_filter(variables, "variables")
    if (length(bad <- intersect(variables, never))) {
      stop("Cannot fill ", paste(bad, collapse = ", "),
           ": rainfall and wind direction cannot be interpolated.", call. = FALSE)
    }
    if (length(unknown <- setdiff(variables, available))) {
      stop("Unknown variables: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
    }
  }

  station <- if ("station_code" %in% names(x)) x$station_code else rep("", nrow(x))
  if (hourly) {
    position <- round(as.numeric(x$datetime) / 3600)
    key <- format(x$datetime, "%m-%d %H", tz = "UTC")
  } else {
    position <- as.numeric(x$date)
    key <- format(x$date, "%m-%d")
  }
  groups <- split(seq_len(nrow(x)), match(station, unique(station)))

  for (v in variables) {
    values <- as.numeric(x[[v]])
    how <- rep(NA_character_, length(values))
    for (rows in groups) {
      ord <- rows[order(position[rows])]
      result <- .fill_station(values[ord], position[ord], key[ord],
                              max_gap, method, min_years)
      values[ord] <- result$values
      how[ord] <- result$how
    }
    x[[v]] <- values
    x[[paste0(v, "_filled")]] <- how
  }
  x
}

# Fills one station's values, given in time order.
.fill_station <- function(values, position, key, max_gap, method, min_years) {
  how <- rep(NA_character_, length(values))
  valid <- !is.na(values)
  original <- values

  if (method != "climatology" && sum(valid) >= 2L) {
    known <- position[valid]
    missing <- which(!valid)
    left <- findInterval(position[missing], known)
    inside <- left >= 1L & left < length(known)
    missing <- missing[inside]
    left <- left[inside]
    span <- known[left + 1L] - known[left]
    close <- span - 1L <= max_gap
    missing <- missing[close]
    left <- left[close]
    span <- span[close]
    fraction <- (position[missing] - known[left]) / span
    values[missing] <- original[valid][left] +
      fraction * (original[valid][left + 1L] - original[valid][left])
    how[missing] <- "linear"
  }

  if (method != "linear") {
    count <- tapply(valid, key, sum)
    typical <- tapply(original, key, mean, na.rm = TRUE)
    todo <- which(is.na(values))
    usable <- count[key[todo]] >= min_years
    todo <- todo[usable]
    values[todo] <- typical[key[todo]]
    how[todo] <- "climatology"
  }
  list(values = unname(values), how = how)
}
