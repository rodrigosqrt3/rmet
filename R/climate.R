#' Climatological normals
#'
#' Computes the mean of each calendar month over the years available for each
#' station, from the output of [inmet_monthly()].
#'
#' @param x A data frame as returned by [inmet_monthly()].
#' @param variables Numeric columns to average. Default: all numeric
#'   measurement columns (everything except identifiers and counts).
#' @param years Optional integer vector of years to use as the reference
#'   period, such as `1991:2020`. Default: all years in `x`.
#' @param min_years Minimum number of years with a value that a station needs
#'   for a calendar month; otherwise the normal is `NA`. WMO uses 30 years for
#'   official normals; the default is lower so that shorter series give
#'   something, but check `n_years`.
#'
#' @return A data frame with one row per station and calendar month:
#'   `station_code`, `month_of_year` (1 to 12), `n_years` (the largest number
#'   of years behind any variable) and the mean of each variable.
#'
#' @examples
#' normals <- inmet_normals(inmet_monthly(inmet_daily(rmet_example)), min_years = 1)
#' normals[, c("month_of_year", "temp_mean_c", "precip_mm")]
#' @seealso [inmet_anomaly()], [inmet_monthly()]
#' @export
inmet_normals <- function(x, variables = NULL, years = NULL, min_years = 10L) {
  if (.is_no_data(x)) return(data.frame())
  min_years <- .validate_positive_integer(min_years, "min_years")
  if (!is.data.frame(x) || !inherits(x[["month"]], "Date")) {
    stop("`x` must be a data frame with a `month` column, as returned by ",
         "inmet_monthly().", call. = FALSE)
  }
  variables <- .monthly_variables(x, variables)
  if (!is.null(years)) {
    if (!is.numeric(years) || !length(years) || anyNA(years) || any(years != floor(years))) {
      stop("`years` must be a vector of whole years.", call. = FALSE)
    }
    x <- x[as.integer(format(x$month, "%Y")) %in% years, , drop = FALSE]
  }
  if (!nrow(x)) return(data.frame())

  station <- if ("station_code" %in% names(x)) x$station_code else rep("", nrow(x))
  calendar <- as.integer(format(x$month, "%m"))
  key <- paste(station, calendar)
  group <- match(key, unique(key))
  first_row <- match(seq_along(unique(key)), group)

  out <- list(station_code = station[first_row], month_of_year = calendar[first_row])
  counts <- matrix(0L, length(first_row), length(variables))
  for (j in seq_along(variables)) {
    values <- as.numeric(x[[variables[[j]]]])
    stat <- .group_stat(values, group, length(first_row), "mean")
    counts[, j] <- stat$count
    out[[variables[[j]]]] <- ifelse(stat$count >= min_years, stat$value, NA_real_)
  }
  out <- c(out[1:2], list(n_years = as.integer(apply(counts, 1, max))), out[-(1:2)])
  out <- .new_data_frame(out, length(first_row))
  out <- out[order(out$station_code, out$month_of_year), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Anomalies relative to the climatological normal
#'
#' Subtracts (or divides by) the normal of each calendar month from the values
#' of [inmet_monthly()].
#'
#' @param x A data frame as returned by [inmet_monthly()].
#' @param normals A data frame from [inmet_normals()]. By default the normals
#'   are computed from `x` itself, which is only appropriate when `x` covers
#'   a long reference period.
#' @param variables Numeric columns to compare; by default the variables of
#'   `normals`.
#' @param relative If `TRUE`, the anomaly is the percentage of the normal
#'   (useful for precipitation) instead of the difference.
#' @param ... Further arguments passed to [inmet_normals()] when `normals` is
#'   not given, such as `years` or `min_years`.
#'
#' @return `x` with one `<variable>_anom` column per variable.
#'
#' @examples
#' monthly <- inmet_monthly(inmet_daily(rmet_example))
#' anomalies <- inmet_anomaly(monthly, min_years = 1)
#' anomalies[, c("month", "temp_mean_c", "temp_mean_c_anom")]
#' @seealso [inmet_normals()]
#' @export
inmet_anomaly <- function(x, normals = NULL, variables = NULL, relative = FALSE, ...) {
  if (.is_no_data(x)) return(x)
  relative <- .validate_flag(relative, "relative")
  if (!is.data.frame(x) || !inherits(x[["month"]], "Date")) {
    stop("`x` must be a data frame with a `month` column, as returned by ",
         "inmet_monthly().", call. = FALSE)
  }
  if (is.null(normals)) normals <- inmet_normals(x, variables = variables, ...)
  if (!is.data.frame(normals) || !all(c("month_of_year", "station_code") %in% names(normals))) {
    stop("`normals` must be the output of inmet_normals().", call. = FALSE)
  }
  variables <- .validate_character_filter(variables, "variables")
  if (is.null(variables)) {
    variables <- setdiff(names(normals), c("station_code", "month_of_year", "n_years"))
  }
  unknown <- setdiff(variables, intersect(names(x), names(normals)))
  if (length(unknown)) {
    stop("Variables missing from `x` or `normals`: ", paste(unknown, collapse = ", "),
         ".", call. = FALSE)
  }
  station <- if ("station_code" %in% names(x)) x$station_code else rep("", nrow(x))
  row <- match(paste(station, as.integer(format(x$month, "%m"))),
               paste(normals$station_code, normals$month_of_year))
  for (v in variables) {
    reference <- as.numeric(normals[[v]])[row]
    value <- as.numeric(x[[v]])
    x[[paste0(v, "_anom")]] <- if (relative) 100 * value / reference else value - reference
  }
  x
}

# Numeric columns of monthly data that can be summarised.
.monthly_variables <- function(x, variables) {
  available <- setdiff(.rmet_measurement_columns(x), "n_days")
  if (is.null(variables)) return(available)
  variables <- .validate_character_filter(variables, "variables")
  unknown <- setdiff(variables, available)
  if (length(unknown)) {
    stop("Unknown variables: ", paste(unknown, collapse = ", "), ".", call. = FALSE)
  }
  variables
}
