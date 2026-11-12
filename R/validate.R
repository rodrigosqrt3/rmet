# Argument validation helpers. Each returns the cleaned value or stops with a
# message naming the offending argument.

.validate_years <- function(years) {
  if (!length(years) || !is.atomic(years)) {
    stop("`years` must be a non-empty vector of integer years.", call. = FALSE)
  }
  numeric_years <- suppressWarnings(as.numeric(years))
  if (anyNA(numeric_years) || any(!is.finite(numeric_years)) ||
      any(numeric_years != floor(numeric_years))) {
    stop("`years` must be a vector of integers.", call. = FALSE)
  }
  years <- as.integer(numeric_years)
  if (any(years < 2000L)) {
    stop("INMET historical archives are available from 2000 onwards.", call. = FALSE)
  }
  if (any(years > .current_year())) {
    stop("Cannot request years in the future.", call. = FALSE)
  }
  unique(years)
}

# The empty data frame (no columns) that inmet_read() returns when nothing was
# read. Functions that take hourly data pass it through instead of failing.
.is_no_data <- function(x) is.data.frame(x) && ncol(x) == 0L

.current_year <-function() as.integer(format(Sys.Date(), "%Y"))

.validate_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

.validate_positive_integer <- function(x, name) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value < 1 || value != floor(value)) {
    stop("`", name, "` must be a positive integer.", call. = FALSE)
  }
  as.integer(value)
}

.validate_directory <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be a single non-empty path.", call. = FALSE)
  }
  path.expand(x)
}

.validate_timezone <- function(tz) {
  if (!is.character(tz) || length(tz) != 1L || is.na(tz) || !nzchar(tz)) {
    stop("`tz` must be a single time-zone name.", call. = FALSE)
  }
  if (!tz %in% OlsonNames()) {
    stop("Unknown timezone: '", tz, "'. See OlsonNames().", call. = FALSE)
  }
  tz
}

.validate_character_filter <- function(x, name, uppercase = FALSE) {
  if (is.null(x)) return(NULL)
  if (!is.character(x) || anyNA(x) || any(!nzchar(trimws(x)))) {
    stop("`", name, "` must be NULL or a character vector without missing values.",
         call. = FALSE)
  }
  x <- trimws(x)
  if (uppercase) x <- toupper(x)
  unique(x)
}

.validate_coordinates <- function(x, name, limit) {
  if (!is.numeric(x) || !length(x) || anyNA(x) || any(!is.finite(x)) ||
      any(abs(x) > limit)) {
    stop("`", name, "` must be numeric values between ", -limit, " and ", limit, ".",
         call. = FALSE)
  }
  as.numeric(x)
}

.validate_number <-function(x, name, lower = -Inf, upper = Inf) {
  if (!is.numeric(x) || length(x) != 1L || is.na(x) || !is.finite(x) ||
      x < lower || x > upper) {
    stop("`", name, "` must be a single number between ", lower, " and ", upper, ".",
         call. = FALSE)
  }
  as.numeric(x)
}

# Date limits are inclusive whole days interpreted in `tz`. Returns POSIXct
# bounds (or NULL) so that they can be compared directly with timestamps.
.validate_date_bounds <- function(start_date, end_date, tz) {
  parse_one <- function(x, name, end = FALSE) {
    if (is.null(x)) return(NULL)
    if (inherits(x, "Date")) x <- format(x, "%Y-%m-%d")
    if (!is.character(x) || length(x) != 1L || is.na(x)) {
      stop("`", name, "` must be one date in YYYY-MM-DD format.", call. = FALSE)
    }
    value <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
    if (is.na(value) || format(value, "%Y-%m-%d") != x) {
      stop("`", name, "` must be a valid date in YYYY-MM-DD format.", call. = FALSE)
    }
    as.POSIXct(paste(x, if (end) "23:59:59" else "00:00:00"), tz = tz)
  }
  start <- parse_one(start_date, "start_date")
  end <- parse_one(end_date, "end_date", end = TRUE)
  if (!is.null(start) && !is.null(end) && start > end) {
    stop("`start_date` must not be after `end_date`.", call. = FALSE)
  }
  list(start = start, end = end)
}
