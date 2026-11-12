#' Which stations and periods are in the cached archives
#'
#' Lists the station files inside the annual archives that were already
#' downloaded, without reading any data. It is a quick way to see which
#' stations have data in a year, and which period each one covers, before
#' calling [inmet_read()].
#'
#' @param years Integer years to list, or `NULL` (default) for every cached
#'   archive. Years without a valid cached archive are skipped with a warning.
#' @param stations Optional station codes, such as `"A801"`.
#' @param dest_dir Directory with the `<year>.zip` archives, [inmet_cache_dir()]
#'   by default.
#'
#' @return A data frame with one row per station and year: `station_code`,
#'   `state`, `year`, `start` and `end` (first and last day covered by the
#'   station files, from their names; `NA` when a name has no period) and
#'   `n_files` (the current-year archive has several cumulative files per
#'   station).
#'
#' @details
#' Only the file names are used, so the call is fast even for archives of
#' 100 MB. The period reflects what INMET announces in the names; a station
#' can still have gaps inside it (see [inmet_completeness()]).
#'
#' @examples
#' # an empty cache gives an empty result
#' inmet_availability(dest_dir = file.path(tempdir(), "rmet-empty"))
#' @seealso [inmet_download()], [inmet_stations()], [inmet_cache_status()]
#' @export
inmet_availability <- function(years = NULL, stations = NULL,
                               dest_dir = inmet_cache_dir()) {
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  stations <- .validate_character_filter(stations, "stations", uppercase = TRUE)
  empty <- data.frame(
    station_code = character(), state = character(), year = integer(),
    start = as.Date(character()), end = as.Date(character()),
    n_files = integer(), stringsAsFactors = FALSE
  )

  if (is.null(years)) {
    files <- if (dir.exists(dest_dir)) {
      list.files(dest_dir, pattern = "^\\d{4}\\.zip$")
    } else character()
    years <- sort(as.integer(sub("\\.zip$", "", files)))
  } else {
    years <- .validate_years(years)
  }
  if (!length(years)) return(empty)

  by_year <- lapply(years, function(year) {
    entries <- .archive_csv_entries(file.path(dest_dir, paste0(year, ".zip")))
    if (is.null(entries)) {
      warning("No valid archive for year ", year, " in '", dest_dir, "'.", call. = FALSE)
      return(NULL)
    }
    .describe_entries(entries, year)
  })
  out <- do.call(rbind, c(list(empty), Filter(Negate(is.null), by_year)))
  if (!is.null(stations)) out <- out[out$station_code %in% stations, , drop = FALSE]
  out <- out[order(out$station_code, out$year), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# Earliest or latest date per group (NA when a group has no dates).
.period_limit <- function(dates, group, fun) {
  values <- vapply(split(as.numeric(dates), group), function(v) {
    v <- v[!is.na(v)]
    if (length(v)) fun(v) else NA_real_
  }, numeric(1), USE.NAMES = FALSE)
  structure(values, class = "Date")
}

# One row per station for the file names of one archive.
.describe_entries <- function(entries, year) {
  name <- basename(entries)
  code <- vapply(name, .extract_station_code, character(1), USE.NAMES = FALSE)
  state <- sub("^INMET_[A-Z]+_([A-Z]{2})_.*$", "\\1", toupper(name))
  state[state == toupper(name)] <- NA_character_
  period <- regmatches(name, regexec("(\\d{2}-\\d{2}-\\d{4})_A_(\\d{2}-\\d{2}-\\d{4})", name))
  start <- as.Date(vapply(period, function(m) if (length(m)) m[[2]] else NA_character_,
                          character(1)), format = "%d-%m-%Y")
  end <- as.Date(vapply(period, function(m) if (length(m)) m[[3]] else NA_character_,
                        character(1)), format = "%d-%m-%Y")
  keep <- !is.na(code)
  if (!any(keep)) return(NULL)
  code <- code[keep]; state <- state[keep]; start <- start[keep]; end <- end[keep]
  group <- match(code, unique(code))
  first <- match(seq_along(unique(code)), group)
  data.frame(
    station_code = code[first],
    state = state[first],
    year = rep(as.integer(year), length(first)),
    start = .period_limit(start, group, min),
    end = .period_limit(end, group, max),
    n_files = as.integer(tabulate(group)),
    stringsAsFactors = FALSE
  )
}
