#' Read cached INMET historical data
#'
#' Parses the station CSV files inside annual INMET archives (downloaded with
#' [inmet_download()]) into one data frame. Files are read directly from the
#' ZIP archives; nothing is extracted to disk.
#'
#' @param years Integer vector of years.
#' @param stations Optional station codes, such as `"A801"`. Matching is
#'   case-insensitive. Use [inmet_stations()] or [inmet_nearest()] to find
#'   codes.
#' @param dest_dir Directory containing `<year>.zip` archives.
#' @param tz Output time zone. Defaults to `"UTC"`, the time standard used in
#'   the source files. Any name in [OlsonNames()] is accepted; only the display
#'   of the timestamps changes, not the instants they represent.
#' @param variables Optional measurement columns to retain (see *Columns*).
#'   Identifier columns are always kept.
#' @param start_date,end_date Optional inclusive date limits in `YYYY-MM-DD`
#'   format, interpreted in `tz`.
#' @param drop_empty Drop hours in which every measurement is missing
#'   (default). INMET files contain such rows for periods when a station was
#'   not reporting. Use `FALSE` to keep them, for example to obtain a regular
#'   hourly grid.
#' @param quiet Suppress progress messages.
#'
#' @return A data frame with one row per station and hour, sorted by station
#'   code and time. Identifier columns come first: `station_code`,
#'   `station_name`, `region`, `state`, `latitude`, `longitude`, `elevation`
#'   (from each file's header) and `datetime` (`POSIXct`). Measurement columns
#'   are numeric; missing values are `NA`.
#'
#' @section Repeated files:
#' The archive of the current year contains several cumulative versions of
#' each station file (for example, January to August and January to
#' September). Each station and hour is returned once, taken from the most
#' recent version that contains it.
#'
#' @section Timestamps:
#' Each row is stamped with the *end* of the hour it describes, in UTC. Values
#' labelled "previous hour" (such as `temp_max_prev_c` and `precip_mm`)
#' summarise the hour ending at `datetime`. Use [inmet_daily()] to build daily
#' summaries in local time.
#'
#' @section Columns:
#' | Column | Description | Unit |
#' |---|---|---|
#' | `precip_mm` | Precipitation in the previous hour | mm |
#' | `pressure_station_hpa` | Station-level pressure | hPa |
#' | `pressure_max_hpa`, `pressure_min_hpa` | Pressure extremes, previous hour | hPa |
#' | `radiation_kjm2` | Global radiation, previous hour | kJ/m^2 |
#' | `temp_dry_c` | Air (dry-bulb) temperature | degC |
#' | `temp_dew_c` | Dew-point temperature | degC |
#' | `temp_max_prev_c`, `temp_min_prev_c` | Air temperature extremes, previous hour | degC |
#' | `temp_dew_max_c`, `temp_dew_min_c` | Dew-point extremes, previous hour | degC |
#' | `humid_rel_pct` | Relative humidity | % |
#' | `humid_rel_max_pct`, `humid_rel_min_pct` | Humidity extremes, previous hour | % |
#' | `wind_dir_deg` | Wind direction | degrees |
#' | `wind_gust_ms` | Maximum gust | m/s |
#' | `wind_speed_ms` | Wind speed | m/s |
#'
#' Unrecognised source columns are kept under a snake_case version of their
#' original title.
#'
#' @examples
#' \donttest{
#' cache <- file.path(tempdir(), "rmet")
#' inmet_download(2023, dest_dir = cache)
#' poa <- inmet_read(2023, stations = "A801", dest_dir = cache)
#' }
#' @seealso [inmet_get()], [inmet_download()], [inmet_daily()],
#'   [inmet_stations()]
#' @export
inmet_read <- function(
    years,
    stations = NULL,
    dest_dir = inmet_cache_dir(),
    tz = "UTC",
    variables = NULL,
    start_date = NULL,
    end_date = NULL,
    drop_empty = TRUE,
    quiet = FALSE
) {
  years <- .validate_years(years)
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  quiet <- .validate_flag(quiet, "quiet")
  drop_empty <- .validate_flag(drop_empty, "drop_empty")
  tz <- .validate_timezone(tz)
  stations <- .validate_character_filter(stations, "stations", uppercase = TRUE)
  variables <- .validate_character_filter(variables, "variables")
  bounds <- .validate_date_bounds(start_date, end_date, tz)

  work_dir <- tempfile("rmet-read-")
  if (!dir.create(work_dir)) stop("Could not create a temporary directory.", call. = FALSE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  found_stations <- character()
  by_year <- vector("list", length(years))

  for (k in seq_along(years)) {
    year <- years[[k]]
    archive <- file.path(dest_dir, paste0(year, ".zip"))
    if (!file.exists(archive)) {
      stop(sprintf("ZIP for year %d not found at '%s'. Run inmet_download(%d) first.",
                   year, archive, year), call. = FALSE)
    }
    listing <- .archive_csv_listing(archive)
    if (is.null(listing)) stop("Archive is invalid or incomplete: ", archive, call. = FALSE)
    entries <- listing$Name
    sizes <- stats::setNames(listing$Length, listing$Name)

    if (!is.null(stations)) {
      codes <- vapply(entries, .extract_station_code, character(1), USE.NAMES = FALSE)
      entries <- entries[!is.na(codes) & codes %in% stations]
      found_stations <- c(found_stations, codes[!is.na(codes) & codes %in% stations])
      if (!length(entries)) {
        warning(sprintf("No matching stations found in year %d.", year), call. = FALSE)
        next
      }
    }
    if (!quiet) message("Reading year ", year, " (", length(entries), " file(s)) ...")

    # Newest file versions first, so that their rows win in .drop_superseded()
    entries <- entries[order(-as.numeric(.entry_end_date(entries)), na.last = TRUE)]
    by_year[[k]] <- .safe_rbind(lapply(entries, function(entry) {
      .read_archive_entry(archive, entry, sizes[[entry]], work_dir, tz, variables,
                          bounds, drop_empty)
    }))
  }

  if (!is.null(stations) && length(years) > 1L) {
    absent <- setdiff(stations, found_stations)
    if (length(absent)) {
      warning("Station(s) not found in any requested year: ",
              paste(absent, collapse = ", "), call. = FALSE)
    }
  }

  out <- .safe_rbind(by_year)
  if (is.null(out)) {
    warning("No data was read. Returning empty data.frame.", call. = FALSE)
    return(data.frame())
  }
  out <- .drop_superseded(out, quiet)
  if (!is.null(variables)) {
    unknown <- setdiff(variables, setdiff(names(out), .inmet_id_columns()))
    if (length(unknown)) {
      warning("Unknown variables ignored: ", paste(unknown, collapse = ", "), call. = FALSE)
    }
  }
  .as_rmet(out, "rmet_hourly")
}

# End of the period covered by a station file, from names such as
# "INMET_S_RS_A801_PORTO ALEGRE_01-01-2026_A_30-09-2026.CSV" (NA if absent).
.entry_end_date <- function(entries) {
  end <- regmatches(entries, regexpr("(?<=_A_)\\d{2}-\\d{2}-\\d{4}", entries, perl = TRUE))
  out <- rep(as.Date(NA), length(entries))
  has <- grepl("_A_\\d{2}-\\d{2}-\\d{4}", entries, perl = TRUE)
  out[has] <- as.Date(end, format = "%d-%m-%Y")
  out
}

# The archive of the current year holds cumulative versions of each station
# file (e.g. "01-01-2026_A_31-08-2026" and "01-01-2026_A_30-09-2026"), so the
# same hours appear several times. Rows are read newest version first; this
# keeps the first row for every station and timestamp (INMET occasionally
# revises values between versions) and orders the result by station and time.
.drop_superseded <- function(df, quiet = TRUE) {
  # integer station ranks (alphabetical) allow fast radix ordering of the key
  station_id <- match(df$station_code, sort(unique(df$station_code), na.last = TRUE))
  key <- station_id * 1e10 + as.numeric(df$datetime)
  duplicate <- !is.na(key) & duplicated(key)
  if (any(duplicate)) {
    if (!quiet) {
      message("  Dropped ", sum(duplicate), " row(s) superseded by newer versions ",
              "of the same station files.")
    }
    df <- df[!duplicate, , drop = FALSE]
    key <- key[!duplicate]
  }
  if (is.unsorted(key, na.rm = TRUE)) {
    df <- df[order(key, method = "radix"), , drop = FALSE]
  }
  rownames(df) <- NULL
  df
}

# Names and uncompressed sizes of the CSV entries of an archive (NULL if the
# archive cannot be listed).
.archive_csv_listing <- function(archive) {
  listing <- tryCatch(utils::unzip(archive, list = TRUE), error = function(e) NULL)
  if (is.null(listing) || !nrow(listing)) return(NULL)
  names <- listing$Name
  keep <- grepl("\\.CSV$", names, ignore.case = TRUE) &
    !grepl("(^|/)__MACOSX(/|$)", names) &
    !.unsafe_archive_path(names)
  listing[keep, c("Name", "Length"), drop = FALSE]
}

# CSV entries of an archive (NULL if the archive cannot be listed).
.archive_csv_entries <- function(archive) {
  listing <- .archive_csv_listing(archive)
  if (is.null(listing)) NULL else listing$Name
}

# Parses one station file; problems become warnings so that a single damaged
# file does not abort reading a whole year.
.read_archive_entry <- function(archive, entry, size, work_dir, tz, variables,
                                bounds, drop_empty) {
  bytes <- tryCatch(
    .read_entry_bytes(archive, entry, size, work_dir),
    error = function(e) {
      warning("Could not extract '", entry, "': ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
  if (is.null(bytes)) return(NULL)
  tryCatch(
    .parse_inmet_raw(
      bytes, tz = tz, variables = variables, dt_start = bounds$start,
      dt_end = bounds$end, drop_empty = drop_empty,
      fallback_code = .extract_station_code(entry)
    ),
    error = function(e) {
      warning("Could not parse '", entry, "': ", conditionMessage(e), call. = FALSE)
      NULL
    }
  )
}

#' Extract INMET archives
#'
#' Copies the original station CSV files out of the annual archives, for use
#' with other software.
#'
#' @param years Integer years to extract.
#' @param dest_dir Directory containing annual ZIP files.
#' @param out_dir Destination directory.
#' @param stations Optional station codes; only their files are extracted.
#' @param overwrite Overwrite existing files.
#' @param quiet Suppress progress messages.
#' @return Invisibly, the paths of the extracted files.
#' @examples
#' \donttest{
#' cache <- file.path(tempdir(), "rmet")
#' inmet_download(2023, dest_dir = cache)
#' inmet_extract(2023, dest_dir = cache, out_dir = file.path(tempdir(), "csv"),
#'               stations = "A801")
#' }
#' @export
inmet_extract <- function(
    years,
    dest_dir = inmet_cache_dir(),
    out_dir = file.path(dest_dir, "csv"),
    stations = NULL,
    overwrite = FALSE,
    quiet = FALSE
) {
  years <- .validate_years(years)
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  out_dir <- .validate_directory(out_dir, "out_dir")
  stations <- .validate_character_filter(stations, "stations", uppercase = TRUE)
  overwrite <- .validate_flag(overwrite, "overwrite")
  quiet <- .validate_flag(quiet, "quiet")
  if (!dir.exists(out_dir) && !dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create `out_dir`: ", out_dir, call. = FALSE)
  }

  paths <- character()
  for (year in years) {
    archive <- file.path(dest_dir, paste0(year, ".zip"))
    if (!.is_valid_zip(archive)) {
      warning(sprintf("Valid ZIP for year %d not found; skipping.", year), call. = FALSE)
      next
    }
    entries <- utils::unzip(archive, list = TRUE)$Name
    if (any(.unsafe_archive_path(entries))) {
      warning("Archive for ", year, " contains unsafe paths; skipping.", call. = FALSE)
      next
    }
    if (!is.null(stations)) {
      codes <- vapply(entries, .extract_station_code, character(1), USE.NAMES = FALSE)
      entries <- entries[!is.na(codes) & codes %in% stations]
      if (!length(entries)) next
    }
    if (!quiet) message("Extracting year ", year, " ...")
    paths <- c(paths, utils::unzip(archive, files = entries, exdir = out_dir,
                                   overwrite = overwrite))
  }
  invisible(paths)
}
