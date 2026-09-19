#' Read cached INMET historical data
#'
#' Parses CSV files inside annual INMET archives into a consistent data frame.
#'
#' @param years Integer vector of years.
#' @param stations Optional station codes. Matching is case-insensitive.
#' @param dest_dir Directory containing `<year>.zip` archives.
#' @param tz Output time zone. Defaults to `"UTC"`, the time standard used in
#'   the source files. Any name in [OlsonNames()] is accepted.
#' @param variables Optional meteorological columns to retain.
#' @param start_date,end_date Optional inclusive date limits in `YYYY-MM-DD`
#'   format, interpreted in `tz`.
#' @param quiet Suppress progress messages.
#'
#' @return A data frame. Identifier and station metadata columns are always
#' retained when data are available.
#'
#' @examples
#' \donttest{
#' df <- inmet_read(2023, stations = "A801", dest_dir = tempdir())
#' }
#' @seealso [inmet_get()], [inmet_download()], [inmet_stations()]
#' @export
inmet_read <- function(
    years,
    stations = NULL,
    dest_dir = tools::R_user_dir("rmet", "cache"),
    tz = "UTC",
    variables = NULL,
    start_date = NULL,
    end_date = NULL,
    quiet = FALSE
) {
  years <- .validate_years(years)
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  quiet <- .validate_flag(quiet, "quiet")
  tz <- .validate_timezone(tz)
  stations <- .validate_character_filter(stations, "stations", uppercase = TRUE)
  variables <- .validate_character_filter(variables, "variables")
  bounds <- .validate_date_bounds(start_date, end_date, tz)

  work_dir <- tempfile("rmet-read-")
  if (!dir.create(work_dir)) stop("Could not create a temporary directory.", call. = FALSE)
  on.exit(unlink(work_dir, recursive = TRUE, force = TRUE), add = TRUE)

  all_chunks <- vector("list", length(years))

  for (k in seq_along(years)) {
    year <- years[k]
    archive <- file.path(dest_dir, paste0(year, ".zip"))
    if (!file.exists(archive)) {
      stop(
        sprintf("ZIP for year %d not found at '%s'. Run inmet_download(%d) first.",
                year, archive, year),
        call. = FALSE
      )
    }
    if (!.is_valid_zip(archive)) {
      stop("Archive is invalid or incomplete: ", archive, call. = FALSE)
    }
    if (!quiet) message("Reading year ", year, " ...")

    entries <- utils::unzip(archive, list = TRUE)
    csv_names <- entries$Name[
      grepl("\\.CSV$", entries$Name, ignore.case = TRUE) &
        !grepl("(^|/)__MACOSX(/|$)", entries$Name)
    ]
    csv_names <- csv_names[!.unsafe_archive_path(csv_names)]

    if (!is.null(stations)) {
      codes <- vapply(csv_names, .extract_station_code, character(1))
      csv_names <- csv_names[!is.na(codes) & codes %in% stations]
      if (!length(csv_names)) {
        warning(sprintf("No matching stations found in year %d.", year), call. = FALSE)
        next
      }
    }

    chunks <- vector("list", length(csv_names))
    year_dir <- file.path(work_dir, as.character(year))
    dir.create(year_dir, recursive = TRUE)

    for (j in seq_along(csv_names)) {
      extracted <- tryCatch(
        utils::unzip(archive, files = csv_names[j], exdir = year_dir),
        error = function(e) {
          warning("Could not extract '", csv_names[j], "': ", conditionMessage(e),
                  call. = FALSE)
          character()
        }
      )
      if (!length(extracted)) next

      chunks[[j]] <- tryCatch(
        .parse_inmet_csv(
          extracted[[1]], tz = tz, variables = variables,
          dt_start = bounds$start, dt_end = bounds$end
        ),
        error = function(e) {
          warning("Could not parse '", csv_names[j], "': ", conditionMessage(e),
                  call. = FALSE)
          NULL
        }
      )
      unlink(extracted, recursive = TRUE, force = TRUE)
    }
    all_chunks[[k]] <- .safe_rbind(chunks)
  }

  out <- .safe_rbind(all_chunks)
  if (is.null(out)) {
    warning("No data was read. Returning empty data.frame.", call. = FALSE)
    return(data.frame())
  }

  id_cols <- .inmet_id_columns()
  data_cols <- setdiff(names(out), id_cols)
  if (!is.null(variables)) {
    unknown <- setdiff(variables, data_cols)
    if (length(unknown)) {
      warning("Unknown variables ignored: ", paste(unknown, collapse = ", "),
              call. = FALSE)
    }
    out <- out[, c(intersect(id_cols, names(out)), intersect(variables, data_cols)),
               drop = FALSE]
  }
  rownames(out) <- NULL
  out
}

#' Extract INMET archives
#'
#' @param years Integer years to extract.
#' @param dest_dir Directory containing annual ZIP files.
#' @param out_dir Destination directory.
#' @param overwrite Overwrite existing files.
#' @param quiet Suppress progress messages.
#' @return Invisibly, paths returned by [utils::unzip()].
#' @export
inmet_extract <- function(
    years,
    dest_dir = tools::R_user_dir("rmet", "cache"),
    out_dir = file.path(dest_dir, "csv"),
    overwrite = FALSE,
    quiet = FALSE
) {
  years <- .validate_years(years)
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  out_dir <- .validate_directory(out_dir, "out_dir")
  overwrite <- .validate_flag(overwrite, "overwrite")
  quiet <- .validate_flag(quiet, "quiet")
  if (!dir.exists(out_dir) &&
      !dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create `out_dir`: ", out_dir, call. = FALSE)
  }

  paths <- character()
  for (year in unique(years)) {
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
    if (!quiet) message("Extracting year ", year, " ...")
    paths <- c(paths, utils::unzip(
      archive, exdir = out_dir, overwrite = overwrite
    ))
  }
  invisible(paths)
}

.inmet_id_columns <- function() {
  c("station_code", "station_name", "region", "state", "latitude",
    "longitude", "elevation", "datetime")
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
    as.POSIXct(
      paste(x, if (end) "23:59:59" else "00:00:00"),
      tz = tz
    )
  }
  start <- parse_one(start_date, "start_date")
  end <- parse_one(end_date, "end_date", end = TRUE)
  if (!is.null(start) && !is.null(end) && start > end) {
    stop("`start_date` must not be after `end_date`.", call. = FALSE)
  }
  list(start = start, end = end)
}

.unsafe_archive_path <- function(x) {
  normalized <- gsub("\\\\", "/", x)
  grepl("^/|^[A-Za-z]:/|(^|/)\\.\\.(/|$)", normalized)
}

.extract_station_code <- function(filename) {
  name <- toupper(basename(filename))
  match <- regmatches(
    name,
    regexpr("(?<=_)[A-Z][A-Z0-9]{3,5}(?=_)", name, perl = TRUE)
  )
  if (!length(match)) NA_character_ else match[[1]]
}

.parse_inmet_csv <- function(path, tz, variables = NULL, dt_start = NULL, dt_end = NULL) {
  header <- readLines(path, n = 30L, encoding = "latin1", warn = FALSE)
  metadata <- .parse_inmet_header(header)
  if (is.na(metadata$code) || !nzchar(metadata$code)) {
    metadata$code <- .extract_station_code(basename(path))
  }
  normalized_header <- .normalize_text(header)
  candidates <- which(
    grepl("(^|;)DATA", normalized_header) & grepl("HORA", normalized_header)
  )
  if (!length(candidates)) stop("Could not locate the CSV column header.", call. = FALSE)
  data_start <- max(candidates)

  df <- utils::read.table(
    path,
    sep = ";",
    header = TRUE,
    skip = data_start - 1L,
    fileEncoding = "latin1",
    na.strings = c("", "NA", "-9999", "-9999.0", "-9999,0", "//"),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    fill = TRUE,
    quote = "",
    comment.char = ""
  )
  empty <- is.na(names(df)) | !nzchar(trimws(names(df))) |
    toupper(trimws(names(df))) %in% c("X", "X.")
  df <- df[, !empty, drop = FALSE]
  df <- .rename_columns(df)
  if (!all(c("date_raw", "hour_raw") %in% names(df))) {
    stop("Required date and hour columns were not recognized.", call. = FALSE)
  }

  measurement_cols <- setdiff(names(df), c("date_raw", "hour_raw"))
  if (length(measurement_cols)) {
    keep <- rowSums(!is.na(df[measurement_cols])) > 0L
    df <- df[keep, , drop = FALSE]
  }
  if (!nrow(df)) return(NULL)

  datetime <- .parse_inmet_datetime(df$date_raw, df$hour_raw, tz)
  invalid <- sum(is.na(datetime))
  if (invalid) warning(invalid, " row(s) have an invalid timestamp.", call. = FALSE)
  df$date_raw <- NULL
  df$hour_raw <- NULL
  df[] <- lapply(df, .parse_inmet_number)

  out <- data.frame(
    station_code = metadata$code,
    station_name = metadata$name,
    region = metadata$region,
    state = metadata$state,
    latitude = metadata$latitude,
    longitude = metadata$longitude,
    elevation = metadata$elevation,
    datetime = datetime,
    df,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if (!is.null(dt_start)) out <- out[!is.na(out$datetime) & out$datetime >= dt_start, , drop = FALSE]
  if (!is.null(dt_end)) out <- out[!is.na(out$datetime) & out$datetime <= dt_end, , drop = FALSE]
  if (!nrow(out)) return(NULL)

  if (!is.null(variables)) {
    keep <- c(.inmet_id_columns(), intersect(variables, names(out)))
    out <- out[, unique(keep), drop = FALSE]
  }
  out
}

.parse_inmet_header <- function(lines) {
  normalized <- .normalize_text(lines)
  grab <- function(pattern) {
    hit <- which(grepl(pattern, normalized, perl = TRUE))[1]
    if (is.na(hit)) return(NA_character_)
    parts <- strsplit(lines[[hit]], ";", fixed = TRUE)[[1]]
    values <- trimws(parts[nzchar(trimws(parts))])
    if (length(values) < 2L) return(NA_character_)
    values[[length(values)]]
  }
  number <- function(pattern) .parse_inmet_number(grab(pattern))
  list(
    region = grab("^REGI"),
    state = grab("^(UF|ESTADO)"),
    code = toupper(grab("(CODIGO|WMO)")),
    name = grab("^(ESTACAO|NOME)"),
    latitude = number("^LATITUDE"),
    longitude = number("^LONGITUDE"),
    elevation = number("^(ALTITUDE|ELEVACAO)")
  )
}

.normalize_text <- function(x) {
  out <- iconv(x, from = "", to = "ASCII//TRANSLIT", sub = "")
  out[is.na(out)] <- x[is.na(out)]
  toupper(trimws(out))
}

.rename_columns <- function(df) {
  normalized <- .normalize_text(names(df))
  classify <- function(x) {
    if (grepl("^DATA($| |\\()", x)) return("date_raw")
    if (grepl("^HORA", x)) return("hour_raw")
    if (grepl("PRECIPITA", x)) return("precip_mm")
    if (grepl("PRESS.*MAX", x)) return("pressure_max_hpa")
    if (grepl("PRESS.*MIN", x)) return("pressure_min_hpa")
    if (grepl("PRESS.*ESTAC", x)) return("pressure_station_hpa")
    if (grepl("RADIA", x)) return("radiation_kjm2")
    if (grepl("TEMP.*ORVALHO.*MAX", x)) return("temp_dew_max_c")
    if (grepl("TEMP.*ORVALHO.*MIN", x)) return("temp_dew_min_c")
    if (grepl("TEMP.*(PONTO.*ORVALHO|ORVALHO.*HOR)", x)) return("temp_dew_c")
    if (grepl("TEMP.*MAX.*HORA ANT", x)) return("temp_max_prev_c")
    if (grepl("TEMP.*MIN.*HORA ANT", x)) return("temp_min_prev_c")
    if (grepl("TEMP.*(BULBO|DO AR).*HOR", x) || grepl("TEMP.*BULBO", x)) return("temp_dry_c")
    if (grepl("TEMP.*MAX", x)) return("temp_max_c")
    if (grepl("TEMP.*MIN", x)) return("temp_min_c")
    if (grepl("UMIDADE.*MAX", x)) return("humid_rel_max_pct")
    if (grepl("UMIDADE.*MIN", x)) return("humid_rel_min_pct")
    if (grepl("UMIDADE.*HOR", x)) return("humid_rel_pct")
    if (grepl("VENTO.*DIRE", x)) return("wind_dir_deg")
    if (grepl("VENTO.*RAJADA", x)) return("wind_gust_ms")
    if (grepl("VENTO.*VELOC", x)) return("wind_speed_ms")
    cleaned <- tolower(gsub("[^A-Z0-9]+", "_", x))
    gsub("^_|_$", "", cleaned)
  }
  names(df) <- make.unique(vapply(normalized, classify, character(1)), sep = "_")
  df
}

.parse_inmet_number <- function(x) {
  if (is.numeric(x)) return(x)
  x <- trimws(as.character(x))
  x <- gsub(",", ".", x, fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

.parse_inmet_datetime <- function(date, hour, tz) {
  date <- trimws(as.character(date))
  hour <- toupper(trimws(as.character(hour)))
  hour <- trimws(gsub("UTC", "", hour, fixed = TRUE))
  compact <- gsub("[^0-9]", "", hour)
  short <- !is.na(compact) & nzchar(compact) & nchar(compact) < 4L
  compact[short] <- sprintf("%04d", suppressWarnings(as.integer(compact[short])))
  next_day <- compact == "2400"
  next_day[is.na(next_day)] <- FALSE
  compact[next_day] <- "0000"
  formatted_hour <- ifelse(
    nchar(compact) == 4L,
    paste0(substr(compact, 1, 2), ":", substr(compact, 3, 4)),
    NA_character_
  )
  value <- as.POSIXct(rep(NA_character_, length(date)), tz = "UTC")
  text <- paste(date, formatted_hour)
  formats <- c("%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%d/%m/%Y %H:%M",
               "%d-%m-%Y %H:%M")
  for (format in formats) {
    missing <- is.na(value)
    if (!any(missing)) break
    value[missing] <- as.POSIXct(text[missing], format = format, tz = "UTC")
  }
  value[!is.na(value) & next_day] <- value[!is.na(value) & next_day] + 86400
  attr(value, "tzone") <- tz
  value
}

.convert_tz <- function(x, tz) {
  tz <- .validate_timezone(tz)
  attr(x, "tzone") <- tz
  x
}
