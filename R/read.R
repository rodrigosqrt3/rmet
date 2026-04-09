#' Read INMET station data from downloaded ZIP files
#'
#' Parses the CSV files inside INMET annual ZIP archives and returns a single
#' tidy `data.frame` with consistent column names, correct data types, and a
#' proper `POSIXct` timestamp column.
#'
#' @param years Integer vector of years to read. Each year must have a
#'   corresponding ZIP file in `dest_dir` (downloaded with [inmet_download()]).
#' @param stations Character vector of station codes (e.g., `"A001"`,
#'   `"A652"`). If `NULL` (default), all stations are returned.
#' @param dest_dir Character. Directory containing the ZIP files. Defaults to
#'   the same cache directory used by [inmet_download()].
#' @param tz Character. Time zone for the `datetime` column. INMET data uses
#'   UTC-3 (Brasília standard time). Defaults to `"America/Sao_Paulo"`.
#' @param variables Character vector of variable names to keep. If `NULL`
#'   (default), all variables are returned. See **Variables** section below.
#' @param start_date Character. Optional start date in `"YYYY-MM-DD"` format.
#' @param end_date Character. Optional end date in `"YYYY-MM-DD"` format.
#' @param quiet Logical. Suppress progress messages. Defaults to `FALSE`.
#'
#' @return A \code{data.frame} containing the parsed INMET data.
#'   Columns include station identifiers (\code{station_code}, \code{region},
#'   \code{state}, \code{latitude}, \code{longitude}, \code{elevation}),
#'   a \code{POSIXct} \code{datetime} column, and various meteorological
#'   measurements (temperature, precipitation, pressure, humidity, wind, and radiation).
#'   Returns an empty \code{data.frame} if no data is found.
#'
#' @section Variables:
#' You can pass a subset of the output column names to `variables` to limit
#' what is returned, e.g.:
#' `variables = c("temp_dry_c", "precip_mm", "humid_rel_pct")`.
#'
#' @examples
#' \donttest{
#' df <- inmet_read(
#'   years = 2000,
#'   stations = "A801",
#'   dest_dir = tempdir()
#' )
#' head(df)
#' }
#' @seealso [inmet_download()], [inmet_stations()]
#' @export
inmet_read <- function(
    years,
    stations   = NULL,
    dest_dir   = tools::R_user_dir("rmet", "cache"),
    tz         = "America/Sao_Paulo",
    variables  = NULL,
    start_date = NULL,
    end_date   = NULL,
    quiet      = FALSE
) {
  years <- as.integer(years)
  if (any(is.na(years))) stop("`years` must be a vector of integers.", call. = FALSE)

  all_chunks <- vector("list", length(years))

  for (k in seq_along(years)) {
    yr  <- years[k]
    zip <- file.path(dest_dir, paste0(yr, ".zip"))

    if (!file.exists(zip)) {
      stop(
        sprintf("ZIP for year %d not found at '%s'. Run inmet_download(%d) first.", yr, zip, yr),
        call. = FALSE
      )
    }

    if (!quiet) message("Reading year ", yr, " ...")

    entries <- tryCatch(
      utils::unzip(zip, list = TRUE),
      error = function(e) stop(sprintf("Cannot open ZIP for %d: %s", yr, conditionMessage(e)), call. = FALSE)
    )

    csv_names <- entries$Name[grepl("\\.CSV$", entries$Name, ignore.case = TRUE) &
                                !grepl("^__", entries$Name)]
    if (!is.null(stations)) {
      stations_up <- toupper(stations)
      keep <- vapply(csv_names, function(nm) {
        code <- .extract_station_code(nm)
        !is.na(code) && code %in% stations_up
      }, logical(1))
      csv_names <- csv_names[keep]
      if (length(csv_names) == 0L) {
        warning(sprintf("No matching stations found in year %d.", yr), call. = FALSE)
        next
      }
    }

    chunks <- vector("list", length(csv_names))
    for (j in seq_along(csv_names)) {
      raw <- tryCatch(
        utils::unzip(zip, files = csv_names[j], exdir = tempdir()),
        error = function(e) {
          warning(sprintf("Could not extract '%s': %s", csv_names[j], conditionMessage(e)), call. = FALSE)
          NULL
        }
      )
      if (is.null(raw)) next
      extracted_path <- file.path(tempdir(), csv_names[j])
      chunks[[j]] <- tryCatch(
        .parse_inmet_csv(extracted_path, tz = tz),
        error = function(e) {
          warning(sprintf("Could not parse '%s': %s", csv_names[j], conditionMessage(e)), call. = FALSE)
          NULL
        }
      )
      # clean up extracted temp file
      try(file.remove(extracted_path), silent = TRUE)
    }

    all_chunks[[k]] <- .safe_rbind(Filter(Negate(is.null), chunks))
  }

  out <- .safe_rbind(Filter(Negate(is.null), all_chunks))

  if (is.null(out) || nrow(out) == 0L) {
    warning("No data was read. Returning empty data.frame.", call. = FALSE)
    return(data.frame())
  }

  # optional date filtering
  if (!is.null(start_date)) {
    dt_start <- as.POSIXct(paste(start_date, "00:00:00"), tz = tz)
    out <- out[which(out$datetime >= dt_start), , drop = FALSE]
  }

  if (!is.null(end_date)) {
    dt_end <- as.POSIXct(paste(end_date, "23:59:59"), tz = tz)
    out <- out[which(out$datetime <= dt_end), , drop = FALSE]
  }

  if (nrow(out) == 0L) {
    warning("No data left after applying start_date / end_date filters.", call. = FALSE)
    return(data.frame())
  }

  # optional variable subsetting (always keep identifier + datetime cols)
  id_cols  <- c("station_code", "station_name", "region", "state",
                "latitude", "longitude", "elevation", "datetime")
  data_cols <- setdiff(names(out), id_cols)

  if (!is.null(variables)) {
    bad <- setdiff(variables, data_cols)
    if (length(bad)) warning("Unknown variables ignored: ", paste(bad, collapse = ", "), call. = FALSE)
    data_cols <- intersect(variables, data_cols)
    out <- out[, c(id_cols, data_cols), drop = FALSE]
  }

  out
}


#' Extract and save INMET ZIP contents to a directory
#'
#' Unzips one or more downloaded INMET annual ZIPs to a target directory,
#' preserving the original CSV files.
#'
#' @param years Integer vector of years to extract.
#' @param dest_dir Character. Directory of ZIP files (same as [inmet_download()]).
#' @param out_dir Character. Directory where CSVs will be written. Defaults to
#'   a subdirectory `csv/` inside `dest_dir`.
#' @param overwrite Logical. Overwrite existing CSVs? Defaults to `FALSE`.
#' @param quiet Logical. Suppress messages. Defaults to `FALSE`.
#'
#' @return Invisible character vector of paths to all extracted CSV files.
#' @export
inmet_extract <- function(
    years,
    dest_dir  = tools::R_user_dir("rmet", "cache"),
    out_dir   = file.path(dest_dir, "csv"),
    overwrite = FALSE,
    quiet     = FALSE
) {
  years <- as.integer(years)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  all_paths <- character(0)

  for (yr in years) {
    zip <- file.path(dest_dir, paste0(yr, ".zip"))
    if (!file.exists(zip)) {
      warning(sprintf("ZIP for year %d not found; skipping.", yr), call. = FALSE)
      next
    }
    if (!quiet) message("Extracting year ", yr, " ...")
    extracted <- utils::unzip(zip, exdir = out_dir, overwrite = overwrite)
    all_paths <- c(all_paths, extracted)
  }

  invisible(all_paths)
}


# ==============================================================================
# Internal helpers
# ==============================================================================

# Extract station code like "A001" from a CSV filename
.extract_station_code <- function(filename) {
  bn  <- basename(filename)
  # Pattern: INMET_<2>_<2>_<CODE>_...
  m <- regmatches(bn, regexpr("(?<=_)[A-Z]\\d{3}(?=_)", bn, perl = TRUE))
  if (length(m) == 0L) NA_character_ else m[[1L]]
}

# Parse a single INMET CSV file -----------------------------------------------
.parse_inmet_csv <- function(path, tz) {
  raw_header <- readLines(path, n = 10L, encoding = "latin1", warn = FALSE)
  meta <- .parse_inmet_header(raw_header)

  data_start <- which(grepl("Data", raw_header, ignore.case = TRUE) &
                        grepl("Hora|Precipita", raw_header, ignore.case = TRUE))
  if (length(data_start) == 0L) data_start <- 9L
  data_start <- max(data_start)

  # ---- detect format by date column name ------------------------------------
  header_line <- raw_header[data_start]

  df <- utils::read.table(
    path,
    sep              = ";",
    header           = TRUE,
    skip             = data_start - 1L,
    fileEncoding     = "latin1",
    na.strings       = c("", "NA", "-9999", "-9999.0", "//"),
    check.names      = FALSE,
    stringsAsFactors = FALSE,
    fill             = TRUE,
    quote            = ""
  )

  empty_cols <- names(df) == "" | is.na(names(df)) | toupper(names(df)) == "X"
  df <- df[, !empty_cols, drop = FALSE]
  df <- df[rowSums(!is.na(df[, -c(1, 2), drop = FALSE])) > 0L, , drop = FALSE]
  if (nrow(df) == 0L) return(NULL)

  df <- .rename_columns(df)

  # ---- build datetime depending on format ------------------------------------
  dt_str <- paste(df[["date_raw"]], df[["hour_raw"]])

  # 1. Clean up INMET's messy hour text (e.g., "1200 UTC" -> "1200")
  dt_str <- sub(" UTC", "", dt_str, ignore.case = TRUE)
  # 2. Add colon if it is missing (e.g., " 1200" -> " 12:00")
  dt_str <- sub(" (\\d{2})(\\d{2})$", " \\1:\\2", dt_str)

  # 3. Iteratively test date formats (patches NAs as it goes)
  datetime <- as.POSIXct(dt_str, format = "%Y-%m-%d %H:%M", tz = "UTC")

  idx_na <- is.na(datetime)
  if (any(idx_na)) {
    datetime[idx_na] <- as.POSIXct(dt_str[idx_na], format = "%Y/%m/%d %H:%M", tz = "UTC")
  }

  idx_na <- is.na(datetime)
  if (any(idx_na)) {
    datetime[idx_na] <- as.POSIXct(dt_str[idx_na], format = "%d/%m/%Y %H:%M", tz = "UTC")
  }

  datetime <- .convert_tz(datetime, tz)
  df[["date_raw"]] <- NULL
  df[["hour_raw"]] <- NULL

  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      x <- gsub(",", ".", x, fixed = TRUE)
      suppressWarnings(as.numeric(x))
    } else x
  })

  data.frame(
    station_code = meta$code,
    station_name = meta$name,
    region       = meta$region,
    state        = meta$state,
    latitude     = meta$latitude,
    longitude    = meta$longitude,
    elevation    = meta$elevation,
    datetime     = datetime,
    df,
    stringsAsFactors = FALSE,
    check.names  = FALSE
  )
}


# Parse the metadata block at the top of each INMET CSV
.parse_inmet_header <- function(lines) {
  .grab <- function(pattern) {
    hit <- lines[grepl(pattern, lines, ignore.case = TRUE)]
    if (length(hit) == 0L) return(NA_character_)
    parts <- strsplit(hit[[1L]], ";")[[1L]]
    trimws(parts[length(parts)])
  }
  .grab_num <- function(pattern) {
    val <- .grab(pattern)
    val <- gsub(",", ".", val, fixed = TRUE)
    suppressWarnings(as.numeric(val))
  }

  list(
    region    = .grab("REGI"),
    state     = .grab("UF|ESTADO"),
    code      = .grab("C.DIGO|CODIGO|WMO"),
    name      = .grab("ESTA..O|ESTACAO|NOME"),
    latitude  = .grab_num("LATITUDE"),
    longitude = .grab_num("LONGITUDE"),
    elevation = .grab_num("ALTITUDE|ELEVA")
  )
}


# Map INMET raw Portuguese column names to clean English names
.rename_columns <- function(df) {
  raw <- names(df)

  mapping <- c(
    # Date / time — both formats
    "DATA \\(YYYY"                                   = "date_raw",
    "^Data$"                                         = "date_raw",
    "HORA \\(UTC\\)"                                 = "hour_raw",
    "^Hora UTC$"                                     = "hour_raw",
    # Precipitation
    "PRECIPITA"                                      = "precip_mm",
    # Pressure (mB and hPa both)
    "PRESS.+ESTAC.+(mB|hPa)"                         = "pressure_station_hpa",
    "PRESS.+MAX.+(mB|hPa)"                           = "pressure_max_hpa",
    "PRESS.+MIN.+(mB|hPa)"                           = "pressure_min_hpa",
    # Radiation
    "RADIA"                                          = "radiation_kjm2",
    # Temperature
    "TEMP.+BULBO"                                    = "temp_dry_c",
    "TEMP.+ORVALHO MAX"                              = "temp_dew_max_c",
    "TEMP.+ORVALHO MIN"                              = "temp_dew_min_c",
    "TEMP.+PONTO.+ORVALHO"                           = "temp_dew_c",
    "TEMP.+M.X.+HORA ANT"                           = "temp_max_prev_c",
    "TEMP.+M.N.+HORA ANT"                           = "temp_min_prev_c",
    "TEMP.+MAX"                                      = "temp_max_c",
    "TEMP.+MIN"                                      = "temp_min_c",
    # Humidity — order matters: MAX and MIN before plain
    "UMIDADE.+MAX"                                   = "humid_rel_max_pct",
    "UMIDADE.+MIN"                                   = "humid_rel_min_pct",
    "UMIDADE.+HORARIA"                               = "humid_rel_pct",
    # Wind
    "VENTO.+DIRE"                                    = "wind_dir_deg",
    "VENTO.+RAJADA"                                  = "wind_gust_ms",
    "VENTO.+VELOCIDADE"                              = "wind_speed_ms"
  )

  new_names <- raw
  for (pattern in names(mapping)) {
    hits <- grepl(pattern, raw, ignore.case = TRUE, perl = TRUE)
    new_names[hits] <- mapping[[pattern]]
  }

  names(df) <- new_names
  df
}

.convert_tz <- function(x, tz) {
  if (!(tz %in% OlsonNames())) {
    warning("Unknown timezone '", tz, "'; keeping UTC.", call. = FALSE)
    return(x)
  }

  attr(x, "tzone") <- tz
  x
}

.safe_rbind <- function(list_df) {
  if (length(list_df) == 0L) return(NULL)
  all_cols <- unique(unlist(lapply(list_df, names)))
  list_df <- lapply(list_df, function(d) {
    missing_cols <- setdiff(all_cols, names(d))
    if (length(missing_cols) > 0L) {
      d[missing_cols] <- NA
    }
    d[all_cols]
  })
  do.call(rbind, list_df)
}
