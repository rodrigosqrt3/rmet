# Parsing of individual INMET station files.
#
# Each annual archive holds one semicolon-separated CSV per station. A file has
# a short metadata block (region, state, station name, code, coordinates)
# followed by a column header line starting with the date and hour columns and
# then hourly rows. Formats differ across years:
#
# * dates as `YYYY-MM-DD`, `YYYY/MM/DD`, `DD/MM/YYYY`, or `DD-MM-YYYY`;
# * hours as `HH:MM`, `HHMM`, or `HHMM UTC` (including `2400 UTC`);
# * Latin-1 or UTF-8 encoding, and some files with characters already replaced
#   by `?` (for example `ESTAC?O:` in 2019);
# * decimal commas, `-9999` or empty fields for missing values, and a trailing
#   `;` that creates an empty phantom column.
#
# The parser reads the file once as text, splits fields with `strsplit()` and
# converts every measurement column in a single vectorised step.

# Lines of one archive entry, read straight from the ZIP. If the entry cannot
# be opened as a connection (e.g. unusual file names) it is extracted to
# `work_dir` instead.
.read_entry_lines <- function(archive, entry, work_dir) {
  con <- tryCatch(unz(archive, entry), error = function(e) NULL)
  lines <- if (!is.null(con)) tryCatch(.read_connection_lines(con), error = function(e) NULL)
  if (!is.null(lines)) return(lines)

  extracted <- utils::unzip(archive, files = entry, exdir = work_dir)
  if (!length(extracted)) stop("entry could not be extracted", call. = FALSE)
  on.exit(unlink(extracted, force = TRUE), add = TRUE)
  .read_connection_lines(file(extracted[[1]]))
}

.read_connection_lines <- function(con) {
  on.exit(try(close(con), silent = TRUE), add = TRUE)
  open(con, "rb")
  .decode_lines(readLines(con, warn = FALSE))
}

# Files are mostly Latin-1, but some are UTF-8. Lines that are valid UTF-8
# (which includes plain ASCII, i.e. all data rows) are kept; the others are
# converted from Latin-1. In practice only a few header lines need converting.
.decode_lines <- function(lines) {
  if (!length(lines)) return(lines)
  latin1 <- !validUTF8(lines)
  Encoding(lines) <- "UTF-8"
  if (any(latin1)) lines[latin1] <- iconv(lines[latin1], from = "latin1", to = "UTF-8")
  lines[1L] <- sub("^\ufeff", "", lines[1L])
  lines
}

# Reads and parses a CSV file on disk. Kept for direct use on extracted files.
.parse_inmet_csv <- function(path, tz, variables = NULL, dt_start = NULL,
                             dt_end = NULL, drop_empty = TRUE) {
  .parse_inmet_lines(
    .read_connection_lines(file(path)),
    tz = tz, variables = variables, dt_start = dt_start, dt_end = dt_end,
    drop_empty = drop_empty, fallback_code = .extract_station_code(basename(path))
  )
}

.parse_inmet_lines <- function(lines, tz, variables = NULL, dt_start = NULL,
                               dt_end = NULL, drop_empty = TRUE,
                               fallback_code = NA_character_) {
  header_row <- .find_column_header(lines)
  metadata <- .parse_inmet_header(lines[seq_len(header_row - 1L)])
  if (is.na(metadata$code) || !nzchar(metadata$code)) metadata$code <- fallback_code

  raw_names <- trimws(strsplit(lines[[header_row]], ";", fixed = TRUE)[[1]])
  keep <- which(nzchar(raw_names) & !toupper(raw_names) %in% c("X", "X."))
  col_names <- .canonical_names(raw_names[keep])
  date_col <- match("date_raw", col_names)
  hour_col <- match("hour_raw", col_names)
  if (anyNA(c(date_col, hour_col))) {
    stop("Required date and hour columns were not recognized.", call. = FALSE)
  }

  body <- lines[-seq_len(header_row)]
  body <- body[grepl(";", body, fixed = TRUE)]
  if (!length(body)) return(NULL)
  # Commas only appear as decimal marks (fields are separated by ";" and dates
  # use "-" or "/"), so they are converted once per line, not once per cell.
  cells <- .split_fields(chartr(",", ".", body), length(raw_names))[keep, , drop = FALSE]

  measure_cols <- setdiff(seq_along(col_names), c(date_col, hour_col))
  values <- .parse_number_matrix(cells[measure_cols, , drop = FALSE])

  rows <- seq_len(ncol(cells))
  if (drop_empty && length(measure_cols)) {
    rows <- which(colSums(!is.na(values)) > 0L)
    if (!length(rows)) return(NULL)
  }

  datetime <- .parse_inmet_datetime(cells[date_col, rows], cells[hour_col, rows], tz)
  invalid <- sum(is.na(datetime))
  if (invalid) warning(invalid, " row(s) have an invalid timestamp.", call. = FALSE)

  in_bounds <- rep(TRUE, length(rows))
  if (!is.null(dt_start)) in_bounds <- in_bounds & !is.na(datetime) & datetime >= dt_start
  if (!is.null(dt_end)) in_bounds <- in_bounds & !is.na(datetime) & datetime <= dt_end
  if (!any(in_bounds)) return(NULL)
  rows <- rows[in_bounds]
  datetime <- datetime[in_bounds]

  n <- length(rows)
  columns <- list(
    station_code = rep(metadata$code, n),
    station_name = rep(metadata$name, n),
    region = rep(metadata$region, n),
    state = rep(metadata$state, n),
    latitude = rep(metadata$latitude, n),
    longitude = rep(metadata$longitude, n),
    elevation = rep(metadata$elevation, n),
    datetime = datetime
  )
  measure_names <- col_names[measure_cols]
  for (j in seq_along(measure_cols)) {
    if (is.null(variables) || measure_names[[j]] %in% variables) {
      columns[[measure_names[[j]]]] <- values[j, rows]
    }
  }
  .new_data_frame(columns, n)
}

.find_column_header <- function(lines) {
  scan <- .normalize_text(lines[seq_len(min(length(lines), 30L))])
  hits <- which(grepl("(^|;)DATA", scan) & grepl("HORA", scan))
  if (!length(hits)) stop("Could not locate the CSV column header.", call. = FALSE)
  max(hits)
}

# Splits semicolon-separated lines into a character matrix with one column per
# row of data. Short rows are padded with NA and long rows truncated.
.split_fields <- function(body, n_fields) {
  fields <- strsplit(body, ";", fixed = TRUE)
  if (all(lengths(fields) == n_fields)) {
    return(matrix(unlist(fields, use.names = FALSE), nrow = n_fields))
  }
  matrix(
    vapply(fields, function(f) { length(f) <- n_fields; f }, character(n_fields)),
    nrow = n_fields
  )
}

# Character matrix (decimal points) -> numeric matrix. Empty strings, "//" and
# the -9999 sentinel become NA. as.numeric() ignores surrounding blanks.
.parse_number_matrix <- function(x) {
  values <- suppressWarnings(as.numeric(x))
  values[!is.na(values) & values == -9999] <- NA_real_
  dim(values) <- dim(x)
  values
}

.parse_inmet_number <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(chartr(",", ".", trimws(as.character(x)))))
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
    state = grab("^(UF|ESTADO)\\s*:"),
    code = toupper(grab("(CODIGO|WMO)")),
    # "ESTACAO:", or the damaged spellings found in some years ("ESTAC?O:")
    name = grab("^(ESTA(CAO|C\\?O|\\?AO|\\?\\?O)|NOME)\\s*:"),
    latitude = number("^LATITUDE"),
    longitude = number("^LONGITUDE"),
    elevation = number("^(ALTITUDE|ELEVACAO)")
  )
}

# Upper-case ASCII version of `x`, used for accent-insensitive matching.
# iconv() transliteration differs across operating systems (macOS, for one,
# handles Portuguese nasal vowels inconsistently), so Portuguese letters are
# translated explicitly first. Unicode escapes keep this source file ASCII.
.normalize_text <- function(x) {
  out <- enc2utf8(as.character(x))
  out <- chartr(
    "\u00c1\u00c0\u00c2\u00c3\u00c4\u00e1\u00e0\u00e2\u00e3\u00e4",
    "AAAAAaaaaa", out
  )
  out <- chartr("\u00c9\u00c8\u00ca\u00cb\u00e9\u00e8\u00ea\u00eb", "EEEEeeee", out)
  out <- chartr("\u00cd\u00cc\u00ce\u00cf\u00ed\u00ec\u00ee\u00ef", "IIIIiiii", out)
  out <- chartr(
    "\u00d3\u00d2\u00d4\u00d5\u00d6\u00f3\u00f2\u00f4\u00f5\u00f6",
    "OOOOOooooo", out
  )
  out <- chartr("\u00da\u00d9\u00db\u00dc\u00fa\u00f9\u00fb\u00fc", "UUUUuuuu", out)
  out <- chartr("\u00c7\u00e7\u00d1\u00f1", "CcNn", out)
  ascii <- iconv(out, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  ascii[is.na(ascii)] <- out[is.na(ascii)]
  toupper(trimws(ascii))
}

# Maps the original Portuguese column titles to stable snake_case names.
# Order matters: specific patterns (dew-point max/min) must precede generic ones.
.canonical_names <- function(x) {
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
  make.unique(vapply(.normalize_text(x), classify, character(1), USE.NAMES = FALSE), sep = "_")
}

.rename_columns <- function(df) {
  names(df) <- .canonical_names(names(df))
  df
}

# UTC timestamps from the date and hour columns. Dates are parsed once per
# distinct value (a file has 24 rows per date) with the format chosen by an
# exact pattern, never by trial and error: strptime() accepts one- to
# four-digit years, so trying "%Y-%m-%d" on "04-01-2020" would yield year 4.
# Hours are added arithmetically, which also maps "2400" to 00:00 next day.
.parse_inmet_datetime <- function(date, hour, tz) {
  date <- trimws(as.character(date))
  unique_dates <- unique(date)
  parsed <- rep(as.Date(NA), length(unique_dates))
  patterns <- c(
    "%Y-%m-%d" = "^\\d{4}-\\d{2}-\\d{2}$",
    "%Y/%m/%d" = "^\\d{4}/\\d{2}/\\d{2}$",
    "%d/%m/%Y" = "^\\d{2}/\\d{2}/\\d{4}$",
    "%d-%m-%Y" = "^\\d{2}-\\d{2}-\\d{4}$"
  )
  for (format in names(patterns)) {
    hit <- !is.na(unique_dates) & grepl(patterns[[format]], unique_dates)
    if (any(hit)) parsed[hit] <- as.Date(unique_dates[hit], format = format)
  }
  day <- parsed[match(date, unique_dates)]

  digits <- gsub("[^0-9]", "", as.character(hour))
  digits[!is.na(digits) & nzchar(digits) & nchar(digits) < 4L] <-
    sprintf("%04d", suppressWarnings(as.integer(digits[!is.na(digits) & nzchar(digits) & nchar(digits) < 4L])))
  hh <- suppressWarnings(as.integer(substr(digits, 1L, 2L)))
  mm <- suppressWarnings(as.integer(substr(digits, 3L, 4L)))
  valid_time <- !is.na(hh) & !is.na(mm) & nchar(digits) == 4L & mm < 60L &
    (hh < 24L | (hh == 24L & mm == 0L))
  seconds <- ifelse(valid_time, hh * 3600 + mm * 60, NA_real_)

  .POSIXct(as.numeric(day) * 86400 + seconds, tz = tz)
}

.convert_tz <- function(x, tz) {
  attr(x, "tzone") <- .validate_timezone(tz)
  x
}

.extract_station_code <- function(filename) {
  name <- toupper(basename(filename))
  match <- regmatches(name, regexpr("(?<=_)[A-Z][A-Z0-9]{3,5}(?=_)", name, perl = TRUE))
  if (!length(match)) NA_character_ else match[[1]]
}

.unsafe_archive_path <- function(x) {
  grepl("^/|^[A-Za-z]:/|(^|/)\\.\\.(/|$)", gsub("\\\\", "/", x))
}

.inmet_id_columns <- function() {
  c("station_code", "station_name", "region", "state", "latitude",
    "longitude", "elevation", "datetime")
}

# A data frame from a list of equal-length columns, without the copying and
# checks of data.frame().
.new_data_frame <- function(columns, n) {
  structure(columns, class = "data.frame", row.names = c(NA_integer_, -as.integer(n)))
}

# Row-binds data frames that may have different columns, filling gaps with NA.
# Much faster than do.call(rbind, ...) for hundreds of station files.
.safe_rbind <- function(list_df) {
  list_df <- Filter(function(x) !is.null(x) && nrow(x) > 0L, list_df)
  if (!length(list_df)) return(NULL)
  sizes <- vapply(list_df, nrow, integer(1))
  all_cols <- unique(unlist(lapply(list_df, names), use.names = FALSE))
  columns <- lapply(all_cols, function(col) {
    pieces <- lapply(seq_along(list_df), function(i) {
      d <- list_df[[i]]
      if (col %in% names(d)) d[[col]] else rep(NA, sizes[[i]])
    })
    template <- Find(function(p) inherits(p, c("POSIXct", "Date")), pieces)
    out <- unlist(lapply(pieces, unclass), use.names = FALSE)
    if (inherits(template, "POSIXct")) {
      out <- .POSIXct(as.numeric(out), tz = attr(template, "tzone"))
    } else if (inherits(template, "Date")) {
      out <- structure(as.numeric(out), class = "Date")
    }
    out
  })
  names(columns) <- all_cols
  .new_data_frame(columns, sum(sizes))
}
