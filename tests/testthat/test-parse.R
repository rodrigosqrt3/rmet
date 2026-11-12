test_that("2019-layout Latin-1 files are parsed into canonical columns", {
  td <- file.path(tempdir(), "rmet_parse_2019")
  create_mock_zip(td, inmet_lines_2019())
  df <- inmet_read(2020, dest_dir = td, quiet = TRUE)

  expect_equal(df$station_name[[1]], "PORTO ALEGRE")
  expect_equal(df$region[[1]], "S")
  expect_equal(df$latitude[[1]], -30.053536)
  expect_equal(
    names(df)[-(1:8)],
    c("precip_mm", "temp_dry_c", "temp_max_prev_c", "temp_dew_max_c")
  )
  # the all-missing 01:00 row is dropped; -9999 and -9999,0 become NA
  expect_equal(format(df$datetime, "%H:%M"), c("00:00", "02:00", "03:00"))
  expect_equal(df$temp_dry_c, c(25.8, NA, 24.1))
  expect_equal(df$temp_dew_max_c, c(23.4, NA, NA))
  expect_equal(df$precip_mm, c(0, 1.2, 0))
  expect_true(is.double(df$precip_mm))
})

test_that("damaged station headers ('ESTAC?O') still give the station name", {
  # INMET's 2019 files literally contain "ESTAC?O:" (rmet 0.2.0 returned NA)
  td <- file.path(tempdir(), "rmet_parse_damaged")
  create_mock_zip(td, inmet_lines_2019(station_line = "ESTAC?O:;PORTO ALEGRE"))
  df <- inmet_read(2020, dest_dir = td, quiet = TRUE)
  expect_equal(unique(df$station_name), "PORTO ALEGRE")
})

test_that("UTF-8 encoded files are read as well as Latin-1 ones", {
  td_utf8 <- file.path(tempdir(), "rmet_parse_utf8")
  td_latin1 <- file.path(tempdir(), "rmet_parse_latin1")
  create_mock_zip(td_utf8, inmet_lines_2019(), encoding = "UTF-8")
  create_mock_zip(td_latin1, inmet_lines_2019(), encoding = "latin1")
  expect_identical(
    inmet_read(2020, dest_dir = td_utf8, quiet = TRUE),
    inmet_read(2020, dest_dir = td_latin1, quiet = TRUE)
  )
})

test_that("drop_empty = FALSE keeps hours without measurements", {
  td <- file.path(tempdir(), "rmet_parse_keep_empty")
  create_mock_zip(td, inmet_lines_2019())
  df <- inmet_read(2020, dest_dir = td, drop_empty = FALSE, quiet = TRUE)
  expect_equal(nrow(df), 4L)
  expect_true(all(is.na(df[2, c("precip_mm", "temp_dry_c")])))
  expect_error(inmet_read(2020, dest_dir = td, drop_empty = NA), "TRUE or FALSE")
})

test_that("variables keep only the requested measurements", {
  td <- file.path(tempdir(), "rmet_parse_vars")
  create_mock_zip(td, inmet_lines_2019())
  df <- inmet_read(2020, dest_dir = td, variables = "temp_dry_c", quiet = TRUE)
  expect_equal(names(df), c(rmet:::.inmet_id_columns(), "temp_dry_c"))
})

test_that("ambiguous day-first dates are never read as year-first", {
  values <- parse_fixture(
    c("04-01-2020", "2020-01-04", "31/12/2019", "2019/12/31", "2020-13-01", "garbage"),
    c("0000", "0000", "2300", "2300", "0000", "0000")
  )$datetime
  expect_equal(
    format(values, "%Y-%m-%d %H:%M"),
    c("2020-01-04 00:00", "2020-01-04 00:00", "2019-12-31 23:00",
      "2019-12-31 23:00", NA, NA)
  )
})

test_that("only real calendar dates with consistent separators are accepted", {
  values <- parse_fixture(
    c("2020-02-30", "29/02/2020", "29/02/2019", "2020-01/04", "2020/01-04",
      " 2020-01-04 ", "2020-1-4", "20-01-2020x", "0/01/2020", "2020-00-10", "2020-01-00"),
    "0000"
  )$datetime
  expect_equal(
    format(values, "%Y-%m-%d"),
    c(NA, "2020-02-29", NA, NA, NA, "2020-01-04", NA, NA, NA, NA, NA)
  )
})

test_that("invalid hours give NA timestamps", {
  values <- parse_fixture(
    rep("2020-01-01", 8), c("2500", "1260", "2401", "", "12:30", "12345", "UTC", "5")
  )$datetime
  expect_equal(format(values, "%H:%M"), c(NA, NA, NA, NA, "12:30", NA, NA, "00:05"))
})

test_that("timestamps are UTC instants displayed in the requested zone", {
  td <- file.path(tempdir(), "rmet_parse_tz")
  create_mock_zip(td, inmet_lines_2019())
  utc <- inmet_read(2020, dest_dir = td, quiet = TRUE)
  local <- inmet_read(2020, dest_dir = td, tz = "America/Sao_Paulo", quiet = TRUE)
  expect_equal(as.numeric(utc$datetime), as.numeric(local$datetime))
  expect_equal(format(local$datetime[[1]]), "2018-12-31 22:00:00")
})

test_that("ragged rows are padded or truncated", {
  lines <- c(
    "Data;Hora UTC;PRECIPITACAO TOTAL;PRESSAO ESTACAO;TEMP BULBO SECO;",
    "2020/01/01;0000;1,5",                       # short row
    "2020/01/01;0100;2;3;4;99;extra",            # long row
    "2020/01/01;0200;;;;",                       # nothing measured
    "a row without separators",
    ""
  )
  kept <- rmet:::.parse_inmet_lines(lines, "UTC", fallback_code = "A001")
  expect_equal(kept$precip_mm, c(1.5, 2))
  expect_equal(kept$pressure_station_hpa, c(NA, 3))
  expect_equal(kept$temp_dry_c, c(NA, 4))
  all_rows <- rmet:::.parse_inmet_lines(lines, "UTC", drop_empty = FALSE, fallback_code = "A001")
  expect_equal(nrow(all_rows), 3L)
  expect_true(all(is.na(all_rows[3, c("precip_mm", "pressure_station_hpa", "temp_dry_c")])))
})

test_that("numbers are read like as.numeric() after decimal commas become points", {
  text <- c("1,5", "-9999", " 7 ", "1e2", "abc", "NA", "1.2.3", "-", ".", "+3,25",
            "123456789012345678", strrep("1", 70), "0,000", "-0,5", "Inf", "",
            "-9999,0", "1,5,5", "12 3", "0.1", "007", "1E-3")
  values <- parse_fixture(rep("2020-01-01", length(text)), "0000", measure = text)
  expected <- suppressWarnings(as.numeric(chartr(",", ".", text)))
  expected[!is.na(expected) & expected == -9999] <- NA
  expected[nchar(text) >= 64] <- NA      # longer than the buffer of the fast reader
  # rows where every measurement is missing are dropped
  expect_equal(values$precip_mm, expected[!is.na(expected)])
})

test_that("files with CRLF line endings, no final newline and a BOM are read", {
  rows <- c("﻿REGIAO:;S", "UF:;RS", "ESTACAO:;TESTE", "CODIGO (WMO):;A001",
            "LATITUDE:;-30,0", "LONGITUDE:;-51,0", "ALTITUDE:;10",
            "Data;Hora UTC;PRECIPITACAO TOTAL;", "2020-01-01;0100;1,5;", "2020-01-01;0200;2,5;")
  crlf <- charToRaw(paste(rows, collapse = "\r\n"))          # no newline at the end
  out <- rmet:::.parse_inmet_raw(crlf, "UTC")
  expect_equal(out$precip_mm, c(1.5, 2.5))
  expect_equal(unique(out$station_code), "A001")
  expect_equal(format(out$datetime, "%H:%M"), c("01:00", "02:00"))
  # a header on the very last line has no rows
  expect_null(rmet:::.parse_inmet_raw(charToRaw("UF:;RS\nData;Hora UTC;PRECIPITACAO"), "UTC"))
  expect_error(rmet:::.parse_inmet_raw(raw(0), "UTC"), "locate")
  expect_error(rmet:::.parse_inmet_raw(charToRaw("UF:;RS\nData;Valor\n1;2\n"), "UTC"),
               "locate")
})

test_that("files larger than the header window are read completely", {
  body <- sprintf("2020-01-%02d;%04d;%d;", rep(1:28, each = 24), rep(0:23 * 100, 28), seq_len(672))
  lines <- c("REGIAO:;S", "UF:;RS", "ESTACAO:;TESTE", "CODIGO (WMO):;A001",
             "LATITUDE:;-30,0", "LONGITUDE:;-51,0", "ALTITUDE:;10",
             "Data;Hora UTC;PRECIPITACAO TOTAL;", rep(body, 30))
  bytes <- charToRaw(paste0(paste(lines, collapse = "\n"), "\n"))
  expect_gt(length(bytes), rmet:::.header_bytes)
  out <- rmet:::.parse_inmet_raw(bytes, "UTC")
  expect_equal(nrow(out), 672L * 30L)
  expect_equal(out$precip_mm[c(1, 672 * 30)], c(1, 672))
})

test_that("the C parser checks its arguments", {
  parse <- function(...) .Call(rmet:::C_parse_inmet_body, ...)
  bytes <- charToRaw("a;b;1\n")
  expect_error(parse("text", 0L, 3L, 1L, 2L, 3L), "raw vector")
  expect_error(parse(bytes, -1L, 3L, 1L, 2L, 3L), "Invalid arguments")
  expect_error(parse(bytes, 100L, 3L, 1L, 2L, 3L), "Invalid arguments")
  expect_error(parse(bytes, 0L, 0L, 1L, 2L, 3L), "Invalid arguments")
  expect_error(parse(bytes, NA_integer_, 3L, 1L, 2L, 3L), "Invalid arguments")
  expect_error(parse(bytes, 0L, 3L, 1L, 2L, 3), "must be integer")
  # fields outside the row layout are ignored
  out <- parse(bytes, 0L, 3L, 0L, 9L, c(3L, 7L))
  expect_equal(dim(out$values), c(2L, 1L))
  expect_equal(out$values[, 1], c(1, NA))
  expect_true(is.na(out$day) && is.na(out$seconds))
})

test_that("line decoding handles Latin-1, UTF-8 and byte-order marks", {
  latin1 <- iconv("ESTA\u00c7\u00c3O:;X", "UTF-8", "latin1")
  Encoding(latin1) <- "unknown"
  expect_equal(rmet:::.decode_lines(latin1), "ESTA\u00c7\u00c3O:;X")
  expect_equal(rmet:::.decode_lines("\ufeffREGIAO:;S"), "REGIAO:;S")
  expect_equal(rmet:::.decode_lines(character()), character())
})

test_that(".safe_rbind keeps POSIXct and Date classes", {
  a <- data.frame(t = as.POSIXct("2020-01-01", tz = "UTC"), d = as.Date("2020-01-01"))
  b <- data.frame(t = as.POSIXct("2020-01-02", tz = "UTC"), x = 1)
  out <- rmet:::.safe_rbind(list(a, b))
  expect_s3_class(out$t, "POSIXct")
  expect_equal(attr(out$t, "tzone"), "UTC")
  expect_s3_class(out$d, "Date")
  expect_equal(out$x, c(NA, 1))
  expect_equal(rownames(out), c("1", "2"))
})

test_that("missing stations across several years are reported", {
  td <- file.path(tempdir(), "rmet_parse_years")
  create_mock_zip(td, inmet_lines_2019(), year = 2020)
  file.copy(file.path(td, "2020.zip"), file.path(td, "2021.zip"))
  expect_warning(
    df <- inmet_read(2020:2021, stations = c("A801", "Z999"), dest_dir = td, quiet = TRUE),
    "not found in any requested year: Z999"
  )
  # both archives hold the same hours: each station-hour is returned once
  expect_equal(nrow(df), 3L)
})

test_that("cumulative versions of a station file are not duplicated", {
  # Like INMET's current-year archive: "01-01_A_31-08" and "01-01_A_30-09"
  # versions of the same station, the newer one with a revised value, plus a
  # station present only in the older version.
  head_rows <- inmet_lines_2019()[1:9]
  older <- c(head_rows, "2019/01/01;0000 UTC;0;25,8;26,4;23,4;",
             "2019/01/01;0200 UTC;1,2;24,0;25,3;22,0;")
  newer <- c(head_rows, "2019/01/01;0000 UTC;0;25,8;26,4;23,4;",
             "2019/01/01;0200 UTC;1,2;-9999;25,3;22,0;",
             "2019/01/01;0300 UTC;0;24,1;24,9;21,0;")
  other <- sub("A801", "A999", c(head_rows, "2019/01/01;0000 UTC;0;20,0;21,0;10,0;"))
  td <- file.path(tempdir(), "rmet_parse_versions")
  create_mock_zip(
    td, list(older, newer, other),
    csv_name = c("INMET_S_RS_A801_PORTO ALEGRE_01-01-2019_A_31-08-2019.CSV",
                 "INMET_S_RS_A801_PORTO ALEGRE_01-01-2019_A_30-09-2019.CSV",
                 "INMET_S_RS_A999_OUTRA_01-01-2019_A_31-08-2019.CSV")
  )
  expect_message(df <- inmet_read(2020, dest_dir = td), "Dropped 2 row")
  expect_equal(df$station_code, c("A801", "A801", "A801", "A999"))
  expect_equal(format(df$datetime, "%H:%M"), c("00:00", "02:00", "03:00", "00:00"))
  expect_equal(df$temp_dry_c[1:3], c(25.8, NA, 24.1))  # revised value from newer file

  expect_equal(
    rmet:::.entry_end_date(c("X_01-01-2019_A_30-09-2019.CSV", "no_dates.CSV")),
    as.Date(c("2019-09-30", NA))
  )
})

test_that("inmet_extract can restrict extraction to some stations", {
  td <- file.path(tempdir(), "rmet_extract_station")
  create_mock_zip(td, inmet_lines_2019())
  out <- file.path(tempdir(), "rmet_extract_station_out")
  unlink(out, recursive = TRUE)
  expect_length(inmet_extract(2020, dest_dir = td, out_dir = out, stations = "Z999",
                              quiet = TRUE), 0L)
  expect_length(inmet_extract(2020, dest_dir = td, out_dir = out, stations = "a801",
                              quiet = TRUE), 1L)
})
