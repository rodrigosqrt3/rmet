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
  values <- rmet:::.parse_inmet_datetime(
    c("04-01-2020", "2020-01-04", "31/12/2019", "2019/12/31", "2020-13-01", "garbage"),
    c("0000", "0000", "2300", "2300", "0000", "0000"),
    "UTC"
  )
  expect_equal(
    format(values, "%Y-%m-%d %H:%M"),
    c("2020-01-04 00:00", "2020-01-04 00:00", "2019-12-31 23:00",
      "2019-12-31 23:00", NA, NA)
  )
})

test_that("invalid hours give NA timestamps", {
  values <- rmet:::.parse_inmet_datetime(
    rep("2020-01-01", 5), c("2500", "1260", "2401", "", "12:30"), "UTC"
  )
  expect_equal(format(values, "%H:%M"), c(NA, NA, NA, NA, "12:30"))
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
  cells <- rmet:::.split_fields(c("a;b;c", "d;e", "f;g;h;i"), 3L)
  expect_equal(dim(cells), c(3L, 3L))
  expect_equal(cells[, 2], c("d", "e", NA))
  expect_equal(cells[, 3], c("f", "g", "h"))
  expect_equal(rmet:::.split_fields(c("a;b", "c;d"), 2L), matrix(c("a", "b", "c", "d"), 2))
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
