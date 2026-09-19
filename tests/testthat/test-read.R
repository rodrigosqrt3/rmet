test_that("inmet_read correctly parses ZIP files", {
  test_cache <- file.path(tempdir(), "rmet_test_read")
  create_mock_inmet_data(test_cache, year = 2020)

  df <- inmet_read(years = 2020, dest_dir = test_cache, quiet = TRUE)

  expect_s3_class(df, "data.frame")
  expect_equal(nrow(df), 3)

  expect_equal(df$precip_mm[1], 0.0)
  expect_true(is.na(df$radiation_kjm2[1]))

  expect_equal(df$station_code[1], "A801")
  expect_equal(df$state[1], "RS")

  expect_s3_class(df$datetime, "POSIXct")

  df_filtered <- inmet_read(
    years = 2020,
    start_date = "2020-09-25",
    end_date = "2020-09-25",
    dest_dir = test_cache,
    quiet = TRUE
  )
  expect_equal(nrow(df_filtered), 2)
  expect_equal(df_filtered$temp_dry_c[1], 14.7)

  df_sub <- inmet_read(
    years = 2020,
    stations = "A801",
    variables = c("precip_mm"),
    dest_dir = test_cache,
    quiet = TRUE
  )
  expect_true("precip_mm" %in% names(df_sub))
  expect_false("temp_dry_c" %in% names(df_sub))
})

test_that("inmet_read errors on NA years", {
  expect_error(inmet_read(years = NA_integer_), "integers")
})

test_that("inmet_read validates dates and time zones", {
  expect_error(inmet_read(2020, tz = "Not/AZone"), "Unknown timezone")
  expect_error(inmet_read(2020, start_date = "2020-99-01"), "valid date")
  expect_error(
    inmet_read(2020, start_date = "2020-02-01", end_date = "2020-01-01"),
    "must not be after"
  )
})

test_that("inmet_read errors when ZIP is missing", {
  td <- file.path(tempdir(), "rmet_read_nozip")
  dir.create(td, showWarnings = FALSE)
  expect_error(
    inmet_read(years = 2020, dest_dir = td),
    "not found"
  )
})

test_that("inmet_read quiet = FALSE emits messages", {
  td <- file.path(tempdir(), "rmet_read_quiet")
  create_mock_inmet_data(td, year = 2020)
  expect_message(
    inmet_read(years = 2020, dest_dir = td, quiet = FALSE),
    "Reading year"
  )
})

test_that("inmet_read warns on unknown variables", {
  td <- file.path(tempdir(), "rmet_read_vars")
  create_mock_inmet_data(td, year = 2020)
  expect_warning(
    inmet_read(years = 2020, dest_dir = td, variables = c("precip_mm", "nonexistent_col"), quiet = TRUE),
    "Unknown variables ignored"
  )
})

test_that("inmet_read warns when date filter removes all rows", {
  td <- file.path(tempdir(), "rmet_read_datefilter")
  create_mock_inmet_data(td, year = 2020)
  expect_warning(
    result <- inmet_read(years = 2020, dest_dir = td,
                         start_date = "2099-01-01", end_date = "2099-01-02",
                         quiet = TRUE),
    "No data was read"
  )
  expect_equal(nrow(result), 0L)
})

test_that("inmet_read warns when station filter matches nothing", {
  td <- file.path(tempdir(), "rmet_read_nostation")
  create_mock_inmet_data(td, year = 2020)

  expect_warning(
    expect_warning(
      result <- inmet_read(years = 2020, dest_dir = td, stations = "ZZZZ", quiet = TRUE),
      "No data was read"
    ),
    "No matching stations found"
  )
  expect_equal(nrow(result), 0L)
})

test_that("inmet_read warns on corrupt CSV inside ZIP", {
  td <- file.path(tempdir(), "rmet_read_corrupt")
  dir.create(td, showWarnings = FALSE)
  csv_name <- "INMET_S_RS_A801_PORTO_ALEGRE_01-01-2020_A_31-12-2020.CSV"
  csv_path <- file.path(tempdir(), csv_name)
  writeLines(c("HEADER", ";;;;", "INVALID;;;DATA"), csv_path)
  zip_path <- file.path(td, "2020.zip")
  old_wd <- setwd(tempdir())
  on.exit(setwd(old_wd), add = TRUE)
  utils::zip(zipfile = zip_path, files = csv_name)
  file.remove(csv_path)

  expect_warning(
    expect_warning(
      result <- inmet_read(years = 2020, dest_dir = td, quiet = TRUE),
      "No data was read"
    ),
    "Could not parse"
  )
})

test_that("inmet_extract extracts CSVs to output directory", {
  td  <- file.path(tempdir(), "rmet_extract_in")
  out <- file.path(tempdir(), "rmet_extract_out")
  create_mock_inmet_data(td, year = 2020)
  expect_message(
    paths <- inmet_extract(years = 2020, dest_dir = td, out_dir = out, quiet = FALSE),
    "Extracting year"
  )
  expect_true(length(paths) > 0)
  expect_true(any(file.exists(paths)))
})

test_that("inmet_extract warns when ZIP missing", {
  td  <- file.path(tempdir(), "rmet_extract_nozip")
  out <- file.path(tempdir(), "rmet_extract_nozip_out")
  dir.create(td, showWarnings = FALSE)
  expect_warning(
    inmet_extract(years = 2020, dest_dir = td, out_dir = out, quiet = TRUE),
    "not found"
  )
})

test_that(".convert_tz handles unknown timezone", {
  expect_error(
    rmet:::.convert_tz(as.POSIXct("2020-01-01", tz = "UTC"), "Not/ATimezone"),
    "Unknown timezone"
  )
})

test_that(".safe_rbind returns NULL on empty input", {
  expect_null(rmet:::.safe_rbind(list()))
})

test_that(".safe_rbind fills missing columns with NA", {
  a <- data.frame(x = 1, y = 2)
  b <- data.frame(x = 3, z = 4)
  result <- rmet:::.safe_rbind(list(a, b))
  expect_true("y" %in% names(result))
  expect_true("z" %in% names(result))
  expect_true(is.na(result$y[2]))
  expect_true(is.na(result$z[1]))
})

test_that("errors when unzip fails", {
  td <- file.path(tempdir(), "rmet_unzip_fail")
  create_mock_inmet_data(td, 2020)

  testthat::with_mocked_bindings(
    unzip = function(...) stop("fail unzip"),
    .package = "utils",
    {
      expect_error(
        inmet_read(years = 2020, dest_dir = td, quiet = TRUE),
        "invalid or incomplete"
      )
    }
  )
})

test_that("specific temperature mappings are not overwritten", {
  input <- data.frame(
    "Data" = "2020-01-01",
    "Hora UTC" = "0000 UTC",
    "TEMPERATURA DO PONTO DE ORVALHO MAX. NA HORA ANT. (AUT) (°C)" = "10,0",
    "TEMPERATURA DO PONTO DE ORVALHO MIN. NA HORA ANT. (AUT) (°C)" = "8,0",
    "TEMPERATURA MÁXIMA NA HORA ANT. (AUT) (°C)" = "20,0",
    check.names = FALSE
  )
  renamed <- rmet:::.rename_columns(input)
  expect_true(all(c("temp_dew_max_c", "temp_dew_min_c", "temp_max_prev_c") %in% names(renamed)))
})

test_that("station codes with four to six characters are recognized", {
  expect_equal(
    rmet:::.extract_station_code("INMET_S_RS_A801_PORTO.CSV"),
    "A801"
  )
  expect_equal(
    rmet:::.extract_station_code("INMET_S_RS_A608B_SAO_GABRIEL.CSV"),
    "A608B"
  )
})

test_that("2400 UTC rolls over to the next day", {
  value <- rmet:::.parse_inmet_datetime("2020-01-01", "2400 UTC", "UTC")
  expect_equal(format(value, tz = "UTC"), "2020-01-02")
})

test_that("inmet_get provides the complete cached workflow", {
  td <- file.path(tempdir(), "rmet_get")
  create_mock_inmet_data(td, year = 2020)
  result <- inmet_get(2020, stations = "A801", dest_dir = td, quiet = TRUE)
  expect_equal(nrow(result), 3L)
  expect_s3_class(result$datetime, "POSIXct")
})

test_that("fallback datetime parsing is triggered", {
  td <- file.path(tempdir(), "rmet_bad_dt")
  create_mock_inmet_bad_datetime(td)

  expect_warning(
    df <- inmet_read(years = 2020, dest_dir = td, quiet = TRUE),
    "invalid timestamp"
  )

  expect_s3_class(df$datetime, "POSIXct")
})

test_that(".safe_rbind handles completely disjoint columns", {
  a <- data.frame(a = 1)
  b <- data.frame(b = 2)
  c <- data.frame(c = 3)

  result <- rmet:::.safe_rbind(list(a, b, c))

  expect_equal(nrow(result), 3)
  expect_true(all(c("a","b","c") %in% names(result)))
})

test_that("inmet_read warns when inner unzip fails to extract a file", {
  td <- file.path(tempdir(), "rmet_inner_unzip_fail")
  create_mock_inmet_data(td, year = 2020)

  real_unzip <- utils::unzip
  call_count <- 0L

  testthat::with_mocked_bindings(
    unzip = function(zipfile, list = FALSE, files = NULL, exdir = tempdir(), ...) {
      if (isTRUE(list)) {
        return(real_unzip(zipfile, list = TRUE))
      }
      call_count <<- call_count + 1L
      if (call_count == 1L) stop("simulated extract failure")
      real_unzip(zipfile, files = files, exdir = exdir, ...)
    },
    .package = "utils",
    {
      expect_warning(
        expect_warning(
          inmet_read(years = 2020, dest_dir = td, quiet = TRUE),
          "No data was read"
        ),
        "Could not extract"
      )
    }
  )
})

test_that("inmet_read handles CSV where all data rows are NA (parse returns NULL)", {
  td <- file.path(tempdir(), "rmet_allna")
  create_mock_inmet_allna(td, year = 2020)

  expect_warning(
    result <- inmet_read(years = 2020, dest_dir = td, quiet = TRUE),
    "No data was read"
  )
  expect_equal(nrow(result), 0L)
})

test_that("column classification covers every canonical variable", {
  input <- as.data.frame(
    setNames(
      replicate(23, "1", simplify = FALSE),
      c(
        "Data", "Hora UTC", "PRECIPITACAO TOTAL",
        "PRESSAO MAX", "PRESSAO MIN", "PRESSAO ESTACAO", "RADIACAO GLOBAL",
        "TEMP PONTO ORVALHO MAX", "TEMP PONTO ORVALHO MIN",
        "TEMP PONTO ORVALHO HORARIA", "TEMP MAX HORA ANT", "TEMP MIN HORA ANT",
        "TEMP BULBO SECO", "TEMP MAX", "TEMP MIN", "UMIDADE MAX",
        "UMIDADE MIN", "UMIDADE HORARIA", "VENTO DIRECAO", "VENTO RAJADA",
        "VENTO VELOCIDADE", "Foo Bar", "Foo-Bar"
      )
    ),
    check.names = FALSE
  )
  result <- rmet:::.rename_columns(input)
  expected <- c(
    "date_raw", "hour_raw", "precip_mm", "pressure_max_hpa",
    "pressure_min_hpa", "pressure_station_hpa", "radiation_kjm2",
    "temp_dew_max_c", "temp_dew_min_c", "temp_dew_c", "temp_max_prev_c",
    "temp_min_prev_c", "temp_dry_c", "temp_max_c", "temp_min_c",
    "humid_rel_max_pct", "humid_rel_min_pct", "humid_rel_pct",
    "wind_dir_deg", "wind_gust_ms", "wind_speed_ms", "foo_bar", "foo_bar_1"
  )
  expect_equal(names(result), expected)
})

test_that("datetime parser covers every accepted date and hour form", {
  values <- rmet:::.parse_inmet_datetime(
    c("2020-01-01", "2020/01/02", "03/01/2020", "04-01-2020"),
    c("0", "0100 UTC", "02:00", "0300"),
    "UTC"
  )
  expect_false(anyNA(values))
  expect_equal(
    format(values, tz = "UTC", format = "%H:%M"),
    c("00:00", "01:00", "02:00", "03:00")
  )

  iso <- rmet:::.parse_inmet_datetime(
    c("2020-01-01", "2020-01-02"), c("0000", "0100"), "UTC"
  )
  expect_false(anyNA(iso))
})

test_that("header helpers handle missing and malformed metadata", {
  parsed <- rmet:::.parse_inmet_header(
    c("REGIAO:", "UF:;RS;", "LATITUDE:;-30,5;")
  )
  expect_true(is.na(parsed$region))
  expect_equal(parsed$state, "RS")
  expect_equal(parsed$latitude, -30.5)
  expect_true(is.na(parsed$name))
  expect_equal(rmet:::.normalize_text(c("estação", " ok ")), c("ESTACAO", "OK"))
})

test_that("single CSV parser covers fallback metadata and structural errors", {
  path <- file.path(tempdir(), "INMET_S_RS_A999_TEST_2020.CSV")
  writeLines(c(
    "REGIAO:;S;", "UF:;RS;", "ESTACAO:;TEST;",
    "Data;Hora UTC", "2020-01-01;0000 UTC"
  ), path)
  result <- rmet:::.parse_inmet_csv(path, tz = "UTC")
  expect_equal(result$station_code, "A999")
  expect_equal(nrow(result), 1L)

  no_header <- tempfile(fileext = ".CSV")
  writeLines(c("REGIAO:;S;", "nothing useful"), no_header)
  expect_error(rmet:::.parse_inmet_csv(no_header, "UTC"), "locate")

  missing_hour <- tempfile(fileext = ".CSV")
  writeLines(c(
    "REGIAO:;S;",
    "Data strange;Observation HORA;TEMP MAX",
    "2020-01-01;0000;20"
  ), missing_hour)
  expect_error(
    rmet:::.parse_inmet_csv(missing_hour, "UTC"),
    "Required date and hour"
  )
})

test_that("inmet_extract covers invalid output and unsafe archives", {
  td <- file.path(tempdir(), "rmet_extract_coverage")
  unlink(td, recursive = TRUE)
  create_mock_inmet_data(td, 2020)

  blocker <- tempfile("rmet-file-")
  writeLines("file", blocker)
  expect_error(
    inmet_extract(2020, dest_dir = td, out_dir = file.path(blocker, "child")),
    "Could not create"
  )

  calls <- 0L
  local_mocked_bindings(
    unzip = function(zipfile, list = FALSE, ...) {
      if (isTRUE(list)) {
        calls <<- calls + 1L
        if (calls == 1L) return(data.frame(Name = "safe.CSV"))
        return(data.frame(Name = "../unsafe.CSV"))
      }
      character()
    },
    .package = "utils"
  )
  expect_warning(
    paths <- inmet_extract(
      2020, dest_dir = td, out_dir = tempdir(), quiet = TRUE
    ),
    "unsafe paths"
  )
  expect_length(paths, 0L)
})

test_that("inmet_get reports archives that could not be obtained", {
  td <- file.path(tempdir(), "rmet_get_missing")
  unlink(td, recursive = TRUE)
  dir.create(td)
  local_mocked_bindings(
    inmet_download = function(...) invisible(character()),
    .package = "rmet"
  )
  expect_error(
    inmet_get(2020, dest_dir = td, quiet = TRUE),
    "Could not obtain"
  )
})

test_that("inmet_read reports temporary-directory creation failures", {
  td <- file.path(tempdir(), "rmet_tempdir_failure")
  unlink(td, recursive = TRUE)
  create_mock_inmet_data(td, 2020)
  local_mocked_bindings(dir.create = function(...) FALSE, .package = "base")
  expect_error(
    inmet_read(2020, dest_dir = td, quiet = TRUE),
    "Could not create a temporary directory"
  )
})
