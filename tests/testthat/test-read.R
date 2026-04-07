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
  expect_equal(nrow(df_filtered), 1)
  expect_equal(df_filtered$temp_dry_c[1], 18.5)

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
    "No data left after"
  )
  expect_equal(nrow(result), 0L)
})

test_that("inmet_read warns when station filter matches nothing", {
  td <- file.path(tempdir(), "rmet_read_nostation")
  create_mock_inmet_data(td, year = 2020)
  expect_warning(
    result <- inmet_read(years = 2020, dest_dir = td,
                         stations = "ZZZZ", quiet = TRUE),
    "No data was read"
  )
  expect_equal(nrow(result), 0L)
})

test_that("inmet_read warns on corrupt CSV inside ZIP", {
  td <- file.path(tempdir(), "rmet_read_corrupt")
  dir.create(td, showWarnings = FALSE)
  csv_name <- "INMET_S_RS_A801_PORTO_ALEGRE_01-01-2020_A_31-12-2020.CSV"
  csv_path <- file.path(tempdir(), csv_name)
  writeLines(rep(";;;;;;;", 5), csv_path)
  zip_path <- file.path(td, "2020.zip")
  old_wd <- setwd(tempdir())
  utils::zip(zipfile = zip_path, files = csv_name)
  setwd(old_wd)
  file.remove(csv_path)
  expect_warning(
    result <- inmet_read(years = 2020, dest_dir = td, quiet = TRUE)
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

test_that(".convert_tz warns on unknown timezone", {
  expect_warning(
    result <- inmetr2:::.convert_tz(as.POSIXct("2020-01-01", tz = "UTC"), "Not/ATimezone"),
    "Unknown timezone"
  )
  expect_s3_class(result, "POSIXct")
})

test_that(".safe_rbind returns NULL on empty input", {
  expect_null(inmetr2:::.safe_rbind(list()))
})

test_that(".safe_rbind fills missing columns with NA", {
  a <- data.frame(x = 1, y = 2)
  b <- data.frame(x = 3, z = 4)
  result <- inmetr2:::.safe_rbind(list(a, b))
  expect_true("y" %in% names(result))
  expect_true("z" %in% names(result))
  expect_true(is.na(result$y[2]))
  expect_true(is.na(result$z[1]))
})
