test_that("cache status and clear work correctly", {
  test_cache <- file.path(tempdir(), "rmet_test_cache")

  inmet_cache_clear(dest_dir = test_cache, ask = FALSE)
  status_empty <- inmet_cache_status(dest_dir = test_cache)
  expect_equal(nrow(status_empty), 0)

  mock_zip <- create_mock_inmet_data(test_cache, year = 2020)
  status_full <- inmet_cache_status(dest_dir = test_cache)

  expect_equal(nrow(status_full), 1)
  expect_equal(status_full$year[1], 2020)
  expect_true(status_full$valid[1])
  expect_false(status_full$partial[1])

  inmet_cache_clear(years = 2020, dest_dir = test_cache, ask = FALSE)
  expect_false(file.exists(mock_zip))
})

test_that("cache clear can remove station catalogue and partial files", {
  td <- file.path(tempdir(), "rmet_clear_extra")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  writeLines("partial", file.path(td, "2020.zip.part"))
  saveRDS(data.frame(station_code = "A001"), file.path(td, "stations.rds"))

  inmet_cache_clear(dest_dir = td, ask = FALSE, catalog = TRUE)
  expect_false(file.exists(file.path(td, "2020.zip.part")))
  expect_false(file.exists(file.path(td, "stations.rds")))
})

test_that("cache status reports an orphan partial download", {
  td <- file.path(tempdir(), "rmet_status_partial")
  unlink(td, recursive = TRUE)
  dir.create(td, recursive = TRUE)
  writeLines("partial", file.path(td, "2020.zip.part"))

  result <- suppressMessages(inmet_cache_status(td))
  expect_equal(result$year, 2020L)
  expect_false(result$valid)
  expect_true(result$partial)
})

test_that("inmet_cache_clear does nothing when no files match", {
  td <- file.path(tempdir(), "rmet_empty_clear")
  dir.create(td, showWarnings = FALSE)
  expect_message(
    inmet_cache_clear(years = 2020, dest_dir = td, ask = FALSE),
    "Nothing to delete"
  )
})

test_that("inmet_cache_clear aborts when user answers no", {
  td <- file.path(tempdir(), "rmet_ask_no")
  create_mock_inmet_data(td, year = 2020)

  local_mocked_bindings(
    readline = function(prompt) "n",
    .package = "base"
  )

  expect_message(
    inmet_cache_clear(dest_dir = td, ask = TRUE),
    "Aborted"
  )
  expect_true(file.exists(file.path(td, "2020.zip")))
})

test_that("inmet_cache_clear deletes when user answers yes", {
  td <- file.path(tempdir(), "rmet_ask_yes")
  create_mock_inmet_data(td, year = 2020)

  local_mocked_bindings(
    readline = function(prompt) "y",
    .package = "base"
  )

  inmet_cache_clear(dest_dir = td, ask = TRUE)
  expect_false(file.exists(file.path(td, "2020.zip")))
})

test_that("validation helpers cover success and failure paths", {
  expect_equal(rmet:::.validate_years(c("2020", "2020", "2021")), c(2020L, 2021L))
  expect_error(rmet:::.validate_years(list(2020)), "non-empty")
  expect_error(rmet:::.validate_years(Inf), "integers")

  expect_true(rmet:::.validate_flag(TRUE, "flag"))
  expect_error(rmet:::.validate_flag(1, "flag"), "TRUE or FALSE")
  expect_error(rmet:::.validate_flag(c(TRUE, FALSE), "flag"), "TRUE or FALSE")

  expect_equal(rmet:::.validate_positive_integer("2", "tries"), 2L)
  expect_error(rmet:::.validate_positive_integer(c(1, 2), "tries"), "positive integer")
  expect_error(rmet:::.validate_positive_integer(1.5, "tries"), "positive integer")

  expect_equal(rmet:::.validate_directory("~", "path"), path.expand("~"))
  expect_error(rmet:::.validate_directory(character(), "path"), "single non-empty")
  expect_error(rmet:::.validate_directory(NA_character_, "path"), "single non-empty")

  expect_equal(rmet:::.validate_timezone("UTC"), "UTC")
  expect_error(rmet:::.validate_timezone(character()), "single time-zone")
  expect_error(rmet:::.validate_timezone(""), "single time-zone")

  expect_null(rmet:::.validate_character_filter(NULL, "x"))
  expect_equal(
    rmet:::.validate_character_filter(c(" rs ", "RS"), "x", uppercase = TRUE),
    "RS"
  )
  expect_error(rmet:::.validate_character_filter(1, "x"), "character vector")
  expect_error(rmet:::.validate_character_filter(c("ok", ""), "x"), "character vector")

  bounds <- rmet:::.validate_date_bounds(as.Date("2020-01-01"), NULL, "UTC")
  expect_s3_class(bounds$start, "POSIXct")
  expect_null(bounds$end)
  expect_error(rmet:::.validate_date_bounds(1, NULL, "UTC"), "one date")
})

test_that("small utility helpers cover edge cases", {
  expect_false(rmet:::.is_valid_zip(NULL))
  expect_false(rmet:::.is_valid_zip(c("a", "b")))
  bad <- tempfile(fileext = ".zip")
  writeLines("not a zip", bad)
  expect_false(rmet:::.is_valid_zip(bad))

  local_mocked_bindings(
    packageVersion = function(...) stop("not installed"),
    .package = "utils"
  )
  expect_match(rmet:::.rmet_user_agent(), "rmet/0.2.0")

  expect_equal(
    rmet:::.inmet_id_columns(),
    c("station_code", "station_name", "region", "state", "latitude",
      "longitude", "elevation", "datetime")
  )
  expect_equal(rmet:::.extract_station_code("no_code.csv"), NA_character_)
  expect_equal(
    rmet:::.unsafe_archive_path(c("ok/file.csv", "../bad", "C:\\bad", "/bad")),
    c(FALSE, TRUE, TRUE, TRUE)
  )
  expect_equal(rmet:::.parse_inmet_number(2.5), 2.5)
  expect_equal(rmet:::.parse_inmet_number(" 2,5 "), 2.5)

  value <- rmet:::.convert_tz(
    as.POSIXct("2020-01-01", tz = "UTC"),
    "America/Sao_Paulo"
  )
  expect_equal(attr(value, "tzone"), "America/Sao_Paulo")
})

test_that("cache helpers handle absent directories and catalogue-only removal", {
  td <- file.path(tempdir(), "rmet_absent_cache")
  unlink(td, recursive = TRUE)
  expect_message(inmet_cache_clear(dest_dir = td, ask = FALSE), "does not exist")

  dir.create(td)
  saveRDS(data.frame(station_code = "A001"), file.path(td, "stations.rds"))
  expect_message(
    inmet_cache_clear(dest_dir = td, ask = FALSE, catalog = TRUE),
    "Deleted 1/1"
  )
  expect_false(file.exists(file.path(td, "stations.rds")))
})
