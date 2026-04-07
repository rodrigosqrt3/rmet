test_that("inmet_download input validation works", {
  expect_error(inmet_download(years = 1999), "2000")
  expect_error(inmet_download(years = 3000), "future")
  expect_error(inmet_download(max_tries = 0), "positive integer")
  expect_error(inmet_download(years = NA_integer_), "integers")
})

test_that("inmet_download creates dest_dir if missing", {
  td <- file.path(tempdir(), "rmet_newdir_test")
  if (dir.exists(td)) unlink(td, recursive = TRUE)

  # mock curl so no network call happens
  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) invisible(NULL),
    .package = "curl"
  )

  # will warn (invalid zip) but dest_dir must be created
  suppressWarnings(
    inmet_download(years = 2020, dest_dir = td, max_tries = 1, quiet = FALSE)
  )
  expect_true(dir.exists(td))
})

test_that("inmet_download force = TRUE removes existing file before download", {
  td <- file.path(tempdir(), "rmet_force_test")
  create_mock_inmet_data(td, year = 2020)

  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) invisible(NULL),
    .package = "curl"
  )

  expect_message(
    suppressWarnings(
      inmet_download(years = 2020, dest_dir = td, max_tries = 1,
                     quiet = FALSE, force = TRUE)
    ),
    "Removing existing file"
  )
})

test_that("inmet_download returns named vector of paths on success", {
  td <- file.path(tempdir(), "rmet_success_test")
  zip_path <- create_mock_inmet_data(td, year = 2020)

  # curl mock that copies our valid zip bytes into the dest file
  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) invisible(NULL),
    .package = "curl"
  )

  # ZIP is already valid on disk — download should detect it and return TRUE
  result <- inmet_download(years = 2020, dest_dir = td, max_tries = 1, quiet = TRUE)

  expect_type(result, "character")
  expect_named(result, "2020")
  expect_true(file.exists(result[["2020"]]))
})

test_that("inmet_download warns and returns empty vector when all attempts fail", {
  td <- file.path(tempdir(), "rmet_fail_test")
  if (!dir.exists(td)) dir.create(td, recursive = TRUE)
  bad_zip <- file.path(td, "2021.zip")
  if (file.exists(bad_zip)) file.remove(bad_zip)

  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) stop("connection refused"),
    .package = "curl"
  )

  expect_warning(
    expect_message(
      result <- inmet_download(years = 2021, dest_dir = td, max_tries = 2, quiet = FALSE),
      "Connection dropped"
    ),
    "Failed to download"
  )
  expect_length(result, 0L)
})

test_that("inmet_download quiet = FALSE emits messages on success", {
  td <- file.path(tempdir(), "rmet_quiet_test")
  create_mock_inmet_data(td, year = 2020)

  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) invisible(NULL),
    .package = "curl"
  )

  expect_message(
    inmet_download(years = 2020, dest_dir = td, max_tries = 1, quiet = FALSE)
  )
})

test_that("inmet_download handles multiple years, partial failure", {
  td <- file.path(tempdir(), "rmet_multi_test")
  create_mock_inmet_data(td, year = 2020)
  # no zip for 2021 — will fail

  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) invisible(NULL),
    .package = "curl"
  )

  expect_warning(
    result <- inmet_download(
      years     = c(2020L, 2021L),
      dest_dir  = td,
      max_tries = 1L,
      quiet     = TRUE
    ),
    "Failed to download"
  )

  # 2020 succeeds (zip already valid), 2021 fails
  expect_true("2020" %in% names(result))
  expect_false("2021" %in% names(result))
})

test_that("inmet_download fetches data for real (skipped on CRAN)", {
  skip_on_cran()
  skip_if_offline()
  test_cache <- file.path(tempdir(), "rmet_dl_test")
  paths <- inmet_download(years = 2023, dest_dir = test_cache, max_tries = 3, quiet = TRUE)
  expect_true(length(paths) > 0)
  expect_true(file.exists(paths[[1]]))
})

