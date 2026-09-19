test_that("inmet_download input validation works", {
  expect_error(inmet_download(years = 1999), "2000")
  expect_error(inmet_download(years = 3000), "future")
  expect_error(inmet_download(max_tries = 0), "positive integer")
  expect_error(inmet_download(years = NA_integer_), "integers")
  expect_error(inmet_download(years = 2020.5), "integers")
  expect_error(inmet_download(years = integer()), "non-empty")
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
      "Attempt"
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

test_that("inmet_download does not contact the server for a valid cached ZIP", {
  td <- file.path(tempdir(), "rmet_cached_test")
  create_mock_inmet_data(td, year = 2020)

  local_mocked_bindings(
    curl_fetch_stream = function(...) stop("network should not be used"),
    .package = "curl"
  )

  expect_message(
    result <- inmet_download(2020, dest_dir = td, quiet = FALSE),
    "already cached"
  )
  expect_true(file.exists(result[[1]]))
})

test_that("failed downloads remain .part files", {
  td <- file.path(tempdir(), "rmet_part_test")
  unlink(td, recursive = TRUE)

  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      fun(charToRaw("not a zip"))
      stop("connection dropped")
    },
    .package = "curl"
  )

  expect_warning(
    inmet_download(2020, dest_dir = td, max_tries = 1, quiet = TRUE),
    "Failed to download"
  )
  expect_false(file.exists(file.path(td, "2020.zip")))
  expect_true(file.exists(file.path(td, "2020.zip.part")))
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
  skip_if(Sys.getenv("RMET_RUN_NETWORK_TESTS") != "true",
          "set RMET_RUN_NETWORK_TESTS=true to run live network tests")
  skip_if_offline()
  test_cache <- file.path(tempdir(), "rmet_dl_test")
  paths <- inmet_download(years = 2023, dest_dir = test_cache, max_tries = 3, quiet = TRUE)
  expect_true(length(paths) > 0)
  expect_true(file.exists(paths[[1]]))
})

test_that("download can stream a valid archive and finalize a valid part", {
  source_dir <- file.path(tempdir(), "rmet_download_source")
  unlink(source_dir, recursive = TRUE)
  source_zip <- create_mock_inmet_data(source_dir, 2020)
  bytes <- readBin(source_zip, what = "raw", n = file.info(source_zip)$size)

  td <- file.path(tempdir(), "rmet_stream_success")
  unlink(td, recursive = TRUE)
  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      fun(bytes)
      invisible(list(status_code = 200L))
    },
    .package = "curl"
  )
  expect_message(
    result <- inmet_download(2020, dest_dir = td, max_tries = 1, quiet = FALSE),
    "Year 2020 OK"
  )
  expect_true(rmet:::.is_valid_zip(result[["2020"]]))

  td_part <- file.path(tempdir(), "rmet_valid_part")
  unlink(td_part, recursive = TRUE)
  dir.create(td_part)
  file.copy(source_zip, file.path(td_part, "2020.zip.part"))
  expect_message(
    part_result <- inmet_download(2020, dest_dir = td_part, quiet = FALSE),
    "Year 2020 OK"
  )
  expect_true(file.exists(part_result[["2020"]]))
})

test_that("download handles legacy partial files and range failures", {
  td <- file.path(tempdir(), "rmet_legacy_partial")
  unlink(td, recursive = TRUE)
  dir.create(td)
  writeLines("old partial", file.path(td, "2020.zip"))

  local_mocked_bindings(
    curl_fetch_stream = function(...) stop("connection failed"),
    .package = "curl"
  )
  expect_warning(
    inmet_download(2020, dest_dir = td, max_tries = 1, quiet = TRUE),
    "Failed to download"
  )
  expect_true(file.exists(file.path(td, "2020.zip.part")))

  writeLines("bad final", file.path(td, "2020.zip"))
  expect_warning(
    inmet_download(2020, dest_dir = td, max_tries = 1, quiet = TRUE),
    "Failed to download"
  )
  expect_false(file.exists(file.path(td, "2020.zip")))

  range_dir <- file.path(tempdir(), "rmet_range_failure")
  unlink(range_dir, recursive = TRUE)
  dir.create(range_dir)
  writeLines("partial", file.path(range_dir, "2020.zip.part"))
  local_mocked_bindings(
    curl_fetch_stream = function(...) stop("HTTP 416 range resume"),
    .package = "curl"
  )
  expect_warning(
    inmet_download(2020, dest_dir = range_dir, max_tries = 1, quiet = TRUE),
    "Failed to download"
  )
  expect_false(file.exists(file.path(range_dir, "2020.zip.part")))
})

test_that("download handles unavailable file sizes and replaces destinations", {
  td <- file.path(tempdir(), "rmet_na_size")
  unlink(td, recursive = TRUE)
  dir.create(td)
  part <- file.path(td, "2020.zip.part")
  writeLines("partial", part)

  local_mocked_bindings(file.size = function(...) NA_real_, .package = "base")
  local_mocked_bindings(
    curl_fetch_stream = function(...) stop("connection failed"),
    .package = "curl"
  )
  expect_warning(
    rmet:::.download_one_year(
      2020, file.path(td, "2020.zip"), part,
      max_tries = 1, quiet = TRUE
    ),
    "Failed to download"
  )

  source_dir <- file.path(tempdir(), "rmet_replace_destination")
  unlink(source_dir, recursive = TRUE)
  valid <- create_mock_inmet_data(source_dir, 2020)
  valid_part <- file.path(source_dir, "valid.zip.part")
  file.copy(valid, valid_part)
  destination <- file.path(source_dir, "destination.zip")
  writeLines("old destination", destination)
  expect_true(rmet:::.finalize_download(valid_part, destination, 2020, quiet = TRUE))
  expect_true(rmet:::.is_valid_zip(destination))
})

test_that("download and finalization report filesystem failures", {
  blocker <- tempfile("rmet-dest-file-")
  writeLines("file", blocker)
  expect_error(
    inmet_download(2020, dest_dir = file.path(blocker, "child"), max_tries = 1),
    "Could not create"
  )

  td <- file.path(tempdir(), "rmet_force_remove_failure")
  unlink(td, recursive = TRUE)
  create_mock_inmet_data(td, 2020)
  local_mocked_bindings(file.remove = function(...) FALSE, .package = "base")
  expect_warning(
    result <- inmet_download(2020, dest_dir = td, force = TRUE, quiet = TRUE),
    "Could not remove"
  )
  expect_length(result, 0L)

  source_dir <- file.path(tempdir(), "rmet_finalize_source")
  unlink(source_dir, recursive = TRUE)
  part <- create_mock_inmet_data(source_dir, 2020)
  destination <- file.path(source_dir, "final.zip")

  local_mocked_bindings(file.rename = function(...) FALSE, .package = "base")
  expect_true(rmet:::.finalize_download(part, destination, 2020, quiet = TRUE))
  expect_true(file.exists(destination))

  part2 <- create_mock_inmet_data(file.path(tempdir(), "rmet_finalize_fail"), 2020)
  local_mocked_bindings(
    file.rename = function(...) FALSE,
    file.copy = function(...) FALSE,
    .package = "base"
  )
  expect_warning(
    expect_false(rmet:::.finalize_download(
      part2, tempfile(fileext = ".zip"), 2020, quiet = TRUE
    )),
    "could not be finalized"
  )
})

