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

mock_headers <- function(status, ...) {
  fields <- list(...)
  lines <- c(sprintf("HTTP/1.1 %d X", status),
             sprintf("%s: %s", names(fields), unlist(fields)), "", "")
  charToRaw(paste(lines, collapse = "\r\n"))
}

valid_zip_bytes <- function() {
  src <- create_mock_inmet_data(file.path(tempdir(), "rmet_bytes_source"), 2020)
  readBin(src, what = "raw", n = file.info(src)$size)
}

test_that("a server that ignores the byte range does not corrupt the archive", {
  bytes <- valid_zip_bytes()
  td <- file.path(tempdir(), "rmet_range_ignored")
  unlink(td, recursive = TRUE)
  dir.create(td)
  writeBin(bytes[1:50], file.path(td, "2020.zip.part"))  # earlier partial download

  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      fun(bytes)  # whole file again, appended after the 50 old bytes
      list(status_code = 200L,
           headers = mock_headers(200L, `Content-Length` = length(bytes)))
    },
    .package = "curl"
  )
  result <- inmet_download(2020, dest_dir = td, max_tries = 1, quiet = TRUE)
  expect_equal(file.size(result[["2020"]]), length(bytes))
  expect_identical(readBin(result[["2020"]], "raw", length(bytes) + 10), bytes)
})

test_that("resumed downloads are completed with the remaining bytes", {
  bytes <- valid_zip_bytes()
  td <- file.path(tempdir(), "rmet_range_resume")
  unlink(td, recursive = TRUE)
  dir.create(td)
  writeBin(bytes[1:50], file.path(td, "2020.zip.part"))

  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      fun(bytes[-(1:50)])
      list(status_code = 206L,
           headers = mock_headers(206L, `Content-Range` =
                                    sprintf("bytes 50-%d/%d", length(bytes) - 1, length(bytes))))
    },
    .package = "curl"
  )
  result <- inmet_download(2020, dest_dir = td, max_tries = 1, quiet = TRUE)
  expect_identical(readBin(result[["2020"]], "raw", length(bytes) + 10), bytes)
  expect_true(file.exists(file.path(td, "2020.zip.meta")))
})

test_that("archives shorter than announced are not accepted", {
  bytes <- valid_zip_bytes()
  td <- file.path(tempdir(), "rmet_truncated")
  unlink(td, recursive = TRUE)
  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      fun(bytes)
      list(status_code = 200L,
           headers = mock_headers(200L, `Content-Length` = length(bytes) + 1000))
    },
    .package = "curl"
  )
  expect_warning(
    result <- inmet_download(2020, dest_dir = td, max_tries = 1, quiet = TRUE),
    "Failed to download"
  )
  expect_length(result, 0L)
  expect_false(file.exists(file.path(td, "2020.zip")))
})

test_that("recent cached archives are refreshed when the server copy changes", {
  year <- rmet:::.current_year()
  td <- file.path(tempdir(), "rmet_updates")
  unlink(td, recursive = TRUE)
  path <- create_mock_inmet_data(td, year = year)
  writeLines(c("size: 1", "etag: \"old\""), file.path(td, paste0(year, ".zip.meta")))
  bytes <- readBin(path, "raw", file.info(path)$size)

  local_mocked_bindings(
    .remote_metadata = function(year) list(size = 1, etag = "\"old\"", last_modified = NA),
    .package = "rmet"
  )
  local_mocked_bindings(
    curl_fetch_stream = function(...) stop("network should not be used"),
    .package = "curl"
  )
  expect_message(inmet_download(year, dest_dir = td, max_tries = 1, quiet = FALSE),
                 "already cached")

  downloads <- 0L
  local_mocked_bindings(
    .remote_metadata = function(year) list(size = length(bytes), etag = "\"new\"", last_modified = NA),
    .package = "rmet"
  )
  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      downloads <<- downloads + 1L
      fun(bytes)
      list(status_code = 200L, headers = mock_headers(200L, ETag = "\"new\""))
    },
    .package = "curl"
  )
  expect_message(inmet_download(year, dest_dir = td, quiet = FALSE), "updated on the server")
  expect_equal(downloads, 1L)
  expect_equal(rmet:::.read_meta(path)$etag, "\"new\"")

  # no update check when disabled, or when the server cannot be reached
  expect_message(inmet_download(year, dest_dir = td, check_updates = FALSE, quiet = FALSE),
                 "already cached")
  local_mocked_bindings(.remote_metadata = function(year) NULL, .package = "rmet")
  expect_message(inmet_download(year, dest_dir = td, quiet = FALSE), "already cached")
  expect_equal(downloads, 1L)
})

test_that("a failed update keeps the previous archive", {
  year <- rmet:::.current_year()
  td <- file.path(tempdir(), "rmet_update_fail")
  unlink(td, recursive = TRUE)
  path <- create_mock_inmet_data(td, year = year)
  local_mocked_bindings(
    .remote_metadata = function(year) list(size = 1, etag = NA, last_modified = NA),
    .package = "rmet"
  )
  local_mocked_bindings(curl_fetch_stream = function(...) stop("offline"), .package = "curl")
  expect_warning(
    expect_warning(
      result <- inmet_download(year, dest_dir = td, max_tries = 1, quiet = TRUE),
      "Failed to download"
    ),
    "Keeping the previously cached archive"
  )
  expect_true(rmet:::.is_valid_zip(result[[as.character(year)]]))
})

test_that("HTTP helpers read sizes from status and headers", {
  h <- rmet:::.response_headers(list(headers = mock_headers(206L, `Content-Range` = "bytes 5-9/10")))
  expect_equal(rmet:::.expected_size(206L, h), 10)
  h <- rmet:::.response_headers(list(headers = mock_headers(200L, `Content-Length` = "42")))
  expect_equal(rmet:::.expected_size(200L, h), 42)
  expect_true(is.na(rmet:::.expected_size(NA_integer_, list())))
  expect_equal(rmet:::.response_headers(NULL), list())
  expect_true(is.na(rmet:::.response_status(NULL)))

  f <- tempfile()
  writeBin(as.raw(1:20), f)
  rmet:::.drop_prefix(f, 5)
  expect_identical(readBin(f, "raw", 100), as.raw(6:20))
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

