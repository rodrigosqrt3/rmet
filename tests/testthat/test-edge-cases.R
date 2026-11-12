test_that("remote metadata is read from a HEAD response, or NULL on failure", {
  headers <- charToRaw(paste(
    "HTTP/1.1 200 OK", "Content-Length: 123", "ETag: \"abc\"",
    "Last-Modified: Mon, 05 Oct 2026 11:47:31 GMT", "", "",
    sep = "\r\n"
  ))
  local_mocked_bindings(
    curl_fetch_memory = function(url, handle) {
      expect_match(url, "dadoshistoricos/2026\\.zip$")
      list(status_code = 200L, headers = headers)
    },
    .package = "curl"
  )
  meta <- rmet:::.remote_metadata(2026)
  expect_equal(meta$size, 123)
  expect_equal(meta$etag, "\"abc\"")
  expect_equal(meta$last_modified, "Mon, 05 Oct 2026 11:47:31 GMT")

  local_mocked_bindings(curl_fetch_memory = function(...) stop("offline"), .package = "curl")
  expect_null(rmet:::.remote_metadata(2026))
})

test_that("download progress reports the expected size", {
  src <- create_mock_inmet_data(file.path(tempdir(), "rmet_progress_src"), 2020)
  bytes <- readBin(src, "raw", file.info(src)$size)
  td <- file.path(tempdir(), "rmet_progress")
  unlink(td, recursive = TRUE)
  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      fun(bytes)
      list(status_code = 200L, headers = charToRaw(paste0(
        "HTTP/1.1 200 OK\r\nContent-Length: ", length(bytes), "\r\n\r\n"
      )))
    },
    .package = "curl"
  )
  expect_message(inmet_download(2020, dest_dir = td, quiet = FALSE), "Total on disk: .* of ")
})

test_that("an update removes a stale partial file before downloading", {
  year <- rmet:::.current_year()
  td <- file.path(tempdir(), "rmet_update_part")
  unlink(td, recursive = TRUE)
  path <- create_mock_inmet_data(td, year = year)
  bytes <- readBin(path, "raw", file.info(path)$size)
  writeBin(as.raw(1:10), paste0(path, ".part"))  # stale partial download

  local_mocked_bindings(
    .remote_metadata = function(year) list(size = 1, etag = NA, last_modified = NA),
    .package = "rmet"
  )
  local_mocked_bindings(
    curl_fetch_stream = function(url, fun, handle) {
      fun(bytes)
      list(status_code = 200L)
    },
    .package = "curl"
  )
  result <- inmet_download(year, dest_dir = td, quiet = TRUE)
  expect_identical(readBin(result[[1]], "raw", length(bytes) + 10), bytes)
  expect_false(file.exists(paste0(path, ".part")))
})

test_that("empty or unreadable version files are ignored", {
  path <- tempfile(fileext = ".zip")
  file.create(rmet:::.meta_path(path))
  expect_null(rmet:::.read_meta(path))
  expect_null(rmet:::.read_meta(tempfile()))
})

test_that("cache status reports the recorded download time", {
  td <- file.path(tempdir(), "rmet_status_meta")
  unlink(td, recursive = TRUE)
  path <- create_mock_inmet_data(td, year = 2020)
  writeLines(c("size: 1", "downloaded: 2026-10-19 18:02:00"), rmet:::.meta_path(path))
  status <- inmet_cache_status(td)
  expect_equal(status$downloaded, "2026-10-19 18:02:00")
})

test_that("rows are ordered by station even when the archive is not", {
  head_rows <- inmet_lines_2019()[1:9]
  a801 <- c(head_rows, "2019/01/01;0000 UTC;0;25,8;26,4;23,4;")
  a001 <- sub("A801", "A001", a801)
  td <- file.path(tempdir(), "rmet_order")
  create_mock_zip(
    td, list(a801, a001),
    csv_name = c("INMET_S_RS_A801_X_01-01-2019_A_31-12-2019.CSV",
                 "INMET_CO_DF_A001_Y_01-01-2019_A_31-12-2019.CSV")
  )
  df <- inmet_read(2020, dest_dir = td, quiet = TRUE)
  expect_equal(df$station_code, c("A001", "A801"))
  expect_equal(rownames(df), c("1", "2"))
})

test_that("files with a header but no data rows give no rows", {
  expect_null(rmet:::.parse_inmet_lines(c("UF:;RS", "Data;Hora UTC;PRECIPITACAO"), "UTC"))
})

test_that("a failed fallback extraction is reported", {
  zip_path <- create_mock_inmet_data(file.path(tempdir(), "rmet_fallback_fail"), 2020)
  local_mocked_bindings(unz = function(...) stop("cannot open"), .package = "base")
  local_mocked_bindings(unzip = function(...) character(), .package = "utils")
  expect_error(
    rmet:::.read_entry_bytes(zip_path, "missing.CSV", 10, tempdir()),
    "could not be extracted"
  )
})

test_that("daily variables without any valid value are NA", {
  x <- data.frame(
    station_code = "A801",
    state = "RS",
    datetime = as.POSIXct("2020-01-01 04:00", tz = "UTC") + 3600 * (0:23),
    precip_mm = NA_real_,
    temp_dry_c = 20
  )
  daily <- inmet_daily(x)
  expect_true(is.na(daily$precip_mm))
  expect_equal(daily$temp_mean_c, 20)
})
