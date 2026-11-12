cache_lines <- function(extra = character()) {
  c("REGIAO:;S", "UF:;RS", "ESTACAO:;PORTO ALEGRE", "CODIGO (WMO):;A801",
    "LATITUDE:;-30,05", "LONGITUDE:;-51,16", "ALTITUDE:;46,97",
    "Data;Hora UTC;PRECIPITACAO TOTAL (mm);TEMP BULBO SECO (C);",
    "2020/01/01;0000 UTC;1,5;20,1;",
    "2020/01/01;0100 UTC;;;",                 # nothing measured
    "2020/01/02;0000 UTC;2,5;21,2;",
    "2020/01/03;0000 UTC;3,5;22,3;",
    extra)
}

parsed_dir <- function(cache, year = 2020) file.path(cache, "parsed", year)

test_that("a cached read equals an uncached read, whatever the request", {
  cache <- file.path(tempdir(), "rmet-parsed-equal")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines())

  requests <- list(
    list(),
    list(variables = "precip_mm"),
    list(drop_empty = FALSE),
    list(tz = "America/Sao_Paulo"),
    list(start_date = "2020-01-02"),
    list(start_date = "2020-01-02", end_date = "2020-01-02", variables = "temp_dry_c"),
    list(stations = "a801", drop_empty = FALSE, tz = "America/Manaus")
  )
  for (request in requests) {
    plain <- do.call(inmet_read, c(list(2020, dest_dir = cache, quiet = TRUE), request))
    first <- do.call(inmet_read, c(list(2020, dest_dir = cache, quiet = TRUE, cache = TRUE), request))
    again <- do.call(inmet_read, c(list(2020, dest_dir = cache, quiet = TRUE, cache = TRUE), request))
    expect_identical(first, plain)
    expect_identical(again, plain)
  }
  expect_length(list.files(parsed_dir(cache)), 1L)
})

test_that("the second read uses the cache and does not parse again", {
  cache <- file.path(tempdir(), "rmet-parsed-hit")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines())
  first <- inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)

  local_mocked_bindings(.parse_inmet_all = function(...) stop("must not parse"),
                        .package = "rmet")
  expect_message(
    again <- inmet_read(2020, dest_dir = cache, cache = TRUE),
    "parsed cache"
  )
  expect_identical(again, first)
  # without the cache the parser is needed
  expect_warning(inmet_read(2020, dest_dir = cache, quiet = TRUE), "Could not parse")
})

test_that("invalid timestamps are reported on every read", {
  cache <- file.path(tempdir(), "rmet-parsed-warn")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines("garbage;0000 UTC;9;9;"))
  expect_warning(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE),
                 "1 row\\(s\\) have an invalid timestamp")
  expect_warning(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE),
                 "1 row\\(s\\) have an invalid timestamp")
})

test_that("a changed archive makes the cache stale", {
  cache <- file.path(tempdir(), "rmet-parsed-stale")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines())
  expect_equal(nrow(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)), 3L)

  # a new version of the archive, with one more day, replaces the old one
  keep <- parsed_dir(cache)
  moved <- file.path(tempdir(), "rmet-parsed-keep")
  file.copy(keep, dirname(moved), recursive = TRUE)
  file.rename(file.path(dirname(moved), "2020"), moved)
  create_mock_zip(cache, cache_lines("2020/01/04;0000 UTC;4,5;23,4;"))
  dir.create(dirname(keep), recursive = TRUE)
  file.copy(moved, dirname(keep), recursive = TRUE)
  file.rename(file.path(dirname(keep), basename(moved)), keep)
  unlink(moved, recursive = TRUE)

  out <- inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)
  expect_equal(nrow(out), 4L)
  expect_equal(nrow(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)), 4L)
})

test_that("damaged or foreign cache files are ignored and replaced", {
  cache <- file.path(tempdir(), "rmet-parsed-bad")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines())
  expected <- inmet_read(2020, dest_dir = cache, quiet = TRUE)
  inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)
  path <- list.files(parsed_dir(cache), full.names = TRUE)

  writeLines("this is not an rds file", path)
  expect_identical(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE), expected)
  expect_true(is.list(readRDS(path)))                        # repaired

  saveRDS("not a list", path)
  expect_identical(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE), expected)

  object <- readRDS(path)
  for (field in c("version", "entry", "stamp")) {
    altered <- object
    altered[[field]] <- "other"
    saveRDS(altered, path)
    expect_identical(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE), expected)
    expect_identical(readRDS(path)[[field]], object[[field]])   # replaced by a valid one
  }
})

test_that("files without data and failed parses are handled", {
  cache <- file.path(tempdir(), "rmet-parsed-empty")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  header_only <- cache_lines()[1:8]
  create_mock_zip(cache, header_only)
  expect_warning(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE), "No data")
  expect_length(list.files(parsed_dir(cache)), 1L)        # "no rows" is remembered
  local_mocked_bindings(.parse_inmet_all = function(...) stop("must not parse"),
                        .package = "rmet")
  expect_warning(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE), "No data")

  # a file that cannot be parsed is not cached
  failing <- file.path(tempdir(), "rmet-parsed-failing")
  on.exit(unlink(failing, recursive = TRUE), add = TRUE)
  create_mock_zip(failing, cache_lines())
  local_mocked_bindings(.parse_inmet_all = function(...) stop("broken file"),
                        .package = "rmet")
  expect_warning(
    expect_warning(inmet_read(2020, dest_dir = failing, cache = TRUE, quiet = TRUE),
                   "Could not parse"),
    "No data"
  )
  expect_length(list.files(parsed_dir(failing)), 0L)
})

test_that("failing to write the cache does not stop the read", {
  cache <- file.path(tempdir(), "rmet-parsed-write")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines())
  expected <- inmet_read(2020, dest_dir = cache, quiet = TRUE)

  local_mocked_bindings(saveRDS = function(...) stop("disk full"), .package = "base")
  expect_identical(inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE), expected)
  expect_length(list.files(parsed_dir(cache)), 0L)
})

test_that("a cache file is copied into place when it cannot be renamed", {
  cache <- file.path(tempdir(), "rmet-parsed-rename")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines())
  local_mocked_bindings(file.rename = function(...) FALSE, .package = "base")
  inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)
  files <- list.files(parsed_dir(cache))
  expect_length(files, 1L)
  expect_match(files, "\\.rds$")
  expect_false(any(grepl("^parsed-", files)))             # no temporary file left
})

test_that("inmet_get passes the cache option on and reports the parsed size", {
  cache <- file.path(tempdir(), "rmet-parsed-get")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines())
  out <- inmet_get(2020, dest_dir = cache, cache = TRUE, check_updates = FALSE, quiet = TRUE)
  expect_equal(nrow(out), 3L)
  expect_length(list.files(parsed_dir(cache)), 1L)

  status <- inmet_cache_status(dest_dir = cache)
  expect_true("parsed_mb" %in% names(status))
  expect_gte(status$parsed_mb, 0)
  expect_equal(inmet_cache_status(dest_dir = file.path(cache, "none"))$parsed_mb, numeric())
  expect_error(inmet_read(2020, dest_dir = cache, cache = "yes"), "TRUE or FALSE")
})

test_that("the parsed files are removed on request only", {
  cache <- file.path(tempdir(), "rmet-parsed-clear")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, cache_lines())
  inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)

  suppressMessages(inmet_cache_clear(2020, dest_dir = cache, ask = FALSE, catalog = FALSE))
  expect_length(list.files(parsed_dir(cache)), 1L)        # the archive went, the parsed file stayed

  create_mock_zip(cache, cache_lines())
  inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)
  expect_message(inmet_cache_clear(2020, dest_dir = cache, ask = FALSE, parsed = TRUE),
                 "Deleted 2/2")
  expect_false(dir.exists(parsed_dir(cache)))

  # every year at once
  create_mock_zip(cache, cache_lines())
  inmet_read(2020, dest_dir = cache, cache = TRUE, quiet = TRUE)
  suppressMessages(inmet_cache_clear(dest_dir = cache, ask = FALSE, parsed = TRUE))
  expect_false(dir.exists(file.path(cache, "parsed", "2020")))
  expect_error(inmet_cache_clear(dest_dir = cache, parsed = NA), "TRUE or FALSE")
})
