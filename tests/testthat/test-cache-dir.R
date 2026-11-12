test_that("the cache directory follows the option, then the environment variable", {
  default <- tools::R_user_dir("rmet", "cache")
  withr::local_options(rmet.cache_dir = NULL)
  withr::local_envvar(RMET_CACHE_DIR = NA)
  expect_equal(inmet_cache_dir(), default)

  withr::local_envvar(RMET_CACHE_DIR = file.path(tempdir(), "from-env"))
  expect_equal(inmet_cache_dir(), path.expand(file.path(tempdir(), "from-env")))

  withr::local_options(rmet.cache_dir = file.path(tempdir(), "from-option"))
  expect_equal(inmet_cache_dir(), path.expand(file.path(tempdir(), "from-option")))
})

test_that("functions use the configured cache directory by default", {
  cache <- file.path(tempdir(), "rmet-option-cache")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_inmet_data(cache, year = 2020)
  withr::local_options(rmet.cache_dir = cache)
  expect_equal(inmet_cache_status()$year, 2020L)
  expect_gt(nrow(inmet_read(2020, quiet = TRUE)), 0L)
})

test_that("functions that take hourly data accept the empty result of inmet_read()", {
  empty <- data.frame()
  expect_equal(inmet_daily(empty), data.frame())
  expect_equal(inmet_completeness(empty), data.frame())
  expect_equal(inmet_qc(empty), empty)
})
