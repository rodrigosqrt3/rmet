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

  inmet_cache_clear(years = 2020, dest_dir = test_cache, ask = FALSE)
  expect_false(file.exists(mock_zip))
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
