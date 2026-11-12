test_that("availability lists stations and periods from file names only", {
  cache <- file.path(tempdir(), "rmet-availability")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(
    cache, list(inmet_lines_2019(), inmet_lines_2019(), inmet_lines_2019()),
    year = 2020,
    csv_name = c("INMET_S_RS_A801_PORTO ALEGRE_01-01-2020_A_31-08-2020.CSV",
                 "INMET_S_RS_A801_PORTO ALEGRE_01-01-2020_A_30-09-2020.CSV",
                 "INMET_CO_DF_A001_BRASILIA_01-01-2020_A_31-12-2020.CSV")
  )
  out <- inmet_availability(dest_dir = cache)
  expect_equal(out$station_code, c("A001", "A801"))
  expect_equal(out$state, c("DF", "RS"))
  expect_equal(out$year, c(2020L, 2020L))
  expect_equal(out$n_files, c(1L, 2L))
  expect_equal(out$end, as.Date(c("2020-12-31", "2020-09-30")))
  expect_equal(out$start, rep(as.Date("2020-01-01"), 2))

  expect_equal(inmet_availability(2020, stations = "a801", dest_dir = cache)$station_code, "A801")
  expect_warning(inmet_availability(2019, dest_dir = cache), "No valid archive")
})

test_that("archives without recognisable station files add no rows", {
  cache <- file.path(tempdir(), "rmet-availability-names")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_zip(cache, list(inmet_lines_2019()), year = 2020,
                  csv_name = "notes.CSV")
  expect_equal(nrow(inmet_availability(dest_dir = cache)), 0L)
})

test_that("availability of an empty cache is an empty data frame", {
  out <- inmet_availability(dest_dir = file.path(tempdir(), "rmet-no-such-dir"))
  expect_equal(nrow(out), 0L)
  expect_named(out, c("station_code", "state", "year", "start", "end", "n_files"))
})
