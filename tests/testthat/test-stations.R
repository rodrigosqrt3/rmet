test_that("inmet_stations returns expected format and all stations by default", {
  st <- inmet_stations()

  expect_s3_class(st, "data.frame")
  expect_true(nrow(st) > 100)

  expected_cols <- c("station_code", "station_name", "region",
                     "state", "latitude", "longitude", "elevation", "start_year")
  expect_true(all(expected_cols %in% names(st)))

  expect_equal(rownames(st)[1], "1")
})

test_that("inmet_stations filtering works correctly", {
  st_s <- inmet_stations(region = "S")
  expect_true(all(st_s$region == "S"))

  st_rs <- inmet_stations(state = "RS")
  expect_true(all(st_rs$state == "RS"))

  st_poa <- inmet_stations(search = "porto alegre")
  expect_true(any(grepl("PORTO ALEGRE", st_poa$station_name)))

  st_combo <- inmet_stations(region = "S", state = "RS", search = "alegre")
  expect_true(nrow(st_combo) > 0)
  expect_true(all(st_combo$state == "RS"))
})

test_that("inmet_stations handles empty results gracefully", {
  st_empty <- inmet_stations(search = "XYZZY_NON_EXISTENT_STATION")

  expect_s3_class(st_empty, "data.frame")
  expect_equal(nrow(st_empty), 0)
})
