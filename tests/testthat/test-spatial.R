test_that("stations become sf points in WGS 84", {
  skip_if_not_installed("sf")
  st <- inmet_nearest(-30.05, -51.17, n = 2, source = "bundled")
  points <- inmet_as_sf(st)
  expect_s3_class(points, "sf")
  expect_equal(sf::st_crs(points)$epsg, 4326L)
  expect_equal(nrow(points), 2L)
  expect_true("latitude" %in% names(points))
  expect_equal(unname(sf::st_coordinates(points)[1, ]), c(st$longitude[1], st$latitude[1]))
  expect_false("latitude" %in% names(inmet_as_sf(st, remove = TRUE)))
})

test_that("rows without coordinates are dropped with a warning", {
  skip_if_not_installed("sf")
  x <- data.frame(station_code = c("A", "B"), latitude = c(-30, NA), longitude = c(-51, -50))
  expect_warning(points <- inmet_as_sf(x), "1 row")
  expect_equal(points$station_code, "A")
})

test_that("inmet_as_sf validates input and needs sf", {
  expect_error(inmet_as_sf(data.frame(x = 1)), "latitude")
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(inmet_as_sf(data.frame(latitude = 1, longitude = 1)), "install.packages")
})
