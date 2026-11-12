test_that("inmet_nearest handles many locations at once", {
  near <- inmet_nearest(c(-30.05, -15.8), c(-51.17, -47.9), n = 2, source = "bundled")
  expect_equal(nrow(near), 4L)
  expect_equal(near$location, c(1L, 1L, 2L, 2L))
  expect_equal(near$rank, c(1L, 2L, 1L, 2L))
  expect_true(all(near$distance_km[c(1, 3)] <= near$distance_km[c(2, 4)]))
  expect_equal(near$station_code[[1]], "A801")
  # one location keeps the plain layout
  expect_false("location" %in% names(inmet_nearest(-30.05, -51.17, n = 1, source = "bundled")))

  expect_error(inmet_nearest(c(-30, -31), -51), "same length")
  expect_error(inmet_nearest(NA_real_, -51), "latitude")
  expect_error(inmet_nearest(-30, 200), "longitude")
  expect_error(inmet_nearest(numeric(), numeric()), "latitude")
})

test_that("stations inside a bounding box are selected", {
  box <- inmet_stations_within(c(-52, -31, -50, -29), source = "bundled")
  expect_true("A801" %in% box$station_code)
  expect_true(all(box$longitude >= -52 & box$longitude <= -50))
  expect_true(all(box$latitude >= -31 & box$latitude <= -29))
  expect_equal(nrow(inmet_stations_within(c(0, 0, 1, 1), source = "bundled")), 0L)

  expect_error(inmet_stations_within(c(-50, -31, -52, -29), source = "bundled"), "west < east")
  expect_error(inmet_stations_within(c(1, 2, 3), source = "bundled"), "west, south")
  expect_error(inmet_stations_within("RS", source = "bundled"), "sf polygon")
})

test_that("stations inside an sf polygon are selected", {
  skip_if_not_installed("sf")
  box <- sf::st_as_sfc(sf::st_bbox(c(xmin = -52, ymin = -31, xmax = -50, ymax = -29),
                                   crs = 4326))
  inside <- inmet_stations_within(box, source = "bundled")
  expect_equal(inside$station_code,
               inmet_stations_within(c(-52, -31, -50, -29), source = "bundled")$station_code)
  projected <- sf::st_transform(box, 31982)
  expect_equal(nrow(inmet_stations_within(sf::st_sf(geometry = projected), source = "bundled")),
               nrow(inside))
})

test_that("a polygon needs sf", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(inmet_stations_within(structure(list(), class = "sfc"), source = "bundled"),
               "install.packages")
})

test_that("inmet_interpolate weights nearby stations", {
  stations <- data.frame(latitude = c(0, 0), longitude = c(0, 2), value = c(10, 20))
  targets <- data.frame(id = 1:3, latitude = c(0, 0, 0), longitude = c(1, 0, 50))
  out <- inmet_interpolate(stations, targets, "value", k = 2)
  expect_equal(out$value[[1]], 15)
  expect_equal(out$value[[2]], 10)                  # on top of a station
  expect_equal(out$n_stations, c(2L, 2L, 2L))
  expect_equal(out$nearest_km[[2]], 0)
  expect_named(out, c("id", "latitude", "longitude", "value", "n_stations", "nearest_km"))

  # closer stations weigh more; k limits the stations used
  closer <- inmet_interpolate(stations, data.frame(latitude = 0, longitude = 0.5), "value")
  expect_lt(closer$value, 12)
  expect_equal(inmet_interpolate(stations, data.frame(latitude = 0, longitude = 0.5),
                                 "value", k = 1)$n_stations, 1L)
  # range limit and missing values
  far <- inmet_interpolate(stations, targets, "value", max_distance_km = 200)
  expect_true(is.na(far$value[[3]]))
  expect_equal(far$n_stations[[3]], 0L)
  empty <- inmet_interpolate(transform(stations, value = NA_real_), targets, "value")
  expect_true(all(is.na(empty$value)))
  expect_true(all(is.na(empty$nearest_km)))
})

test_that("inmet_interpolate validates its input", {
  stations <- data.frame(latitude = 0, longitude = 0, value = 1)
  targets <- data.frame(latitude = 0, longitude = 1)
  expect_error(inmet_interpolate(data.frame(a = 1), targets, "a"), "`x` must")
  expect_error(inmet_interpolate(stations, data.frame(a = 1), "value"), "`targets` must")
  expect_error(inmet_interpolate(stations, targets, "nothing"), "numeric column")
  expect_error(inmet_interpolate(stations, targets, c("value", "value")), "numeric column")
  expect_error(inmet_interpolate(stations, targets, "value", power = 0), "power")
  expect_error(inmet_interpolate(stations, targets, "value", k = 0), "positive integer")
  expect_error(inmet_interpolate(stations, targets, "value", max_distance_km = -1), "max_distance")
})
