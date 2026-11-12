monthly_temperature <- function(temp, latitude = 0, station = "A", years = 1) {
  months <- seq(as.Date("2020-01-01"), by = "month", length.out = 12 * years)
  data.frame(station_code = station, latitude = latitude, month = months,
             temp_mean_c = rep(temp, length.out = length(months)),
             stringsAsFactors = FALSE)
}

test_that("Thornthwaite potential evapotranspiration follows the method", {
  # constant 20 degC at the equator: heat index 97.8, a = 2.139, 73.9 mm per
  # 30-day month of 12 daylight hours
  out <- inmet_pet_thornthwaite(monthly_temperature(20))
  expect_equal(out$pet_mm[[1]], 73.9 * 31 / 30, tolerance = 0.01)
  expect_equal(out$pet_mm[[2]], 73.9 * 29 / 30, tolerance = 0.01)   # February 2020

  seasonal <- inmet_pet_thornthwaite(monthly_temperature(c(26, 24, 22, 18, 14, 11, 10, 12, 14, 18, 21, 24), -30))
  expect_gt(seasonal$pet_mm[[1]], seasonal$pet_mm[[7]])
  # hot months use the polynomial for T >= 26.5; freezing months have no evaporation
  hot <- inmet_pet_thornthwaite(monthly_temperature(c(28, rep(20, 11))))
  expect_gt(hot$pet_mm[[1]], 0)
  cold <- inmet_pet_thornthwaite(monthly_temperature(c(-5, rep(10, 11))))
  expect_equal(cold$pet_mm[[1]], 0)
  expect_equal(inmet_pet_thornthwaite(monthly_temperature(-3))$pet_mm, rep(0, 12))
})

test_that("Thornthwaite needs the 12 calendar months and the right input", {
  incomplete <- monthly_temperature(20)[1:6, ]
  expect_true(all(is.na(inmet_pet_thornthwaite(incomplete)$pet_mm)))
  two <- rbind(monthly_temperature(20, station = "A"),
               monthly_temperature(15, station = "B")[1:3, ])
  out <- inmet_pet_thornthwaite(two)
  expect_false(anyNA(out$pet_mm[out$station_code == "A"]))
  expect_true(all(is.na(out$pet_mm[out$station_code == "B"])))
  no_station <- monthly_temperature(20)[, c("latitude", "month", "temp_mean_c")]
  expect_false(anyNA(inmet_pet_thornthwaite(no_station)$pet_mm))
  expect_error(inmet_pet_thornthwaite(data.frame(a = 1)), "month")
  expect_equal(inmet_pet_thornthwaite(data.frame()), data.frame())
})

test_that("the water balance follows Thornthwaite and Mather", {
  x <- data.frame(precip_mm = c(0, 200, NA, 0), pet_mm = c(50, 50, 50, 50))
  out <- inmet_water_balance(x, awc = 100)
  expect_equal(out$storage_mm[[1]], 100 * exp(-0.5))
  expect_equal(out$etr_mm[[1]], 100 - 100 * exp(-0.5))
  expect_equal(out$deficit_mm[[1]], 50 - out$etr_mm[[1]])
  expect_equal(out$excess_mm[[1]], 0)
  expect_equal(out$storage_mm[[2]], 100)
  expect_equal(out$etr_mm[[2]], 50)
  expect_equal(out$excess_mm[[2]], 150 - (100 - 100 * exp(-0.5)))
  expect_true(all(is.na(c(out$storage_mm[[3]], out$etr_mm[[3]], out$deficit_mm[[3]], out$excess_mm[[3]]))))
  # the missing period leaves the storage unchanged for the next one
  expect_equal(out$storage_mm[[4]], 100 * exp(-0.5))
})

test_that("water balance conserves water and handles stations and options", {
  set.seed(1)
  x <- data.frame(station_code = rep(c("A", "B"), each = 24),
                  precip_mm = runif(48, 0, 200), pet_mm = runif(48, 20, 120))
  out <- inmet_water_balance(x, awc = 80, initial = 40)
  expect_true(all(out$storage_mm >= 0 & out$storage_mm <= 80))
  expect_equal(out$deficit_mm, x$pet_mm - out$etr_mm)
  expect_true(all(out$excess_mm >= 0))
  # P - ETR = change in storage + excess, within each station
  change <- ave(out$storage_mm, out$station_code, FUN = function(s) diff(c(NA, s)))
  inner <- !is.na(change)
  expect_equal((x$precip_mm - out$etr_mm - out$excess_mm)[inner], change[inner])
  # a separate series starts from the initial storage again
  expect_equal(out$storage_mm[[25]], inmet_water_balance(x[25, ], awc = 80, initial = 40)$storage_mm)
  # another column can supply the evapotranspiration
  y <- transform(x, et0_mm = pet_mm)
  expect_equal(inmet_water_balance(y, pet = "et0_mm", awc = 80, initial = 40)$etr_mm, out$etr_mm)
})

test_that("water balance validates its input", {
  x <- data.frame(precip_mm = 1, pet_mm = 1)
  expect_error(inmet_water_balance(x, awc = 0), "awc")
  expect_error(inmet_water_balance(x, awc = 50, initial = 60), "initial")
  expect_error(inmet_water_balance(x, pet = c("a", "b")), "one column")
  expect_error(inmet_water_balance(x, pet = "other"), "other")
  expect_error(inmet_water_balance(data.frame(a = 1)), "precip_mm")
  expect_equal(inmet_water_balance(data.frame()), data.frame())
})

test_that("radiation units convert both ways", {
  expect_equal(inmet_convert_radiation(20, "mj_m2", "w_m2"), 20 / 0.0864)
  expect_equal(inmet_convert_radiation(20, "mj_m2", "mm"), 20 * 0.408)
  expect_equal(inmet_convert_radiation(inmet_convert_radiation(20, "mj_m2", "cal_cm2"),
                                       "cal_cm2", "mj_m2"), 20)
  expect_equal(inmet_convert_radiation(1000, "kj_m2", "mj_m2"), 1)
  expect_equal(inmet_convert_radiation(100, "j_cm2", "mj_m2"), 1)
  expect_error(inmet_convert_radiation(1, "mj_m2", "btu"), "should be one of")
  expect_error(inmet_convert_radiation("1", "mj_m2", "w_m2"), "numeric")
})
