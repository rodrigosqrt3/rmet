uccle <- function() {
  # FAO-56 example 18: Uccle, 6 July, 50.80 N, 100 m
  ea <- 1.409
  dew <- 237.3 * log(ea / 0.6108) / (17.27 - log(ea / 0.6108))
  data.frame(
    date = as.Date("2020-07-06"), latitude = 50.80, elevation = 100,
    temp_max_c = 21.5, temp_min_c = 12.3, temp_dew_mean_c = dew,
    wind_speed_mean_ms = 2.078 / (4.87 / log(67.8 * 10 - 5.42)),
    radiation_mj_m2 = 22.07
  )
}

test_that("Hargreaves-Samani needs only temperature and is close to FAO-56", {
  daily <- uccle()[, c("date", "latitude", "temp_max_c", "temp_min_c")]
  out <- inmet_et0(daily, method = "hargreaves")
  expect_equal(out$et0_mm, 4.0, tolerance = 0.1)
  expect_error(inmet_et0(daily[, c("date", "latitude")], method = "hargreaves"),
               "temp_max_c")
})

test_that("radiation can be estimated from the temperature range", {
  daily <- uccle()
  no_radiation <- daily[, names(daily) != "radiation_mj_m2"]
  expect_error(inmet_et0(no_radiation), "radiation_mj_m2")

  estimated <- inmet_et0(no_radiation, estimate_radiation = TRUE)
  expect_equal(estimated$et0_mm, 3.9, tolerance = 0.25)
  coastal <- inmet_et0(no_radiation, estimate_radiation = TRUE, coastal = TRUE)
  expect_gt(coastal$et0_mm, estimated$et0_mm)

  # measured values are kept; only missing days use the estimate
  both <- rbind(daily, transform(daily, radiation_mj_m2 = NA_real_))
  out <- inmet_et0(both, estimate_radiation = TRUE)
  expect_equal(out$et0_mm[[1]], inmet_et0(daily)$et0_mm)
  expect_equal(out$et0_mm[[2]], estimated$et0_mm)
  expect_true(is.na(inmet_et0(both)$et0_mm[[2]]))

  expect_error(inmet_et0(daily, estimate_radiation = "yes"), "TRUE or FALSE")
  expect_error(inmet_et0(daily, coastal = NA), "TRUE or FALSE")
  expect_error(inmet_et0(daily, method = "other"), "should be one of")
})

test_that("design evapotranspiration is a percentile per group", {
  daily <- data.frame(station_code = rep(c("A", "B"), each = 5),
                      et0_mm = c(1:5, c(10, 20, 30, 40, NA)))
  design <- inmet_design_et0(daily, percentile = 0.5, by = "station_code")
  expect_equal(design$station_code, c("A", "B"))
  expect_equal(design$n_days, c(5L, 4L))
  expect_equal(design$et0_design_mm, c(3, 25))
  expect_equal(inmet_design_et0(daily)$n_days, 9L)
  expect_true(is.na(inmet_design_et0(data.frame(et0_mm = NA_real_))$et0_design_mm))

  expect_error(inmet_design_et0(daily, by = "nothing"), "Unknown grouping")
  expect_error(inmet_design_et0(data.frame(a = 1)), "et0_mm")
  expect_error(inmet_design_et0(daily, percentile = 2), "between 0 and 1")
  expect_equal(inmet_design_et0(data.frame()), data.frame())
})
