test_that("vapour pressure and deficit follow the FAO-56 formulas", {
  x <- data.frame(temp_dry_c = c(20, 30), temp_dew_c = c(10, 30),
                  humid_rel_pct = c(50, 100))
  out <- inmet_derive(x, c("vapor_pressure_kpa", "vpd_kpa"))
  expect_equal(out$vapor_pressure_kpa[1], 0.6108 * exp(17.27 * 10 / 247.3))
  expect_equal(out$vpd_kpa[2], 0)
  expect_gt(out$vpd_kpa[1], 0)
  # without a dew point, humidity is used
  out <- inmet_derive(x[c("temp_dry_c", "humid_rel_pct")], "vapor_pressure_kpa")
  expect_equal(out$vapor_pressure_kpa[1], 0.6108 * exp(17.27 * 20 / 257.3) * 0.5)
})

test_that("wind components use the meteorological convention", {
  x <- data.frame(wind_speed_ms = c(10, 10, 10, 10), wind_dir_deg = c(0, 90, 180, 270))
  out <- inmet_derive(x, c("wind_u_ms", "wind_v_ms"))
  expect_equal(out$wind_u_ms, c(0, -10, 0, 10), tolerance = 1e-12)
  expect_equal(out$wind_v_ms, c(-10, 0, 10, 0), tolerance = 1e-12)
})

test_that("heat index matches the NWS table and is close to the temperature when mild", {
  # NWS table: 90 F and 70 % humidity feel like about 106 F (41.1 C)
  out <- inmet_derive(data.frame(temp_dry_c = (90 - 32) * 5 / 9, humid_rel_pct = 70),
                      "heat_index_c")
  expect_equal(out$heat_index_c * 9 / 5 + 32, 106, tolerance = 0.02)
  mild <- inmet_derive(data.frame(temp_dry_c = 15, humid_rel_pct = 50), "heat_index_c")
  expect_lt(abs(mild$heat_index_c - 15), 2)
})

test_that("flagged values are ignored and missing inputs are reported", {
  x <- data.frame(temp_dry_c = c(20, 20), humid_rel_pct = c(50, 50),
                  temp_dry_c_flag = c(NA, "range"))
  out <- inmet_derive(x, "vpd_kpa")
  expect_false(is.na(out$vpd_kpa[1]))
  expect_true(is.na(out$vpd_kpa[2]))
  expect_message(inmet_derive(data.frame(a = 1), "wind_u_ms"), "skipped")
  expect_message(inmet_derive(data.frame(a = 1), "vapor_pressure_kpa"), "skipped")
  expect_message(inmet_derive(data.frame(temp_dry_c = 20), "vpd_kpa"), "skipped")
  expect_message(inmet_derive(data.frame(temp_dry_c = 20), "heat_index_c"), "skipped")
  expect_equal(inmet_derive(data.frame()), data.frame())
  expect_error(inmet_derive(list(a = 1)), "data frame")
})

test_that("inmet_et0 falls back to humidity on days without a dew point", {
  daily <- data.frame(
    date = as.Date("2020-07-06") + 0:1, latitude = -30, elevation = 50,
    temp_max_c = 25, temp_min_c = 15, temp_dew_mean_c = c(12, NA),
    humid_rel_mean_pct = 70, wind_speed_mean_ms = 2, radiation_mj_m2 = 12
  )
  out <- inmet_et0(daily)
  expect_false(anyNA(out$et0_mm))
  expect_error(inmet_et0(daily[, setdiff(names(daily),
                                         c("temp_dew_mean_c", "humid_rel_mean_pct"))]),
               "temp_dew_mean_c")
  # humidity alone is enough
  expect_false(anyNA(inmet_et0(daily[, names(daily) != "temp_dew_mean_c"])$et0_mm))
})

test_that("inmet_et0 reproduces FAO-56 example 18", {
  # Uccle (Brussels), 6 July: latitude 50.80 N, elevation 100 m,
  # Tmax 21.5, Tmin 12.3, ea 1.409 kPa, u2 2.078 m/s, Rs 22.07 MJ/m2; ET0 = 3.9 mm/day
  ea <- 1.409
  dew <- 237.3 * log(ea / 0.6108) / (17.27 - log(ea / 0.6108))
  daily <- data.frame(
    date = as.Date("2020-07-06"), latitude = 50.80, elevation = 100,
    temp_max_c = 21.5, temp_min_c = 12.3, temp_dew_mean_c = dew,
    wind_speed_mean_ms = 2.078 / (4.87 / log(67.8 * 10 - 5.42)),
    radiation_mj_m2 = 22.07
  )
  expect_equal(inmet_et0(daily)$et0_mm, 3.9, tolerance = 0.05)
})

test_that("inmet_et0 gives NA for missing inputs and validates its input", {
  daily <- inmet_et0(inmet_daily(rmet_example))
  expect_true("et0_mm" %in% names(daily))
  expect_true(all(daily$et0_mm >= 0 | is.na(daily$et0_mm)))
  expect_gt(sum(!is.na(daily$et0_mm)), 300)
  expect_lt(max(daily$et0_mm, na.rm = TRUE), 12)
  expect_error(inmet_et0(data.frame(date = Sys.Date())), "inmet_daily")
  expect_equal(inmet_et0(data.frame()), data.frame())
})
