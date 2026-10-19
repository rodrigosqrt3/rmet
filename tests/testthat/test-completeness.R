test_that("completeness counts valid hours per station and period", {
  stamps <- as.POSIXct("2020-02-01 00:00", tz = "UTC") + 3600 * 0:99
  x <- data.frame(
    station_code = "A801",
    datetime = stamps,
    temp_dry_c = 20,
    precip_mm = c(rep(NA, 30), rep(0, 70)),
    station_name = "PORTO ALEGRE"
  )
  out <- inmet_completeness(x)
  expect_equal(out$period, "2020-02")
  expect_equal(out$hours_expected, 29L * 24L)   # leap year
  expect_equal(out$hours_present, 100L)
  expect_equal(out$temp_dry_c, round(100 * 100 / 696, 1))
  expect_equal(out$precip_mm, round(100 * 70 / 696, 1))
  expect_false("station_name" %in% names(out))

  yearly <- inmet_completeness(x, by = "year", variables = "precip_mm")
  expect_equal(yearly$hours_expected, 366L * 24L)
  expect_equal(names(yearly), c("station_code", "period", "hours_expected",
                                "hours_present", "precip_mm"))
})

test_that("flagged values do not count as valid", {
  x <- data.frame(
    station_code = c("B1", "A1", "A1"),
    datetime = as.POSIXct(c("2021-12-31 23:00", "2021-12-31 23:00", "2022-01-01 00:00"), tz = "UTC"),
    temp_dry_c = c(1, 2, 3),
    temp_dry_c_flag = c(NA, "range", NA)
  )
  out <- inmet_completeness(x, by = "year")
  expect_equal(out$station_code, c("A1", "A1", "B1"))
  expect_equal(out$period, c("2021", "2022", "2021"))
  expect_equal(out$temp_dry_c, c(0, round(100 / 8760, 1), round(100 / 8760, 1)))
  expect_false("temp_dry_c_flag" %in% names(out))
})

test_that("completeness works without station codes and validates input", {
  x <- data.frame(datetime = as.POSIXct("2020-12-31 12:00", tz = "UTC"), precip_mm = 1)
  out <- inmet_completeness(x)
  expect_true(is.na(out$station_code))
  expect_equal(out$hours_expected, 31L * 24L)
  expect_equal(nrow(inmet_completeness(x[0, ])), 0L)
  expect_error(inmet_completeness(x, variables = "nope"), "Unknown variables: nope")
  expect_error(inmet_completeness(data.frame(a = 1)), "POSIXct")
  expect_error(inmet_completeness(x, by = "week"), "arg")
})
