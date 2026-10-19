# Hourly records stamped at the END of each hour, in UTC, as in INMET files.
# Brasilia time is UTC-3 in 2020 (no daylight saving after 2019).
hourly_fixture <- function() {
  stamps <- as.POSIXct("2020-01-01 03:00", tz = "UTC") + 3600 * (0:48)
  data.frame(
    station_code = "A801",
    station_name = "PORTO ALEGRE",
    state = "RS",
    longitude = -51.17,
    datetime = stamps,
    temp_dry_c = seq_along(stamps) / 2,
    temp_max_prev_c = seq_along(stamps) / 2 + 0.5,
    temp_min_prev_c = seq_along(stamps) / 2 - 0.5,
    precip_mm = 1,
    radiation_kjm2 = rep(c(-2, 100), length.out = length(stamps)),
    stringsAsFactors = FALSE
  )
}

test_that("records are assigned to the local day containing the middle of the hour", {
  daily <- inmet_daily(hourly_fixture(), min_hours = 1)
  # 03:00 UTC covers 23:00-00:00 local on Dec 31; 04:00..03:00 UTC is Jan 1
  expect_equal(daily$date, as.Date(c("2019-12-31", "2020-01-01", "2020-01-02")))
  expect_equal(daily$n_hours, c(1L, 24L, 24L))
  expect_equal(daily$precip_mm, c(1, 24, 24))
  expect_equal(daily$temp_max_c[2], 25 / 2 + 0.5)  # rows 2..25
  expect_equal(daily$temp_min_c[2], 2 / 2 - 0.5)
  expect_equal(daily$temp_mean_c[2], mean((2:25) / 2))
})

test_that("days with too few hours give NA", {
  x <- hourly_fixture()
  x$precip_mm[30:40] <- NA  # 11 missing hours on Jan 2
  daily <- inmet_daily(x, min_hours = 20)
  expect_true(is.na(daily$temp_max_c[1]))       # 1 hour on Dec 31
  expect_equal(daily$precip_mm[2], 24)
  expect_true(is.na(daily$precip_mm[3]))        # 13 valid hours < 20
  expect_false(is.na(daily$temp_max_c[3]))      # temperature still complete
})

test_that("UTC days can be requested explicitly", {
  daily <- inmet_daily(hourly_fixture(), tz = "UTC", min_hours = 1)
  expect_equal(daily$date[1], as.Date("2020-01-01"))
  expect_equal(daily$n_hours[1], 22L)  # stamps 03:00..24:00 -> midpoints 02:30..23:30
})

test_that("air temperature is used when previous-hour extremes are absent", {
  x <- hourly_fixture()[, c("station_code", "datetime", "temp_dry_c")]
  daily <- inmet_daily(x, tz = "America/Sao_Paulo", min_hours = 1)
  expect_equal(daily$temp_max_c[2], 25 / 2)
  expect_equal(daily$temp_min_c[2], 2 / 2)
  expect_false("precip_mm" %in% names(daily))
})

test_that("radiation sums positive values in MJ/m2", {
  daily <- inmet_daily(hourly_fixture(), min_hours = 20)
  # Jan 1 = rows 2..25: 12 rows of 100 kJ/m2 and 12 of -2 (clipped to 0)
  expect_equal(daily$radiation_mj_m2[2], 1.2)
  expect_true(is.na(daily$radiation_mj_m2[1]))
})

test_that("stations are summarised separately and sorted", {
  a <- hourly_fixture()
  b <- hourly_fixture()
  b$station_code <- "A001"
  b$precip_mm <- 2
  daily <- inmet_daily(rbind(a, b), min_hours = 1)
  expect_equal(daily$station_code, rep(c("A001", "A801"), each = 3))
  expect_equal(daily$precip_mm[daily$station_code == "A001"], c(2, 48, 48))
})

test_that("inmet_daily validates its input", {
  expect_error(inmet_daily(data.frame(x = 1)), "POSIXct")
  expect_error(inmet_daily(hourly_fixture(), tz = "Nowhere/City"), "Unknown timezone")
  expect_error(inmet_daily(hourly_fixture(), min_hours = 0), "positive integer")
  expect_equal(nrow(inmet_daily(hourly_fixture()[0, ])), 0L)
})

test_that("the example dataset gives plausible daily values", {
  daily <- inmet_daily(rmet_example)
  full <- daily[daily$n_hours >= 20, ]
  expect_gt(nrow(full), 300L)
  expect_true(all(full$temp_max_c >= full$temp_mean_c, na.rm = TRUE))
  expect_true(all(full$temp_mean_c >= full$temp_min_c, na.rm = TRUE))
  expect_true(all(format(full$date, "%Y") == "2023"))
})
