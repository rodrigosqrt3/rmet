daily_fixture <- function() {
  dates <- as.Date("2020-01-01") + 0:61  # January and February 2020 (29 days)
  data.frame(
    station_code = "A801",
    station_name = "PORTO ALEGRE",
    state = "RS",
    date = dates,
    temp_max_c = 30,
    temp_min_c = 20,
    temp_mean_c = 25,
    precip_mm = rep(c(0, 2), length.out = length(dates)),
    stringsAsFactors = FALSE
  )
}

test_that("monthly values summarise the daily values", {
  monthly <- inmet_monthly(daily_fixture(), min_days = 1)
  expect_equal(monthly$month, as.Date(c("2020-01-01", "2020-02-01", "2020-03-01")))
  expect_equal(monthly$n_days, c(31L, 29L, 2L))
  expect_equal(monthly$temp_max_mean_c, rep(30, 3))
  expect_equal(monthly$temp_min_abs_c, rep(20, 3))
  expect_equal(monthly$precip_mm[1], 2 * 15)       # 15 wet days in January
  expect_equal(monthly$rain_days[1], 15)
})

test_that("months with too few valid days give NA", {
  x <- daily_fixture()
  x$precip_mm[1:15] <- NA
  monthly <- inmet_monthly(x, min_days = 20)
  expect_true(is.na(monthly$precip_mm[1]))          # 16 valid days
  expect_false(is.na(monthly$precip_mm[2]))
  expect_true(is.na(monthly$temp_mean_c[3]))        # 2 days in March
})

test_that("inmet_monthly works on real daily output and rejects hourly data", {
  monthly <- inmet_monthly(inmet_daily(rmet_example))
  # 2023 UTC starts on the evening of 2022-12-31 in local time: 13 local months
  expect_equal(format(monthly$month[c(1, nrow(monthly))]), c("2022-12-01", "2023-12-01"))
  expect_equal(nrow(monthly), 13L)
  expect_error(inmet_monthly(rmet_example), "inmet_daily")
  expect_error(inmet_monthly(daily_fixture(), min_days = 0), "positive integer")
  expect_equal(inmet_monthly(data.frame()), data.frame())
  expect_equal(inmet_monthly(daily_fixture()[0, ]), data.frame())
})
