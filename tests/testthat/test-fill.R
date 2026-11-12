hourly_gap_fixture <- function() {
  data.frame(
    station_code = "A801",
    datetime = as.POSIXct("2020-01-01 00:00", tz = "UTC") + 3600 * (0:47),
    temp_dry_c = as.numeric(1:48),
    precip_mm = 0,
    stringsAsFactors = FALSE
  )
}

test_that("short gaps are interpolated and marked", {
  x <- hourly_gap_fixture()
  x$temp_dry_c[10:12] <- NA
  out <- inmet_fill(x, "temp_dry_c", method = "linear")
  expect_equal(out$temp_dry_c, as.numeric(1:48))
  expect_equal(which(out$temp_dry_c_filled == "linear"), 10:12)
  expect_true(all(is.na(out$temp_dry_c_filled[-(10:12)])))
})

test_that("long gaps, edges and sparse series stay missing", {
  x <- hourly_gap_fixture()
  x$temp_dry_c[c(1, 20:25, 48)] <- NA
  out <- inmet_fill(x, "temp_dry_c", method = "linear", max_gap = 3)
  expect_true(all(is.na(out$temp_dry_c[c(1, 20:25, 48)])))
  expect_equal(inmet_fill(x, "temp_dry_c", method = "linear", max_gap = 6)$temp_dry_c[20:25],
               as.numeric(20:25))
  # rows that are absent count as missing steps
  y <- hourly_gap_fixture()[-(10:13), ]
  expect_equal(sum(!is.na(inmet_fill(y, "temp_dry_c", method = "linear", max_gap = 3)$temp_dry_c_filled)), 0L)
  # fewer than two valid values: nothing to interpolate
  z <- hourly_gap_fixture()
  z$temp_dry_c[-1] <- NA
  expect_true(all(is.na(inmet_fill(z, "temp_dry_c", method = "linear")$temp_dry_c[-1])))
})

test_that("the typical value fills the rest, once there are enough years", {
  dates <- as.Date("2018-01-01") + 0:(365 * 5)
  x <- data.frame(station_code = "A801", date = dates,
                  temp_mean_c = 20 + as.numeric(format(dates, "%d")) / 10,
                  stringsAsFactors = FALSE)
  truth <- x$temp_mean_c
  x$temp_mean_c[400:420] <- NA          # 21 days: too long to interpolate
  out <- inmet_fill(x, max_gap = 3)
  expect_false(anyNA(out$temp_mean_c))
  expect_true(all(out$temp_mean_c_filled[400:420] == "climatology"))
  expect_equal(out$temp_mean_c[400:420], truth[400:420], tolerance = 1e-8)

  # not enough years with data for that day of the year
  expect_true(anyNA(inmet_fill(x, min_years = 10)$temp_mean_c))
  # climatology only: short gaps are not interpolated
  y <- x
  y$temp_mean_c[10] <- NA
  expect_equal(inmet_fill(y, method = "climatology")$temp_mean_c_filled[10], "climatology")
  expect_equal(inmet_fill(y, method = "linear")$temp_mean_c_filled[10], "linear")
})

test_that("stations are filled separately and row order is kept", {
  a <- hourly_gap_fixture()
  b <- transform(hourly_gap_fixture(), station_code = "A802", temp_dry_c = 100 + temp_dry_c)
  x <- rbind(a, b)
  x$temp_dry_c[c(5, 53)] <- NA
  x <- x[c(49:96, 1:48), ]               # scrambled station blocks
  out <- inmet_fill(x, "temp_dry_c", method = "linear")
  expect_equal(out$temp_dry_c[out$station_code == "A802"][5], 105)
  expect_equal(out$temp_dry_c[out$station_code == "A801"][5], 5)
  expect_equal(out$station_code, x$station_code)
})

test_that("default variables skip rain, direction and radiation; checks are enforced", {
  x <- hourly_gap_fixture()
  x$wind_dir_deg <- 10
  x$radiation_kjm2 <- 1
  x$precip_mm[3] <- NA
  out <- inmet_fill(x)
  expect_true("temp_dry_c_filled" %in% names(out))
  expect_false(any(c("precip_mm_filled", "wind_dir_deg_filled", "radiation_kjm2_filled") %in% names(out)))
  expect_true("radiation_kjm2_filled" %in% names(inmet_fill(x, "radiation_kjm2")))

  expect_error(inmet_fill(x, "precip_mm"), "cannot be interpolated")
  expect_error(inmet_fill(x, "nothing"), "Unknown variables")
  expect_error(inmet_fill(data.frame(a = 1)), "datetime")
  expect_error(inmet_fill(x, max_gap = 0), "positive integer")
  expect_equal(inmet_fill(data.frame()), data.frame())

  no_station <- x[, names(x) != "station_code"]
  expect_s3_class(inmet_fill(no_station, "temp_dry_c"), "data.frame")
})
