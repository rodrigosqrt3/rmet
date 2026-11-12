year_fixture <- function() {
  dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "day")
  rain <- rep(0, length(dates))
  rain[11:13] <- c(10, 25, 0.5)
  tmax <- rep(30, length(dates))
  tmin <- rep(15, length(dates))
  tmax[100] <- 40
  tmin[c(50, 51)] <- c(-2, 21)
  tmin[60:61] <- 22
  data.frame(station_code = "A801", station_name = "PORTO ALEGRE", date = dates,
             temp_max_c = tmax, temp_min_c = tmin, precip_mm = rain,
             stringsAsFactors = FALSE)
}

test_that("yearly indices have the ETCCDI definitions", {
  out <- inmet_indices(year_fixture())
  expect_equal(nrow(out), 1L)
  expect_equal(out$period, as.Date("2020-01-01"))
  expect_equal(out$n_days, 366L)
  expect_equal(out$txx, 40)
  expect_equal(out$tnn, -2)
  expect_equal(out$frost_days, 1)
  expect_equal(out$summer_days, 366)
  expect_equal(out$tropical_nights, 3)
  expect_equal(out$dtr, mean(year_fixture()$temp_max_c - year_fixture()$temp_min_c))
  expect_equal(out$gdd, sum(pmax((year_fixture()$temp_max_c + year_fixture()$temp_min_c) / 2 - 10, 0)))
  expect_equal(out$prcptot, 35)
  expect_equal(out$rain_days, 2)
  expect_equal(out$r10mm, 2)
  expect_equal(out$r20mm, 1)
  expect_equal(out$sdii, 17.5)
  expect_equal(out$rx1day, 25)
  expect_equal(out$rx5day, 35.5)
  expect_equal(out$cwd, 2)
  expect_equal(out$cdd, 366 - 12 - 0)            # days 13 (0.5 mm) to 366 are dry
  expect_equal(inmet_indices(year_fixture(), base_temp = 20)$gdd,
               sum(pmax((year_fixture()$temp_max_c + year_fixture()$temp_min_c) / 2 - 20, 0)))
})

test_that("monthly indices and missing days follow min_valid", {
  out <- inmet_indices(year_fixture(), by = "month")
  expect_equal(nrow(out), 12L)
  expect_equal(out$prcptot[1:2], c(35, 0))
  expect_equal(out$sdii[2], 0)
  expect_equal(out$cwd[2], 0)

  x <- year_fixture()
  x$temp_max_c[1:200] <- NA
  yearly <- inmet_indices(x)
  expect_true(is.na(yearly$txx))
  expect_equal(yearly$prcptot, 35)
  # days that are absent count as missing, and a partial year is not enough
  partial <- inmet_indices(year_fixture()[1:200, ])
  expect_true(is.na(partial$prcptot))
  expect_equal(inmet_indices(year_fixture()[1:200, ], min_valid = 0.5)$prcptot, 35)
  # rolling sums need five valid days in a row
  sparse <- data.frame(date = as.Date("2020-01-01") + 0:9,
                       precip_mm = rep(c(1, NA), 5))
  expect_true(is.na(inmet_indices(sparse, by = "month", min_valid = 0.4)$rx5day))
  expect_true(is.na(inmet_indices(transform(sparse, precip_mm = NA_real_), min_valid = 0)$prcptot))
})

test_that("indices work with one kind of variable and several stations", {
  x <- year_fixture()
  only_rain <- inmet_indices(x[, c("station_code", "date", "precip_mm")])
  expect_false("txx" %in% names(only_rain))
  only_temp <- inmet_indices(x[, c("date", "temp_max_c", "temp_min_c")])
  expect_false("prcptot" %in% names(only_temp))
  expect_false("station_code" %in% names(only_temp))

  two <- rbind(x, transform(x, station_code = "A802"))
  out <- inmet_indices(two)
  expect_equal(out$station_code, c("A801", "A802"))
})

test_that("indices validate their input", {
  expect_error(inmet_indices(data.frame(a = 1)), "date")
  expect_error(inmet_indices(data.frame(date = Sys.Date(), a = 1)), "precip_mm")
  expect_error(inmet_indices(year_fixture(), min_valid = 2), "between 0 and 1")
  expect_error(inmet_indices(year_fixture(), by = "week"), "should be one of")
  expect_equal(inmet_indices(data.frame()), data.frame())
})

monthly_rain <- function(years = 30, station = "A") {
  set.seed(42)
  months <- seq(as.Date("1990-01-01"), by = "month", length.out = 12 * years)
  rain <- rgamma(length(months), shape = 2, scale = 50)
  rain[sample(length(rain), 10)] <- 0
  data.frame(station_code = station, month = months, precip_mm = rain,
             stringsAsFactors = FALSE)
}

test_that("SPI is standard normal over the reference period", {
  out <- inmet_spi(monthly_rain(), scale = 1)
  expect_equal(attr(out, "spi_scale"), 1L)
  expect_lt(abs(mean(out$spi)), 0.1)
  expect_equal(sd(out$spi), 1, tolerance = 0.1)
  # a very wet month is positive, a dry one negative
  expect_gt(out$spi[which.max(out$precip_mm)], 1)
  expect_lt(out$spi[which.min(out$precip_mm)], 0)

  three <- inmet_spi(monthly_rain(), scale = 3)
  expect_true(all(is.na(three$spi[1:2])))
  expect_false(anyNA(three$spi[-(1:2)]))
})

test_that("SPI needs enough history and complete accumulations", {
  expect_true(all(is.na(inmet_spi(monthly_rain(10), scale = 1)$spi)))
  expect_false(anyNA(inmet_spi(monthly_rain(10), scale = 1, min_years = 5)$spi))
  gap <- monthly_rain()[-100, ]
  out <- inmet_spi(gap, scale = 3)
  # the first two months, and the two present months whose accumulation includes the gap
  expect_equal(sum(is.na(out$spi)), 2 + 2)
  both <- rbind(monthly_rain(station = "A"), monthly_rain(station = "B"))
  expect_equal(inmet_spi(both, scale = 1)$spi[1:360], inmet_spi(monthly_rain(), scale = 1)$spi)
  no_station <- monthly_rain()[, c("month", "precip_mm")]
  expect_false(anyNA(inmet_spi(no_station, scale = 1)$spi))
  # degenerate distributions have no SPI
  constant <- transform(monthly_rain(), precip_mm = 100)
  expect_true(all(is.na(inmet_spi(constant, scale = 1)$spi)))
  dry <- transform(monthly_rain(), precip_mm = 0)
  expect_true(all(is.na(inmet_spi(dry, scale = 1)$spi)))

  expect_error(inmet_spi(data.frame(a = 1)), "inmet_monthly")
  expect_error(inmet_spi(monthly_rain(), scale = 0), "positive integer")
  expect_equal(inmet_spi(data.frame()), data.frame())
})
