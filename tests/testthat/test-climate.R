three_years <- function(station = "A", offset = 0) {
  months <- seq(as.Date("2020-01-01"), by = "month", length.out = 36)
  data.frame(station_code = station, month = months,
             temp_mean_c = offset + rep(1:12, 3) + rep(0:2, each = 12),
             precip_mm = rep(c(10, 20, 30), each = 12),
             n_days = 30L, stringsAsFactors = FALSE)
}

test_that("normals average each calendar month over the years", {
  normals <- inmet_normals(three_years(), min_years = 3)
  expect_equal(nrow(normals), 12L)
  expect_equal(normals$month_of_year, 1:12)
  expect_equal(normals$n_years, rep(3L, 12))
  expect_equal(normals$temp_mean_c, (1:12) + 1)
  expect_equal(normals$precip_mm, rep(20, 12))
  expect_false("n_days" %in% names(normals))
  expect_equal(names(normals)[1:3], c("station_code", "month_of_year", "n_years"))
})

test_that("normals respect years, variables and min_years", {
  expect_equal(inmet_normals(three_years(), years = 2020:2021, min_years = 2)$precip_mm,
               rep(15, 12))
  expect_true(all(is.na(inmet_normals(three_years(), years = 2020:2021, min_years = 3)$precip_mm)))
  only <- inmet_normals(three_years(), variables = "precip_mm", min_years = 1)
  expect_false("temp_mean_c" %in% names(only))
  two <- inmet_normals(rbind(three_years("A"), three_years("B", 5)), min_years = 3)
  expect_equal(two$station_code, rep(c("A", "B"), each = 12))
  expect_equal(two$temp_mean_c[13:24] - two$temp_mean_c[1:12], rep(5, 12))
  no_station <- three_years()[, c("month", "temp_mean_c")]
  expect_equal(nrow(inmet_normals(no_station, min_years = 3)), 12L)
  expect_equal(inmet_normals(three_years(), years = 1950), data.frame())
  expect_error(inmet_normals(three_years(), years = "a"), "whole years")
  expect_error(inmet_normals(three_years(), years = 1990.5), "whole years")

  expect_error(inmet_normals(data.frame(a = 1)), "inmet_monthly")
  expect_error(inmet_normals(three_years(), variables = "nothing"), "Unknown variables")
  expect_error(inmet_normals(three_years(), min_years = 0), "positive integer")
  expect_equal(inmet_normals(data.frame()), data.frame())
})

test_that("anomalies are differences or percentages of the normal", {
  x <- three_years()
  out <- inmet_anomaly(x, min_years = 3)
  expect_equal(out$temp_mean_c_anom, rep(c(-1, 0, 1), each = 12))
  expect_equal(out$precip_mm_anom, rep(c(-10, 0, 10), each = 12))
  relative <- inmet_anomaly(x, variables = "precip_mm", relative = TRUE, min_years = 3)
  expect_equal(relative$precip_mm_anom, rep(c(50, 100, 150), each = 12))
  expect_false("temp_mean_c_anom" %in% names(relative))

  # normals from a reference period, given explicitly
  reference <- inmet_normals(x, years = 2020, min_years = 1)
  based <- inmet_anomaly(x, normals = reference)
  expect_equal(based$temp_mean_c_anom, rep(0:2, each = 12))
  # stations are matched by code; stations without normals get NA
  other <- inmet_anomaly(rbind(x, three_years("Z")), normals = reference)
  expect_true(all(is.na(other$temp_mean_c_anom[other$station_code == "Z"])))
  no_station <- x[, c("month", "temp_mean_c")]
  expect_equal(nrow(inmet_anomaly(no_station, min_years = 3)), 36L)
})

test_that("anomaly validates its input", {
  x <- three_years()
  expect_error(inmet_anomaly(data.frame(a = 1)), "inmet_monthly")
  expect_error(inmet_anomaly(x, normals = data.frame(a = 1)), "inmet_normals")
  expect_error(inmet_anomaly(x, normals = inmet_normals(x, variables = "precip_mm", min_years = 1),
                             variables = "temp_mean_c"), "missing")
  expect_error(inmet_anomaly(x, relative = NA), "TRUE or FALSE")
  expect_equal(inmet_anomaly(data.frame()), data.frame())
})

test_that("plot draws hourly, daily and monthly data", {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  expect_invisible(plot(rmet_example))
  expect_invisible(plot(rmet_example, variable = "precip_mm", ylab = "mm", main = "Rain"))
  daily <- inmet_daily(rmet_example)
  expect_invisible(plot(daily))
  monthly <- inmet_monthly(daily)
  expect_invisible(plot(monthly, variable = "precip_mm", type = "b"))
  # several stations get a legend; station filters are case-insensitive
  two <- rbind(three_years("A"), three_years("B", 5))
  class(two) <- c("rmet_monthly", "rmet_data", "data.frame")
  expect_invisible(plot(two))
  expect_invisible(plot(two, station = "b"))
  many <- do.call(rbind, lapply(LETTERS[1:10], function(s) three_years(s)))
  class(many) <- c("rmet_monthly", "rmet_data", "data.frame")
  expect_invisible(plot(many))

  expect_error(plot(two, station = "Z"), "No data")
  expect_error(plot(two, variable = "nothing"), "measurement column")
  no_time <- two[, c("station_code", "temp_mean_c")]
  expect_error(plot(no_time), "no `datetime`")
  expect_error(plot(two[, c("month"), drop = FALSE]), "measurement column")
})
