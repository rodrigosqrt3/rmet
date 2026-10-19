# 30 consecutive hours at one station, with one planted problem per check.
qc_fixture <- function() {
  n <- 30L
  i <- seq_len(n)
  temp <- 20 + (i %% 3) * 0.1
  x <- data.frame(
    station_code = "A801",
    state = "RS",
    datetime = as.POSIXct("2020-01-01 00:00", tz = "UTC") + 3600 * (i - 1),
    temp_dry_c = temp,
    temp_max_prev_c = temp + 0.5,
    temp_min_prev_c = temp - 0.5,
    temp_dew_c = 15 + (i %% 2) * 0.1,
    humid_rel_pct = 60 + (i %% 4),
    precip_mm = 0,
    pressure_station_hpa = 1000 + (i %% 5) * 0.1,
    wind_speed_ms = 2 + (i %% 3) * 0.1,
    wind_gust_ms = 5,
    wind_dir_deg = 90 + i,
    radiation_kjm2 = 100
  )
  x$temp_dry_c[10] <- 35                 # spike (also raise the hour's extremes)
  x$temp_max_prev_c[10] <- 35.5
  x$temp_min_prev_c[10] <- 34.5
  x$temp_max_prev_c[5] <- 10             # maximum below minimum
  x$temp_dew_c[7] <- 25                  # dew point above air temperature
  x$wind_gust_ms[3] <- 1                 # gust below mean speed
  x$humid_rel_pct[2] <- 120              # out of range
  x$humid_rel_pct[20] <- 150             # out of range and spike
  x$radiation_kjm2[25] <- -100           # out of range
  x$precip_mm[15:28] <- 0.2              # stuck rain gauge (14 hours)
  x$wind_dir_deg[1:13] <- 236            # stuck vane while windy
  x$wind_dir_deg[17:30] <- 0             # constant direction, but calm:
  x$wind_speed_ms[17:30] <- 0            #   not flagged
  x
}

test_that("each check flags the planted problem", {
  expect_message(q <- inmet_qc(qc_fixture()), "Flagged")
  expect_equal(which(!is.na(q$temp_dry_c_flag)), c(5L, 10L))
  expect_equal(q$temp_dry_c_flag[10], "spike")
  # with the hour's maximum at 10 degC, the 20 degC reading is inconsistent too
  expect_equal(q$temp_dry_c_flag[5], "consistency")
  expect_equal(q$temp_max_prev_c_flag[5], "consistency")
  expect_equal(q$temp_min_prev_c_flag[5], "consistency")
  expect_equal(q$temp_dew_c_flag[7], "consistency,spike")
  expect_equal(q$wind_gust_ms_flag[3], "consistency")
  expect_equal(q$humid_rel_pct_flag[2], "range,spike")
  expect_equal(q$humid_rel_pct_flag[20], "range,spike")
  expect_equal(q$radiation_kjm2_flag[25], "range")
  expect_equal(which(!is.na(q$precip_mm_flag)), 15:28)
  expect_equal(unique(q$precip_mm_flag[15:28]), "persistence")
  expect_equal(which(!is.na(q$wind_dir_deg_flag)), 1:13)
  expect_true(all(is.na(q$pressure_station_hpa_flag)))

  s <- attr(q, "qc_summary")
  expect_equal(s$flagged[s$variable == "precip_mm" & s$check == "persistence"], 14)
  expect_equal(sort(unique(s$check)), c("consistency", "persistence", "range", "spike"))
})

test_that("remove sets flagged values to NA without adding columns", {
  q <- inmet_qc(qc_fixture(), action = "remove", quiet = TRUE)
  expect_false(any(grepl("_flag$", names(q))))
  expect_true(is.na(q$temp_dry_c[10]))
  expect_true(all(is.na(q$precip_mm[15:28])))
  expect_equal(q$temp_dry_c[11], qc_fixture()$temp_dry_c[11])
})

test_that("checks can be selected", {
  q <- inmet_qc(qc_fixture(), checks = "range", quiet = TRUE)
  expect_true(all(is.na(q$temp_dry_c_flag)))
  expect_equal(q$humid_rel_pct_flag[20], "range")
  expect_equal(unique(attr(q, "qc_summary")$check), "range")
  q <- inmet_qc(qc_fixture(), checks = "persistence", persistence_hours = 20, quiet = TRUE)
  expect_true(all(is.na(q$precip_mm_flag)))
})

test_that("neighbours are only compared within a station and one hour apart", {
  x <- qc_fixture()
  gap <- inmet_qc(x[-9, ], checks = "spike", quiet = TRUE)
  expect_true(all(is.na(gap$temp_dry_c_flag)))

  two <- rbind(x[1:10, ], transform(x[11:30, ], station_code = "A802"))
  q <- inmet_qc(two, checks = "spike", quiet = TRUE)
  expect_true(all(is.na(q$temp_dry_c_flag)))  # hour 10 has no next hour at A801
})

test_that("row order does not change the flags", {
  x <- qc_fixture()
  set.seed(1)
  shuffled <- x[sample(nrow(x)), ]
  q1 <- inmet_qc(x, quiet = TRUE)
  q2 <- inmet_qc(shuffled, quiet = TRUE)
  expect_equal(q2$temp_dry_c_flag, q1$temp_dry_c_flag[as.integer(rownames(shuffled))])
  expect_equal(q2$precip_mm_flag, q1$precip_mm_flag[as.integer(rownames(shuffled))])
})

test_that("data without station codes or wind speed are handled", {
  x <- qc_fixture()
  x$station_code <- NULL
  x$wind_speed_ms <- NULL
  q <- inmet_qc(x, quiet = TRUE)
  expect_equal(which(!is.na(q$temp_dry_c_flag)), c(5L, 10L))
  # without wind speed, a constant direction in calm hours is flagged too
  expect_equal(which(!is.na(q$wind_dir_deg_flag)), c(1:13, 17:30))

  none <- inmet_qc(data.frame(datetime = x$datetime), quiet = TRUE)
  expect_null(attr(none, "qc_summary"))
  expect_equal(names(none), "datetime")
})

test_that("daily summaries ignore flagged values", {
  x <- qc_fixture()
  raw <- inmet_daily(x, tz = "UTC", min_hours = 1)
  checked <- inmet_daily(inmet_qc(x, quiet = TRUE), tz = "UTC", min_hours = 1)
  # UTC day 2020-01-01 = records stamped 01:00 to 24:00 (rows 2 to 25)
  expect_equal(raw$date[2], as.Date("2020-01-01"))
  expect_equal(raw$temp_mean_c[2], mean(x$temp_dry_c[2:25]))
  expect_equal(checked$temp_mean_c[2], mean(x$temp_dry_c[c(2:4, 6:9, 11:25)]))
  expect_equal(raw$precip_mm[2], 0.2 * 11)
  expect_equal(checked$precip_mm[2], 0)
})

test_that("inmet_qc validates its input", {
  expect_error(inmet_qc(data.frame(x = 1)), "POSIXct")
  expect_error(inmet_qc(qc_fixture(), persistence_hours = 0), "positive integer")
  expect_error(inmet_qc(qc_fixture(), checks = "nonsense"), "arg")
  expect_error(inmet_qc(qc_fixture(), quiet = NA), "TRUE or FALSE")
})
