test_that("outputs have rmet classes and stay data frames", {
  expect_s3_class(rmet_example, c("rmet_hourly", "rmet_data", "data.frame"))
  daily <- inmet_daily(rmet_example)
  expect_s3_class(daily, c("rmet_daily", "rmet_data", "data.frame"))
  expect_s3_class(inmet_monthly(daily), "rmet_monthly")
  expect_true(is.data.frame(daily))
  # the class survives subsetting, flags and derived variables
  expect_s3_class(inmet_qc(rmet_example, quiet = TRUE), "rmet_hourly")
  expect_s3_class(inmet_derive(rmet_example), "rmet_hourly")
  expect_s3_class(rmet_example[1:10, ], "rmet_hourly")
  expect_s3_class(rmet_example[, c("datetime", "temp_dry_c")], "rmet_data")
})

test_that("inmet_read returns an rmet_hourly data frame", {
  cache <- file.path(tempdir(), "rmet-class-read")
  on.exit(unlink(cache, recursive = TRUE), add = TRUE)
  create_mock_inmet_data(cache, year = 2020)
  out <- inmet_read(2020, dest_dir = cache, quiet = TRUE)
  expect_s3_class(out, "rmet_hourly")
})

test_that("print shows a header and a truncated table", {
  expect_output(print(rmet_example), "<rmet hourly> 8,663 rows")
  expect_output(print(rmet_example), "Period: 2023-01-01")
  expect_output(print(rmet_example, n = 3), "more rows")
  expect_invisible(print(rmet_example[1:3, ]))
  # a table without the time column still prints
  expect_output(print(rmet_example[, "temp_dry_c", drop = FALSE]), "<rmet hourly>")
  expect_output(print(inmet_daily(rmet_example)), "<rmet daily>")
})

test_that("print mentions quality-control flag columns", {
  checked <- inmet_qc(rmet_example, quiet = TRUE)
  expect_output(print(checked[1:3, ]), "Quality control")
})

test_that("summary reports one row per station", {
  s <- summary(rmet_example)
  expect_s3_class(s, "summary.rmet_data")
  expect_equal(s$station_code, "A801")
  expect_equal(s$n_rows, nrow(rmet_example))
  expect_true(s$complete_pct > 0 && s$complete_pct <= 100)
  expect_output(print(s), "summary of rmet hourly")
  expect_equal(summary(inmet_daily(rmet_example))$last,
               max(inmet_daily(rmet_example)$date))
})

test_that("as.data.frame removes the rmet classes", {
  expect_identical(class(as.data.frame(rmet_example)), "data.frame")
})
