test_that("inmet_stations returns expected format and all stations by default", {
  st <- inmet_stations(source = "bundled")

  expect_s3_class(st, "data.frame")
  expect_true(nrow(st) > 100)

  expected_cols <- c("station_code", "station_name", "region",
                     "state", "latitude", "longitude", "elevation", "start_year")
  expect_true(all(expected_cols %in% names(st)))

  expect_equal(rownames(st)[1], "1")
})

test_that("inmet_stations filtering works correctly", {
  st_s <- inmet_stations(region = "S", source = "bundled")
  expect_true(all(st_s$region == "S"))

  st_rs <- inmet_stations(state = "RS", source = "bundled")
  expect_true(all(st_rs$state == "RS"))

  st_poa <- inmet_stations(search = "porto alegre", source = "bundled")
  expect_true(any(grepl("PORTO ALEGRE", st_poa$station_name)))

  st_combo <- inmet_stations(region = "S", state = "RS", search = "alegre", source = "bundled")
  expect_true(nrow(st_combo) > 0)
  expect_true(all(st_combo$state == "RS"))
})

test_that("inmet_stations handles empty results gracefully", {
  st_empty <- inmet_stations(search = "XYZZY_NON_EXISTENT_STATION", source = "bundled")

  expect_s3_class(st_empty, "data.frame")
  expect_equal(nrow(st_empty), 0)
})

test_that("official API fields are standardized", {
  raw <- data.frame(
    CD_ESTACAO = c("B828", "A801"),
    DC_NOME = c("ACEGUA", "PORTO ALEGRE"),
    SG_ESTADO = c("RS", "RS"),
    VL_LATITUDE = c("-31.87", "-30.05"),
    VL_LONGITUDE = c("-54.11", "-51.17"),
    VL_ALTITUDE = c("271", "46.97"),
    DT_INICIO_OPERACAO = c("2025-09-16T21:00:00.000-03:00", "2000-09-22T21:00:00.000-03:00"),
    DT_FIM_OPERACAO = c(NA, NA),
    CD_SITUACAO = c("Pane", "Operante"),
    SG_ENTIDADE = c("INMET", "INMET"),
    CD_WSI = c(NA, "wsi"),
    CD_OSCAR = c(NA, "oscar"),
    stringsAsFactors = FALSE
  )
  result <- rmet:::.standardize_remote_stations(raw)
  expect_equal(nrow(result), 2L)
  expect_true(all(result$region == "S"))
  expect_s3_class(result$start_date, "Date")
  expect_true(all(c("status", "entity", "start_year") %in% names(result)))
})

test_that("auto station source uses and filters a cached official catalogue", {
  td <- file.path(tempdir(), "rmet_station_cache")
  dir.create(td, recursive = TRUE, showWarnings = FALSE)
  cached <- rmet:::.load_bundled_stations()
  cached$status <- "Operante"
  saveRDS(cached, file.path(td, "stations.rds"))

  result <- inmet_stations(
    state = "RS", status = "operante", source = "auto", max_age = 1e6,
    cache_dir = td
  )
  expect_true(nrow(result) > 0L)
  expect_true(all(result$state == "RS"))
})

test_that("station searches are accent-insensitive", {
  result <- inmet_stations(search = "gravatai", source = "bundled")
  expect_true(any(grepl("GRAVATA", result$station_name)))
})
