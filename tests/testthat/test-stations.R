station_api_fixture <- function(optional_codes = TRUE) {
  out <- data.frame(
    CD_ESTACAO = c("B828", "A801", "A801", ""),
    DC_NOME = c("ACEGUA", "PORTO ALEGRE", "DUPLICATE", "EMPTY"),
    SG_ESTADO = c("RS", "RS", "RS", "RS"),
    VL_LATITUDE = c("-31.87", "-30.05", "-30.05", "-30"),
    VL_LONGITUDE = c("-54.11", "-51.17", "-51.17", "-50"),
    VL_ALTITUDE = c("271", "46.97", "46.97", "10"),
    DT_INICIO_OPERACAO = c(
      "2025-09-16T21:00:00.000-03:00",
      "2000-09-22T21:00:00.000-03:00",
      "2000-09-22T21:00:00.000-03:00",
      ""
    ),
    DT_FIM_OPERACAO = c(NA, "", NA, NA),
    CD_SITUACAO = c("Pane", "Operante", "Operante", "Pane"),
    SG_ENTIDADE = c("INMET", "INMET", "INMET", "INMET"),
    stringsAsFactors = FALSE
  )
  if (optional_codes) {
    out$CD_WSI <- c(NA, "wsi", "wsi", NA)
    out$CD_OSCAR <- c(NA, "oscar", "oscar", NA)
  }
  out
}

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

test_that("station interface validates options and filters entities", {
  expect_error(inmet_stations(source = "bundled", max_age = -1), "non-negative")
  expect_error(inmet_stations(source = "bundled", refresh = TRUE), "cannot be used")
  expect_error(
    inmet_stations(source = "bundled", search = c("a", "b")),
    "single text"
  )
  expect_error(inmet_stations(source = "unknown"), "arg")

  result <- inmet_stations(source = "bundled", entity = "inmet")
  expect_true(nrow(result) > 0L)
  expect_true(all(result$entity == "INMET"))
})

test_that("remote station catalogue is fetched, standardized, and cached", {
  raw <- station_api_fixture(optional_codes = FALSE)
  json <- jsonlite::toJSON(
    raw, dataframe = "rows", auto_unbox = TRUE, na = "null"
  )
  local_mocked_bindings(
    curl_fetch_memory = function(...) {
      list(status_code = 200L, content = charToRaw(json))
    },
    .package = "curl"
  )
  fetched <- rmet:::.fetch_station_catalog()
  expect_equal(nrow(fetched), 2L)
  expect_true(all(is.na(fetched$wsi_code)))

  td <- file.path(tempdir(), "rmet_remote_cache")
  unlink(td, recursive = TRUE)
  local_mocked_bindings(
    .fetch_station_catalog = function() fetched,
    .package = "rmet"
  )
  result <- rmet:::.load_stations(
    source = "auto", refresh = TRUE, cache_dir = td, quiet = TRUE
  )
  expect_equal(attr(result, "catalog_source"), "INMET API")
  expect_true(file.exists(file.path(td, "stations.rds")))

  cached <- rmet:::.load_stations(
    source = "auto", refresh = FALSE, max_age = 30,
    cache_dir = td, quiet = TRUE
  )
  expect_equal(attr(cached, "catalog_source"), "cache")
})

test_that("station catalogue handles HTTP and schema failures", {
  local_mocked_bindings(
    curl_fetch_memory = function(...) {
      list(status_code = 503L, content = raw())
    },
    .package = "curl"
  )
  expect_error(rmet:::.fetch_station_catalog(), "HTTP 503")

  expect_error(
    rmet:::.standardize_remote_stations(data.frame(CD_ESTACAO = "A001")),
    "missing"
  )
  expect_false(rmet:::.valid_station_catalog(NULL))
  expect_false(rmet:::.valid_station_catalog(data.frame()))
  expect_false(rmet:::.valid_station_catalog(data.frame(station_code = "A001")))
  expect_true(rmet:::.valid_station_catalog(
    data.frame(
      station_code = "A001", station_name = "X", state = "DF",
      latitude = 0, longitude = 0
    )
  ))
  expect_true(is.na(rmet:::.api_date(c("", NA_character_)))[1])
  expect_equal(
    unname(rmet:::.state_regions()[c("RS", "DF")]),
    c("S", "CO")
  )
})

test_that("station catalogue falls back through every cache path", {
  cached <- rmet:::.load_bundled_stations()
  td <- file.path(tempdir(), "rmet_stale_catalog")
  unlink(td, recursive = TRUE)
  dir.create(td)
  saveRDS(cached, file.path(td, "stations.rds"))

  local_mocked_bindings(
    .fetch_station_catalog = function() stop("offline"),
    .package = "rmet"
  )
  expect_warning(
    stale_remote <- rmet:::.load_stations(
      source = "remote", refresh = TRUE, cache_dir = td, quiet = FALSE
    ),
    "older cached"
  )
  expect_equal(attr(stale_remote, "catalog_source"), "stale cache")

  expect_warning(
    stale_auto <- rmet:::.load_stations(
      source = "auto", refresh = TRUE, cache_dir = td, quiet = TRUE
    ),
    "older cached"
  )
  expect_equal(attr(stale_auto, "catalog_source"), "stale cache")

  empty <- file.path(tempdir(), "rmet_no_catalog")
  unlink(empty, recursive = TRUE)
  expect_error(
    rmet:::.load_stations(
      source = "remote", refresh = TRUE, cache_dir = empty, quiet = TRUE
    ),
    "could not be retrieved"
  )
  expect_warning(
    fallback <- rmet:::.load_stations(
      source = "auto", refresh = TRUE, cache_dir = empty, quiet = TRUE
    ),
    "bundled station catalogue"
  )
  expect_equal(attr(fallback, "catalog_source"), "bundled fallback")
})

test_that("station cache uses copy fallback when rename is unavailable", {
  remote <- rmet:::.standardize_remote_stations(station_api_fixture())
  td <- file.path(tempdir(), "rmet_catalog_copy")
  unlink(td, recursive = TRUE)
  local_mocked_bindings(
    .fetch_station_catalog = function() remote,
    .package = "rmet"
  )
  local_mocked_bindings(file.rename = function(...) FALSE, .package = "base")
  result <- rmet:::.load_stations(
    source = "auto", refresh = TRUE, cache_dir = td, quiet = TRUE
  )
  expect_true(file.exists(file.path(td, "stations.rds")))
  expect_equal(attr(result, "catalog_source"), "INMET API")
})
