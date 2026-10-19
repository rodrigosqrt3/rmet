test_that("states map to their historical time zones", {
  expect_equal(
    inmet_timezone(c("RS", "rs ", "MT", "MS", "AC", "RO", "RR", "TO", "BA", "XX")),
    c("America/Sao_Paulo", "America/Sao_Paulo", "America/Cuiaba", "America/Campo_Grande",
      "America/Rio_Branco", "America/Porto_Velho", "America/Boa_Vista",
      "America/Araguaina", "America/Bahia", NA)
  )
  # split states, using INMET station coordinates
  expect_equal(
    inmet_timezone(c("AM", "AM", "AM", "PA", "PA", "PE", "PE"),
                   c(-69.87, -67.33, -67.06, -54.72, -48.44, -32.42, -34.95)),
    c("America/Eirunepe", "America/Eirunepe", "America/Manaus",
      "America/Santarem", "America/Belem", "America/Noronha", "America/Recife")
  )
  expect_equal(inmet_timezone("AM"), "America/Manaus")  # unknown longitude
  expect_true(all(inmet_timezone(names(rmet:::.state_regions())) %in% OlsonNames()))
})

test_that("the station catalogue includes each station's time zone", {
  st <- inmet_stations(source = "bundled")
  zone <- function(code) st$time_zone[st$station_code == code]
  expect_equal(zone("A801"), "America/Sao_Paulo")
  expect_equal(zone("A109"), "America/Eirunepe")   # Eirunepe, AM
  expect_equal(zone("A134"), "America/Manaus")     # Sao Gabriel da Cachoeira, AM
  expect_false(anyNA(st$time_zone))
})

test_that("automatic time zones give each station its own local days", {
  stamps <- as.POSIXct("2020-01-02 04:00", tz = "UTC") + 3600 * c(0, 2)
  x <- data.frame(
    station_code = c("A801", "A801", "A108", "A108"),
    state = c("RS", "RS", "AC", "AC"),
    longitude = c(-51.2, -51.2, -72.7, -72.7),
    datetime = rep(stamps, 2),
    precip_mm = 1
  )
  daily <- inmet_daily(x, min_hours = 1)
  # hour midpoints 03:30 and 05:30 UTC: both on Jan 2 in Porto Alegre (UTC-3);
  # 22:30 on Jan 1 and 00:30 on Jan 2 in Acre (UTC-5)
  expect_equal(daily$date[daily$station_code == "A801"], as.Date("2020-01-02"))
  expect_equal(daily$date[daily$station_code == "A108"], as.Date(c("2020-01-01", "2020-01-02")))
  expect_equal(unique(daily$time_zone[daily$station_code == "A108"]), "America/Rio_Branco")

  fixed <- inmet_daily(x, tz = "America/Sao_Paulo", min_hours = 1)
  expect_equal(unique(fixed$time_zone), "America/Sao_Paulo")
  expect_equal(unique(fixed$date), as.Date("2020-01-02"))

  expect_error(inmet_daily(x[, -2]), "needs a `state` column")
  x$state[1] <- "ZZ"
  expect_error(inmet_daily(x), "Unknown state\\(s\\): ZZ")
})
