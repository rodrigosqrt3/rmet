# Regenerates inst/extdata/stations.csv, the offline fallback catalogue, from
# the official INMET API. Run from the package root before each release:
#
#   Rscript data-raw/stations.R
#
# The bundled file must always come from the API. Never edit it by hand.

pkgload::load_all(".", quiet = TRUE)

stations <- .fetch_station_catalog()
stopifnot(
  nrow(stations) > 500,
  !anyDuplicated(stations$station_code),
  # spot checks against well-known stations
  stations$station_name[stations$station_code == "A801"] == "PORTO ALEGRE - JARDIM BOTANICO",
  stations$state[stations$station_code == "A801"] == "RS",
  stations$state[stations$station_code == "A001"] == "DF"
)

out <- stations[.station_columns()]
for (col in c("start_date", "end_date")) out[[col]] <- format(out[[col]])
path <- file.path("inst", "extdata", "stations.csv")
con <- file(path, open = "w", encoding = "UTF-8")
writeLines(paste0("# snapshot: ", format(Sys.Date())), con)
utils::write.csv(out, con, row.names = FALSE, na = "")
close(con)

message("Wrote ", nrow(out), " stations to ", path)
