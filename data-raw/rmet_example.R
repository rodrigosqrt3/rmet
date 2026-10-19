# Regenerates data/rmet_example.rda: one year (2023) of hourly data from
# station A801 (Porto Alegre - Jardim Botanico), as returned by inmet_read()
# with default settings (UTC timestamps). Run from the package root:
#
#   Rscript data-raw/rmet_example.R

pkgload::load_all(".", quiet = TRUE)

cache <- file.path(tempdir(), "rmet-example")
rmet_example <- inmet_get(2023, stations = "A801", dest_dir = cache)
stopifnot(
  nrow(rmet_example) > 8000,
  all(rmet_example$station_code == "A801"),
  attr(rmet_example$datetime, "tzone") == "UTC",
  # air temperature extremes must bracket the hourly air temperature
  mean(rmet_example$temp_max_prev_c >= rmet_example$temp_dry_c, na.rm = TRUE) > 0.99
)
usethis::use_data(rmet_example, overwrite = TRUE, compress = "xz")
