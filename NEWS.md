# rmet 0.3.0

## Important corrections

* **The bundled station catalogue was wrong in rmet 0.1.0 to 0.2.0.** Only 8
  of its 117 rows matched INMET's official catalogue: 67 codes do not exist
  and 42 pointed to a different station in another state (for example, A801
  was listed as Sao Luis, MA instead of Porto Alegre, RS). In 0.1.0 it was the
  only source used by `inmet_stations()`; in 0.2.0 it was used offline, with
  `source = "bundled"`, and in examples. It is now a snapshot of the official
  catalogue (669 stations, all columns), regenerated from the INMET API by
  `data-raw/stations.R`, and tests check it against known stations. Results
  obtained from the old catalogue should be re-checked.
* Station `start_date` and `end_date` were one day early: the API timestamps
  (e.g. `2000-09-21T21:00-03:00`) were truncated instead of converted. They are
  now UTC calendar dates (`2000-09-22` for A801).
* The example dataset `rmet_example` was built with rmet 0.1.0 and had the
  dew-point extremes in the `temp_max_c` and `temp_min_c` columns, and
  timestamps in Sao Paulo time. It has been regenerated with the current
  parser (UTC timestamps, `temp_dew_max_c` and `temp_dew_min_c`).
* **Hours of the current year were duplicated.** INMET's current-year archive
  holds cumulative versions of each station file (for example January to
  August and January to September 2026), and every version was read: for
  A801 in 2026, 5,747 of 12,214 rows were duplicates, which doubles any sum
  over those hours. Each station and hour is now returned once, from the most
  recent version (INMET occasionally revises values between versions). As a
  consequence, `inmet_read()` output is now sorted by station code and time.
* `station_name` was `NA` for files whose header reads `ESTAC?O:`, a damaged
  spelling present in INMET's 2019 archive.
* Date columns are now parsed with a format chosen by exact pattern. Trying
  formats in turn could read a day-first date such as `04-01-2020` as year 4.
* Cached archives of the current and previous year were never refreshed,
  although INMET keeps appending data to them. `inmet_download()` (and
  `inmet_get()`) now check the server version of these archives and download
  updates (`check_updates = TRUE`).
* The vignette computed daily values with `as.Date(datetime)`, i.e. UTC days,
  and daily extremes from instantaneous temperatures. It now uses
  `inmet_daily()`.

## New features

* `inmet_daily()` builds daily summaries per station and local day: maximum
  and minimum temperature from the previous-hour extremes, means, totals,
  gusts and radiation, with a `min_hours` completeness rule and an `n_hours`
  column.
* `inmet_nearest()` finds the stations closest to a location.
* `inmet_read()` and `inmet_get()` gain `drop_empty`; `FALSE` keeps hours in
  which every measurement is missing.
* `inmet_extract()` gains `stations` to extract only some station files.
* `inmet_cache_status()` reports when each archive was downloaded.
* New vignette: "Time Zones, Missing Hours and Daily Data".

## Performance and reliability

* Station files are read directly from the archives, without extracting them
  to disk, and parsed with vectorised code. Reading a full year (545 stations,
  4 million rows) takes about half the time of 0.2.0, with identical values.
* Downloads check the final size against the size announced by the server, so
  truncated files are no longer accepted, and recover when a server ignores a
  resume request instead of corrupting the archive.
* Files are decoded line by line as UTF-8 or Latin-1, so archives in either
  encoding are read correctly.
* All measurement columns are now `double` (some were `integer` before).

## Internal

* Code reorganised into `validate.R`, `parse.R`, `read.R`, `download.R`,
  `stations.R`, `daily.R` and `cache.R`, with comments on the INMET format.

# rmet 0.2.0

## New features

* Added `inmet_get()` as the recommended one-call download-and-read workflow.
* `inmet_stations()` can now retrieve and cache the complete official INMET
  automatic-station catalogue. New filters cover operational status and
  provider entity; a bundled catalogue remains available as an offline fallback.
* Station metadata now includes `start_date`, `end_date`, `status`, `entity`,
  `wsi_code`, and `oscar_code`. The existing `start_year` column is retained.
* Cache inspection reports partial downloads, and cache clearing can remove the
  cached station catalogue.

## Correctness and reliability

* Changed the default output time zone from `"America/Sao_Paulo"` to `"UTC"`.
  INMET historical files record UTC, while Brazilian stations span multiple
  time zones. Users can still request any Olson time zone explicitly.
* Fixed column matching that could classify dew-point maximum and minimum
  temperatures as air-temperature maximum and minimum values.
* Added support for station codes longer than four characters.
* Added strict validation for years, dates, flags, directories, and time zones.
* Added support for `2400 UTC` timestamps and explicit warnings for unparseable
  timestamps.
* Downloads now reuse valid archives without contacting the server, write to
  `.part` files, resume interrupted transfers, validate ZIP structure, and only
  then publish the final archive.
* Temporary extraction now uses an isolated directory that is always cleaned.
  Unsafe archive paths are rejected.
* Corrected the package HTTP user agent and added HTTP timeout/error handling.

## Documentation

* Made `install.packages("rmet")` the primary installation instruction.
* Corrected station-column names in the vignette and clarified timestamp,
  filtering, caching, and raw-data quality behavior.

# rmet 0.1.0

* Initial CRAN release with archive download, parsing, extraction, station
  lookup, and cache-management helpers.
