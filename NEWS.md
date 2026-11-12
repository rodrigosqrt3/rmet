# rmet 0.4.0

## New features

* `inmet_monthly()` aggregates `inmet_daily()` output into monthly values per
  station (mean and absolute extremes of temperature, precipitation and rain
  days, humidity, wind, radiation), with a `min_days` completeness rule.
* `inmet_et0()` adds the FAO-56 Penman-Monteith reference evapotranspiration
  (`et0_mm`) to daily data; it reproduces the FAO-56 worked example 18. With
  `method = "hargreaves"` it needs only temperature, and
  `estimate_radiation = TRUE` fills missing radiation from the temperature
  range. `inmet_design_et0()` gives the design value (a percentile, by default
  0.8) per group.
* `inmet_pet_thornthwaite()` adds monthly potential evapotranspiration from
  temperature, and `inmet_water_balance()` computes the Thornthwaite-Mather
  soil water balance (storage, actual evapotranspiration, deficit and excess).
* `inmet_fill()` fills short gaps of hourly or daily data by linear
  interpolation and the rest with the typical value of the day of the year,
  marking every filled value in `<variable>_filled`. Rainfall and wind
  direction cannot be filled.
* `inmet_indices()` computes yearly or monthly climate indices from daily data
  (ETCCDI-style temperature and precipitation indices, dry and wet spells,
  growing degree days), `inmet_spi()` the Standardized Precipitation Index, and
  `inmet_normals()` and `inmet_anomaly()` climatological normals and
  anomalies.
* `inmet_nearest()` accepts many locations at once (for example, municipality
  centroids), `inmet_stations_within()` selects the stations inside a
  bounding box or an sf polygon, and `inmet_interpolate()` estimates a
  variable at other locations by inverse-distance weighting.
* `plot()` draws `rmet_hourly`, `rmet_daily` and `rmet_monthly` data, and
  `inmet_convert_radiation()` converts between radiation units.
* `inmet_derive()` adds vapour pressure, vapour pressure deficit, wind
  components (`wind_u_ms`, `wind_v_ms`) and the heat index to hourly data,
  ignoring values flagged by `inmet_qc()`.
* `inmet_availability()` lists the stations and periods in the cached
  archives from the file names alone, without reading any data.
* `inmet_cache_dir()` gives the default cache directory, which can now be
  changed for a whole session with `options(rmet.cache_dir = )` or the
  environment variable `RMET_CACHE_DIR`. All `dest_dir` and `cache_dir`
  arguments use it by default.

* The results of `inmet_read()`, `inmet_get()`, `inmet_daily()` and
  `inmet_monthly()` have the classes `rmet_hourly`, `rmet_daily` and
  `rmet_monthly` (all also `rmet_data` and `data.frame`), with a compact
  `print()`, a `summary()` per station and `as.data.frame()`. They remain
  ordinary data frames, and the example dataset has the new class.
* New vignette: "Monthly Values, Evapotranspiration and Derived Variables".

## Improvements

* `inmet_daily()`, `inmet_qc()` and `inmet_completeness()` return an empty
  result for the empty data frame that `inmet_read()` and `inmet_get()` give
  when no data is found, instead of failing with a message about `datetime`.
* The requests for the station catalogue and for the server version of an
  archive are tried up to three times before giving up.
* `inmet_nearest()` reports stations without coordinates that it ignores.

## Performance

* The data rows of the station files are now parsed by compiled C code
  (without new dependencies) from the raw bytes read from the archive. Reading
  a full year (567 files, 4.2 million rows) takes 21 s instead of 134 s, and 120
  stations take 4.8 s instead of 22 s, with identical results: the new
  parser was compared with the previous one on 125 real files from
  every kind of year (2000 to 2026), 853,000 rows, and agrees exactly.

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
  column. By default (`tz = "auto"`) each station uses its own local time.
* `inmet_qc()` flags implausible hourly values: out of physical range,
  internally inconsistent, isolated spikes, and stuck sensors. Flags are
  stored in `<variable>_flag` columns (or the values removed), summarised in
  the `qc_summary` attribute, and respected by `inmet_daily()` and
  `inmet_completeness()`.
* `inmet_completeness()` reports the percentage of valid hours per station
  and month or year.
* `inmet_timezone()` returns the historical local time zone of Brazilian
  stations; `inmet_stations()` gains a `time_zone` column.
* `inmet_nearest()` finds the stations closest to a location.
* `inmet_as_sf()` converts stations or data to `sf` points (requires the
  suggested package sf).
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
  `stations.R`, `daily.R`, `qc.R`, `completeness.R`, `timezone.R`,
  `spatial.R` and `cache.R`, with comments on the INMET format. `utils.R` was
  removed.

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
