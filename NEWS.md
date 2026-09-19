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
