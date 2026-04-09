# rmet 0.1.0

* Initial CRAN release.
* Added `inmet_download()` with a retry and resume algorithm to handle unstable connections when downloading ZIP archives.
* Added `inmet_read()` to safely parse raw INMET CSVs. It automatically drops trailing semicolon artifacts, aligns missing columns across years, parses multiple legacy date formats, and shifts UTC times to local Brazilian time (`America/Sao_Paulo`).
* Added `inmet_extract()` for safely extracting CSVs to a local directory.
* Added `inmet_stations()` to provide a bundled, offline-ready catalogue of automatic weather stations.
* Added cache management utilities: `inmet_cache_status()` and `inmet_cache_clear()`.
