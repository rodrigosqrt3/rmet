# rmet

[![CRAN status](https://www.r-pkg.org/badges/version/rmet)](https://CRAN.R-project.org/package=rmet)
[![R-CMD-check](https://github.com/rodrigosqrt3/rmet/actions/workflows/r.yml/badge.svg)](https://github.com/rodrigosqrt3/rmet/actions/workflows/r.yml)
[![codecov](https://codecov.io/gh/rodrigosqrt3/rmet/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rodrigosqrt3/rmet)

`rmet` downloads and parses historical hourly weather data published by
Brazil's National Institute of Meteorology (INMET). It handles format changes
across years, inconsistent encodings and column names, interrupted downloads,
and local caching.

## Installation

Install the released version from CRAN:

```r
install.packages("rmet")
```

Install the development version from GitHub:

```r
# install.packages("pak")
pak::pak("rodrigosqrt3/rmet")
```

## Quick start

The recommended interface downloads missing archives, reuses valid cached
files, and reads the requested data:

```r
library(rmet)

weather <- inmet_get(
  years = 2023:2025,
  stations = "A801",
  variables = c("temp_dry_c", "precip_mm", "humid_rel_pct")
)
```

Timestamps are returned in UTC by default because the historical source files
use UTC. Convert their display explicitly when needed:

```r
weather_sp <- inmet_get(2025, stations = "A801", tz = "America/Sao_Paulo")
```

## Find stations

`inmet_stations()` obtains the current official automatic-station catalogue,
caches it locally, refreshes it periodically, and falls back to a cached or
bundled catalogue when offline.

```r
stations <- inmet_stations(state = "RS", status = "Operante")
stations[, c("station_code", "station_name", "status", "start_date")]

# Force a catalogue update
stations <- inmet_stations(refresh = TRUE)
```

The catalogue can also be filtered by region, state, station name or code,
operational status, and provider entity.

## Lower-level workflow

```r
inmet_download(2024:2025)
inmet_cache_status()

weather <- inmet_read(
  years = 2024:2025,
  stations = c("A801", "A802"),
  start_date = "2024-06-01",
  end_date = "2024-08-31"
)
```

| Function | Purpose |
|---|---|
| `inmet_get()` | Download and read data in one call. |
| `inmet_download()` | Reliably download and cache annual ZIP archives. |
| `inmet_read()` | Parse cached archives into consistent data frames. |
| `inmet_extract()` | Extract original CSV files. |
| `inmet_stations()` | Search the current official station catalogue. |
| `inmet_cache_status()` | Inspect complete and partial cached downloads. |
| `inmet_cache_clear()` | Remove cached archives and catalogue data. |

## Data quality

INMET describes automatic-station observations as raw data that have not
undergone consistency validation. `rmet` standardizes structure and missing
values but does not certify that measurements are meteorologically plausible.
Users should apply quality-control rules appropriate to their analysis.

## Data source and license

Data are retrieved from INMET and are not redistributed by `rmet`. Package
code is licensed under GPL (>= 3).

Please report reproducible problems through the package's GitHub issue tracker.
