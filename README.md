# rmet

[![CRAN status](https://www.r-pkg.org/badges/version/rmet)](https://CRAN.R-project.org/package=rmet)
[![R-CMD-check](https://github.com/rodrigosqrt3/rmet/actions/workflows/r.yml/badge.svg)](https://github.com/rodrigosqrt3/rmet/actions/workflows/r.yml)
[![codecov](https://codecov.io/gh/rodrigosqrt3/rmet/branch/main/graph/badge.svg)](https://app.codecov.io/gh/rodrigosqrt3/rmet)

`rmet` downloads and parses historical hourly weather data from the automatic
stations of Brazil's National Institute of Meteorology (INMET). It handles the
format and encoding changes across years, validates and resumes downloads,
keeps a local cache up to date, and builds daily summaries in local time.

> **Upgrading from rmet 0.1.0 or 0.2.0?** The station catalogue bundled with
> those versions was incorrect (most station codes pointed to the wrong
> station). Please upgrade and re-check any station metadata obtained offline
> or with `source = "bundled"`. See [NEWS](NEWS.md) for all corrections.

## Installation

```r
install.packages("rmet")

# development version
# install.packages("pak")
pak::pak("rodrigosqrt3/rmet")
```

## Quick start

```r
library(rmet)

# stations near Porto Alegre
inmet_nearest(latitude = -30.03, longitude = -51.23, n = 3)

# hourly data (archives are downloaded once and cached)
weather <- inmet_get(years = 2022:2024, stations = "A801")

# flag implausible values, check completeness, summarise per local day
weather <- inmet_qc(weather)
inmet_completeness(weather)
daily <- inmet_daily(weather)

# monthly values and reference evapotranspiration
monthly <- inmet_monthly(inmet_et0(daily))
```

Timestamps are returned in UTC, as in the source files, and mark the end of
the hour each row describes. `inmet_daily()` assigns hours to each station's
local day, skips values flagged by `inmet_qc()`, and only reports a daily value
when enough hours are available.

## Main functions

| Function | Purpose |
|---|---|
| `inmet_get()` | Download (if needed) and read hourly data in one call. |
| `inmet_daily()` | Daily summaries per station and local day. |
| `inmet_monthly()` | Monthly summaries from daily data. |
| `inmet_et0()`, `inmet_design_et0()` | Reference evapotranspiration (Penman-Monteith or Hargreaves) and its design value. |
| `inmet_pet_thornthwaite()`, `inmet_water_balance()` | Monthly potential evapotranspiration and soil water balance. |
| `inmet_indices()`, `inmet_spi()` | Climate indices and the Standardized Precipitation Index. |
| `inmet_normals()`, `inmet_anomaly()` | Climatological normals and anomalies. |
| `inmet_fill()` | Fill short gaps, always marked. |
| `inmet_interpolate()` | Estimate a variable at other locations from the stations. |
| `inmet_stations_within()` | Stations inside a box or polygon. |
| `inmet_derive()` | Vapour pressure deficit, wind components and heat index. |
| `inmet_qc()` | Flag implausible hourly values. |
| `inmet_availability()` | Stations and periods present in the cached archives. |
| `inmet_completeness()` | Percentage of valid hours per station and month or year. |
| `inmet_stations()` | Search the official station catalogue. |
| `inmet_nearest()` | Find the stations closest to a location. |
| `inmet_timezone()` | Local time zone of Brazilian stations. |
| `inmet_as_sf()` | Convert stations or data to `sf` points for maps. |
| `inmet_download()` | Download, validate and update annual ZIP archives. |
| `inmet_read()` | Read cached archives into a data frame. |
| `inmet_extract()` | Extract the original CSV files. |
| `inmet_cache_status()`, `inmet_cache_clear()`, `inmet_cache_dir()` | Manage the cache; set its location with `options(rmet.cache_dir = )`. |

## How it works

* **Catalogue.** `inmet_stations()` uses INMET's station API, cached for 30
  days, and falls back to a dated snapshot of the official catalogue shipped
  with the package.
* **Downloads.** INMET publishes one archive per year (about 100 MB for recent
  years) with every station. Downloads go to a `.part` file, are checked
  against the size reported by the server and resumed after interruptions.
  Archives of the current and previous year are re-downloaded when INMET
  publishes a newer version.
* **Parsing.** Files are read directly from the archives. Date and hour
  formats, Latin-1 and UTF-8 encodings, decimal commas, `-9999` sentinels and
  damaged header text from different years are handled; columns get stable
  English names (see `?inmet_read`).

## Data quality

INMET describes automatic-station observations as raw data that have not
undergone consistency validation. `inmet_qc()` flags gross errors (impossible
values, inconsistencies, spikes, stuck sensors), but it cannot certify that
every measurement is meteorologically plausible. See
`vignette("daily-data", package = "rmet")` for time-zone, missing-data and
quality-control advice.

## Data source and license

Data are retrieved from INMET and are not redistributed by `rmet`, apart from
the station catalogue snapshot and a one-station example dataset. Package code
is licensed under GPL (>= 3). Please report problems at
<https://github.com/rodrigosqrt3/rmet/issues>.
