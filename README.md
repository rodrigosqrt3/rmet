# rmet

[![R-CMD-check](https://github.com/rodrigosqrt3/rmet/actions/workflows/r.yaml/badge.svg)](https://github.com/rodrigosqrt3/rmet/actions/workflows/r.yaml)

[![codecov](https://codecov.io/gh/rodrigosqrt3/rmet/branch/main/graph/badge.svg)](https://codecov.io/gh/rodrigosqrt3/rmet)

rmet automates the download and processing of historical hourly weather data
from Brazil's National Institute of Meteorology
([INMET](https://portal.inmet.gov.br/)). It resolves formatting inconsistencies
in raw CSV files across different years, removes structural artifacts,
standardizes column names, converts timestamps to local Brazilian time zones,
and outputs tidy data frames ready for analysis.

## Installation

```r
# install.packages("devtools")
devtools::install_github("rodrigosqrt3/rmet")
```

## Usage

The typical workflow has three steps: browse the station catalogue, download
the annual archives, then read them into R.

```r
library(rmet)

# Browse the bundled catalogue of automatic weather stations
stations <- inmet_stations(state = "RS")
head(stations)

# Download annual ZIP archives (resumable)
inmet_download(2022:2024)

# Read into R, optionally filtering by date, station, and variable
df <- inmet_read(
  years      = 2022:2024,
  stations   = c("A801", "A802"),
  start_date = "2023-01-01",
  end_date   = "2023-06-30",
  variables  = c("temp_dry_c", "precip_mm", "humid_rel_pct")
)

head(df)
```

## Functions

| Function | Description |
|---|---|
| `inmet_download()` | Download annual ZIP files with automatic retry and resume. |
| `inmet_read()` | Parse downloaded archives into a tidy data frame. |
| `inmet_extract()` | Unzip CSV files to a local directory for external use. |
| `inmet_stations()` | Browse the bundled automatic station catalogue. |
| `inmet_cache_status()` | Inspect metadata for locally cached archives. |
| `inmet_cache_clear()` | Remove downloaded archives from the local cache. |

## Data source

Data are provided by INMET and are freely available at
<https://portal.inmet.gov.br/>. The package does not redistribute any data;
it only automates retrieval and parsing of files hosted by INMET.

## License

GPL-3
