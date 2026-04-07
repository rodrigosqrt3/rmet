# rmet

[![R-CMD-check](https://github.com/yourgh/rmet/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/yourgh/rmet/actions/workflows/R-CMD-check.yaml)
[![CRAN status](https://www.r-pkg.org/badges/version/rmet)](https://CRAN.R-project.org/package=rmet)

`rmet` provides a robust, fast, and tidy interface to download and process historical hourly weather data from Brazil's National Institute of Meteorology ([INMET](https://portal.inmet.gov.br/)). 

Working with INMET's raw CSV files can be challenging due to unstable server connections, changing date formats over the years, and structural artifacts like trailing semicolons. `rmet` automates the entire pipeline, from safely downloading the archives to outputting clean, analysis-ready data.

## Key Features
* **Resumable Downloads:** Handles dropped connections by automatically resuming partial ZIP downloads.
* **Tidy Architecture:** Standardizes column names, converts types, and safely drops phantom columns caused by INMET's trailing semicolons.
* **Format Normalization:** Seamlessly parses different datetime formats used by INMET between 2000 and the present.
* **Timezone Correction:** Automatically shifts UTC timestamps to local Brazilian time (`America/Sao_Paulo`) to align dates correctly.
* **Granular Filtering:** Read exact days, specific stations, and select variables without loading entire years into memory.

## Installation

```r
# Install the development version from GitHub:
# install.packages("pak")
pak::pak("yourgh/rmet")
```

## Quick Start

```r
library(rmet)

# 1. Browse the bundled catalogue of automatic stations
stations <- inmet_stations(state = "RS")
head(stations)

# 2. Download data for specific years (resumable — safe to re-run)
# Downloads are saved to a persistent cache automatically
inmet_download(2022:2024)

# 3. Read into R as a tidy data.frame
# Filter by exact dates, stations, and variables
df <- inmet_read(
  years      = 2022:2024,
  stations   = c("A801", "A802"),
  start_date = "2023-01-01",
  end_date   = "2023-06-30",
  variables  = c("temp_dry_c", "precip_mm", "humid_rel_pct")
)

head(df)
```

## Core Functions

| Function | Description |
|---|---|
| `inmet_download()` | Download annual ZIP files with an automatic back-off and resume algorithm. |
| `inmet_read()` | Parse downloaded ZIP archives directly into a tidy `data.frame`. |
| `inmet_extract()` | Unzip CSV files to a local directory for external use. |
| `inmet_stations()` | Browse the package's bundled automatic station catalogue. |
| `inmet_cache_status()` | View metadata for locally cached INMET archives. |
| `inmet_cache_clear()` | Safely clear the downloaded archives from your local machine. |
