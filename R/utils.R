#' rmet: Access Historical Weather Data from INMET
#'
#' Tools to discover stations and download, cache, and parse historical hourly
#' weather data from Brazil's National Institute of Meteorology (INMET).
#'
#' @section Main functions:
#' * [inmet_get()] downloads and reads data in one call.
#' * [inmet_download()] downloads annual archives.
#' * [inmet_read()] parses cached archives.
#' * [inmet_stations()] searches the official station catalogue.
#'
#' @keywords internal
"_PACKAGE"

.validate_years <- function(years) {
  if (!length(years) || !is.atomic(years)) {
    stop("`years` must be a non-empty vector of integer years.", call. = FALSE)
  }
  numeric_years <- suppressWarnings(as.numeric(years))
  if (anyNA(numeric_years) || any(!is.finite(numeric_years)) ||
      any(numeric_years != floor(numeric_years))) {
    stop("`years` must be a vector of integers.", call. = FALSE)
  }
  years <- as.integer(numeric_years)
  current <- as.integer(format(Sys.Date(), "%Y"))
  if (any(years < 2000L)) {
    stop("INMET historical archives are available from 2000 onwards.", call. = FALSE)
  }
  if (any(years > current)) stop("Cannot request years in the future.", call. = FALSE)
  unique(years)
}

.validate_flag <- function(x, name) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    stop("`", name, "` must be TRUE or FALSE.", call. = FALSE)
  }
  x
}

.validate_positive_integer <- function(x, name) {
  value <- suppressWarnings(as.numeric(x))
  if (length(value) != 1L || is.na(value) || !is.finite(value) ||
      value < 1 || value != floor(value)) {
    stop("`", name, "` must be a positive integer.", call. = FALSE)
  }
  as.integer(value)
}

.validate_directory <- function(x, name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop("`", name, "` must be a single non-empty path.", call. = FALSE)
  }
  path.expand(x)
}

.is_valid_zip <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) return(FALSE)
  entries <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
  !is.null(entries) && nrow(entries) > 0L
}

.rmet_user_agent <- function() {
  version <- tryCatch(
    as.character(utils::packageVersion("rmet")),
    error = function(e) "0.2.0"
  )
  paste0("rmet/", version, " (+https://github.com/rodrigosqrt3/rmet)")
}

.safe_rbind <- function(list_df) {
  list_df <- Filter(function(x) !is.null(x) && nrow(x) > 0L, list_df)
  if (!length(list_df)) return(NULL)
  all_cols <- unique(unlist(lapply(list_df, names), use.names = FALSE))
  list_df <- lapply(list_df, function(d) {
    missing_cols <- setdiff(all_cols, names(d))
    if (length(missing_cols)) d[missing_cols] <- NA
    d[all_cols]
  })
  out <- do.call(rbind, list_df)
  rownames(out) <- NULL
  out
}

#' Inspect downloaded INMET archives
#'
#' @param dest_dir Cache directory used by [inmet_download()].
#' @return Invisibly, a data frame with archive year, path, size, validity, and
#'   whether a partial download exists.
#' @export
inmet_cache_status <- function(dest_dir = tools::R_user_dir("rmet", "cache")) {
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  files <- if (dir.exists(dest_dir)) {
    list.files(dest_dir, pattern = "^\\d{4}\\.zip(\\.part)?$", full.names = TRUE)
  } else character()

  if (!length(files)) {
    message("No downloaded ZIPs found in: ", dest_dir)
    return(invisible(data.frame(
      year = integer(), path = character(), size_mb = numeric(),
      valid = logical(), partial = logical(), stringsAsFactors = FALSE
    )))
  }

  years <- sort(unique(as.integer(sub("\\.zip(\\.part)?$", "", basename(files)))))
  zips <- file.path(dest_dir, paste0(years, ".zip"))
  parts <- paste0(zips, ".part")
  out <- data.frame(
    year = years,
    path = zips,
    size_mb = round(ifelse(file.exists(zips), file.size(zips), file.size(parts)) / 1e6, 1),
    valid = vapply(zips, .is_valid_zip, logical(1)),
    partial = file.exists(parts),
    stringsAsFactors = FALSE
  )
  out[order(out$year), , drop = FALSE]
}

#' Clear the rmet cache
#'
#' @param years Integer years to remove, or `NULL` for every archive.
#' @param dest_dir Cache directory.
#' @param ask Ask for confirmation before deleting.
#' @param catalog Also remove the cached official station catalogue.
#' @return Invisible `NULL`.
#' @export
inmet_cache_clear <- function(
    years = NULL,
    dest_dir = tools::R_user_dir("rmet", "cache"),
    ask = interactive(),
    catalog = FALSE
) {
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  ask <- .validate_flag(ask, "ask")
  catalog <- .validate_flag(catalog, "catalog")
  if (!dir.exists(dest_dir)) {
    message("Cache directory does not exist: ", dest_dir)
    return(invisible(NULL))
  }

  if (is.null(years)) {
    targets <- list.files(
      dest_dir,
      pattern = "^\\d{4}\\.zip(\\.part)?$",
      full.names = TRUE
    )
  } else {
    years <- .validate_years(years)
    bases <- file.path(dest_dir, paste0(unique(years), ".zip"))
    targets <- c(bases, paste0(bases, ".part"))
    targets <- targets[file.exists(targets)]
  }
  if (catalog) {
    catalog_path <- file.path(dest_dir, "stations.rds")
    if (file.exists(catalog_path)) targets <- c(targets, catalog_path)
  }
  targets <- unique(targets)

  if (!length(targets)) {
    message("Nothing to delete.")
    return(invisible(NULL))
  }
  if (ask) {
    answer <- readline(sprintf(
      "Delete %d cached file(s) from '%s'? [y/N] ",
      length(targets), dest_dir
    ))
    if (!grepl("^[Yy]", answer)) {
      message("Aborted.")
      return(invisible(NULL))
    }
  }

  removed <- file.remove(targets)
  message(sprintf("Deleted %d/%d file(s).", sum(removed), length(targets)))
  invisible(NULL)
}
