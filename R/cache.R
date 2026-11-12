#' Default cache directory of rmet
#'
#' Returns the directory where rmet stores downloaded archives and the station
#' catalogue. It is the first one that is set among the option
#' `rmet.cache_dir`, the environment variable `RMET_CACHE_DIR`, and the
#' user-level cache directory of R (`tools::R_user_dir("rmet", "cache")`).
#' Every function that takes a `dest_dir` or `cache_dir` argument uses it as
#' the default.
#'
#' @return A single path.
#' @examples
#' inmet_cache_dir()
#'
#' # use a different directory for the whole session
#' old <- options(rmet.cache_dir = file.path(tempdir(), "rmet"))
#' inmet_cache_dir()
#' options(old)
#' @seealso [inmet_cache_status()], [inmet_cache_clear()]
#' @export
inmet_cache_dir <- function() {
  option <- getOption("rmet.cache_dir")
  if (is.character(option) && length(option) == 1L && !is.na(option) && nzchar(option)) {
    return(path.expand(option))
  }
  environment <- Sys.getenv("RMET_CACHE_DIR", unset = "")
  if (nzchar(environment)) return(path.expand(environment))
  tools::R_user_dir("rmet", "cache")
}

#' Inspect downloaded INMET archives
#'
#' @param dest_dir Cache directory used by [inmet_download()]. Defaults to
#'   [inmet_cache_dir()].
#' @return A data frame with one row per cached year: archive path, size in
#'   MB, whether the ZIP is valid, whether a partial download exists, and when
#'   the archive was downloaded (UTC, if recorded). Returned invisibly when the
#'   cache is empty.
#' @examples
#' inmet_cache_status(dest_dir = file.path(tempdir(), "rmet"))
#' @export
inmet_cache_status <- function(dest_dir = inmet_cache_dir()) {
  dest_dir <- .validate_directory(dest_dir, "dest_dir")
  files <- if (dir.exists(dest_dir)) {
    list.files(dest_dir, pattern = "^\\d{4}\\.zip(\\.part)?$", full.names = TRUE)
  } else character()

  if (!length(files)) {
    message("No downloaded ZIPs found in: ", dest_dir)
    return(invisible(data.frame(
      year = integer(), path = character(), size_mb = numeric(), valid = logical(),
      partial = logical(), downloaded = character(), stringsAsFactors = FALSE
    )))
  }

  years <- sort(unique(as.integer(sub("\\.zip(\\.part)?$", "", basename(files)))))
  zips <- file.path(dest_dir, paste0(years, ".zip"))
  parts <- paste0(zips, ".part")
  downloaded <- vapply(zips, function(z) {
    meta <- if (file.exists(.meta_path(z))) tryCatch(read.dcf(.meta_path(z)), error = function(e) NULL)
    if (!is.null(meta) && "downloaded" %in% colnames(meta)) meta[1, "downloaded"] else NA_character_
  }, character(1), USE.NAMES = FALSE)

  out <- data.frame(
    year = years,
    path = zips,
    size_mb = round(ifelse(file.exists(zips), file.size(zips), file.size(parts)) / 1e6, 1),
    valid = vapply(zips, .is_valid_zip, logical(1), USE.NAMES = FALSE),
    partial = file.exists(parts),
    downloaded = downloaded,
    stringsAsFactors = FALSE
  )
  out
}

#' Clear the rmet cache
#'
#' @param years Integer years to remove, or `NULL` for every archive.
#' @param dest_dir Cache directory.
#' @param ask Ask for confirmation before deleting.
#' @param catalog Also remove the cached official station catalogue.
#' @return Invisible `NULL`.
#' @examples
#' inmet_cache_clear(dest_dir = file.path(tempdir(), "rmet"), ask = FALSE)
#' @export
inmet_cache_clear <- function(
    years = NULL,
    dest_dir = inmet_cache_dir(),
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
    targets <- list.files(dest_dir, pattern = "^\\d{4}\\.zip(\\.part|\\.meta)?$",
                          full.names = TRUE)
  } else {
    years <- .validate_years(years)
    bases <- file.path(dest_dir, paste0(years, ".zip"))
    targets <- c(bases, paste0(bases, ".part"), .meta_path(bases))
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
    answer <- readline(sprintf("Delete %d cached file(s) from '%s'? [y/N] ",
                               length(targets), dest_dir))
    if (!grepl("^[Yy]", answer)) {
      message("Aborted.")
      return(invisible(NULL))
    }
  }

  removed <- file.remove(targets)
  message(sprintf("Deleted %d/%d file(s).", sum(removed), length(targets)))
  invisible(NULL)
}

.is_valid_zip <- function(path) {
  if (!is.character(path) || length(path) != 1L || !file.exists(path)) return(FALSE)
  entries <- tryCatch(utils::unzip(path, list = TRUE), error = function(e) NULL)
  !is.null(entries) && nrow(entries) > 0L
}

.rmet_user_agent <- function() {
  version <- tryCatch(as.character(utils::packageVersion("rmet")),
                      error = function(e) "0.3.0")
  paste0("rmet/", version, " (+https://github.com/rodrigosqrt3/rmet)")
}
