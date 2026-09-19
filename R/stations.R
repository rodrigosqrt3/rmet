#' Search the INMET automatic-station catalogue
#'
#' Returns automatic weather stations published by INMET. By default, the
#' function uses a cached copy of the official API response and refreshes stale
#' caches periodically. If the service is unavailable, it falls back to an
#' older cache or to the smaller catalogue bundled with the package.
#'
#' @param region,state Optional region or state abbreviations.
#' @param search Optional literal text contained in the station name or code.
#' @param status Optional operational status, such as `"Operante"` or `"Pane"`.
#' @param entity Optional station owner/provider, such as `"INMET"`.
#' @param source One of `"auto"`, `"remote"`, or `"bundled"`. `"auto"` uses
#'   the cache, then the official API, then the bundled fallback.
#' @param refresh Force a fresh request to the official API.
#' @param max_age Maximum cache age in days before an automatic refresh.
#' @param cache_dir Directory used for the catalogue cache.
#' @param quiet Suppress informational messages.
#'
#' @return A data frame with station code, name, region, state, coordinates,
#' altitude, operational dates and status, and provider metadata. The legacy
#' `start_year` column is retained for compatibility.
#'
#' @examples
#' # Offline, reproducible example using the bundled fallback
#' inmet_stations(state = "RS", source = "bundled")
#'
#' \donttest{
#' # Current official catalogue (downloaded once and then cached)
#' inmet_stations(state = "RS", refresh = TRUE, cache_dir = tempdir())
#' }
#' @seealso [inmet_get()], [inmet_read()]
#' @export
inmet_stations <- function(
    region = NULL,
    state = NULL,
    search = NULL,
    status = NULL,
    entity = NULL,
    source = c("auto", "remote", "bundled"),
    refresh = FALSE,
    max_age = 30,
    cache_dir = tools::R_user_dir("rmet", "cache"),
    quiet = FALSE
) {
  source <- match.arg(source)
  refresh <- .validate_flag(refresh, "refresh")
  if (!is.numeric(max_age) || length(max_age) != 1L || is.na(max_age) ||
      !is.finite(max_age) || max_age < 0) {
    stop("`max_age` must be one non-negative number of days.", call. = FALSE)
  }
  if (source == "bundled" && refresh) {
    stop("`refresh = TRUE` cannot be used with `source = \"bundled\"`.", call. = FALSE)
  }
  quiet <- .validate_flag(quiet, "quiet")
  cache_dir <- .validate_directory(cache_dir, "cache_dir")
  region <- .validate_character_filter(region, "region", uppercase = TRUE)
  state <- .validate_character_filter(state, "state", uppercase = TRUE)
  status <- .validate_character_filter(status, "status")
  entity <- .validate_character_filter(entity, "entity", uppercase = TRUE)
  search <- .validate_character_filter(search, "search")
  if (!is.null(search) && length(search) != 1L) {
    stop("`search` must contain a single text value.", call. = FALSE)
  }

  stations <- .load_stations(
    source = source,
    refresh = refresh,
    max_age = max_age,
    cache_dir = cache_dir,
    quiet = quiet
  )
  if (!is.null(region)) {
    stations <- stations[toupper(stations$region) %in% region, , drop = FALSE]
  }
  if (!is.null(state)) {
    stations <- stations[toupper(stations$state) %in% state, , drop = FALSE]
  }
  if (!is.null(status)) {
    stations <- stations[tolower(stations$status) %in% tolower(status), , drop = FALSE]
  }
  if (!is.null(entity)) {
    stations <- stations[toupper(stations$entity) %in% entity, , drop = FALSE]
  }
  if (!is.null(search)) {
    needle <- .normalize_text(search)
    matched <- grepl(needle, .normalize_text(stations$station_name), fixed = TRUE) |
      grepl(needle, .normalize_text(stations$station_code), fixed = TRUE)
    stations <- stations[matched, , drop = FALSE]
  }
  rownames(stations) <- NULL
  stations
}

.load_stations <- function(source = "auto", refresh = FALSE,
                           max_age = 30,
                           cache_dir = tools::R_user_dir("rmet", "cache"),
                           quiet = FALSE) {
  cache_path <- file.path(cache_dir, "stations.rds")

  if (source == "bundled") return(.load_bundled_stations())

  cached <- if (file.exists(cache_path)) {
    tryCatch(readRDS(cache_path), error = function(e) NULL)
  } else NULL
  cache_age <- if (file.exists(cache_path)) {
    as.numeric(difftime(Sys.time(), file.info(cache_path)$mtime, units = "days"))
  } else Inf

  if (!refresh && .valid_station_catalog(cached) && cache_age <= max_age) {
    attr(cached, "catalog_source") <- "cache"
    return(cached)
  }

  remote <- tryCatch(
    .fetch_station_catalog(),
    error = function(e) {
      if (!quiet) message("Could not update station catalogue: ", conditionMessage(e))
      NULL
    }
  )
  if (.valid_station_catalog(remote)) {
    if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
    if (dir.exists(cache_dir)) {
      temporary <- tempfile("stations-", tmpdir = cache_dir, fileext = ".rds")
      saveRDS(remote, temporary, version = 2)
      if (!file.rename(temporary, cache_path)) {
        file.copy(temporary, cache_path, overwrite = TRUE)
        unlink(temporary)
      }
    }
    attr(remote, "catalog_source") <- "INMET API"
    return(remote)
  }

  if (source == "remote") {
    if (.valid_station_catalog(cached)) {
      warning("Using an older cached station catalogue because refresh failed.",
              call. = FALSE)
      attr(cached, "catalog_source") <- "stale cache"
      return(cached)
    }
    stop("The official INMET station catalogue could not be retrieved.", call. = FALSE)
  }
  if (.valid_station_catalog(cached)) {
    warning("Using an older cached station catalogue because refresh failed.",
            call. = FALSE)
    attr(cached, "catalog_source") <- "stale cache"
    return(cached)
  }
  warning(
    "Using the bundled station catalogue because the official catalogue is unavailable.",
    call. = FALSE
  )
  .load_bundled_stations()
}

.fetch_station_catalog <- function() {
  handle <- curl::new_handle()
  curl::handle_setopt(
    handle,
    useragent = .rmet_user_agent(),
    connecttimeout = 20L,
    timeout = 60L,
    failonerror = TRUE,
    followlocation = TRUE
  )
  response <- curl::curl_fetch_memory(
    "https://apitempo.inmet.gov.br/estacoes/T",
    handle = handle
  )
  if (response$status_code < 200L || response$status_code >= 300L) {
    stop("INMET API returned HTTP ", response$status_code, ".", call. = FALSE)
  }
  raw <- jsonlite::fromJSON(rawToChar(response$content), simplifyDataFrame = TRUE)
  .standardize_remote_stations(raw)
}

.standardize_remote_stations <- function(raw) {
  required <- c("CD_ESTACAO", "DC_NOME", "SG_ESTADO", "VL_LATITUDE",
                "VL_LONGITUDE", "VL_ALTITUDE", "DT_INICIO_OPERACAO",
                "DT_FIM_OPERACAO", "CD_SITUACAO", "SG_ENTIDADE")
  missing <- setdiff(required, names(raw))
  if (length(missing)) {
    stop("The INMET station response is missing: ", paste(missing, collapse = ", "),
         call. = FALSE)
  }
  state <- toupper(trimws(as.character(raw$SG_ESTADO)))
  start_date <- .api_date(raw$DT_INICIO_OPERACAO)
  end_date <- .api_date(raw$DT_FIM_OPERACAO)
  out <- data.frame(
    station_code = toupper(trimws(as.character(raw$CD_ESTACAO))),
    station_name = trimws(as.character(raw$DC_NOME)),
    region = unname(.state_regions()[state]),
    state = state,
    latitude = suppressWarnings(as.numeric(raw$VL_LATITUDE)),
    longitude = suppressWarnings(as.numeric(raw$VL_LONGITUDE)),
    elevation = suppressWarnings(as.numeric(raw$VL_ALTITUDE)),
    start_date = start_date,
    end_date = end_date,
    start_year = as.integer(format(start_date, "%Y")),
    status = trimws(as.character(raw$CD_SITUACAO)),
    entity = trimws(as.character(raw$SG_ENTIDADE)),
    wsi_code = if ("CD_WSI" %in% names(raw)) as.character(raw$CD_WSI) else NA_character_,
    oscar_code = if ("CD_OSCAR" %in% names(raw)) as.character(raw$CD_OSCAR) else NA_character_,
    stringsAsFactors = FALSE
  )
  out <- out[!is.na(out$station_code) & nzchar(out$station_code), , drop = FALSE]
  out <- out[!duplicated(out$station_code), , drop = FALSE]
  out <- out[order(out$state, out$station_name, out$station_code), , drop = FALSE]
  rownames(out) <- NULL
  out
}

.api_date <- function(x) {
  text <- as.character(x)
  text[is.na(x) | !nzchar(text)] <- NA_character_
  as.Date(substr(text, 1L, 10L), format = "%Y-%m-%d")
}

.load_bundled_stations <- function() {
  path <- system.file("extdata", "stations.csv", package = "rmet", mustWork = TRUE)
  stations <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    fileEncoding = "UTF-8",
    colClasses = c(
      station_code = "character", station_name = "character",
      region = "character", state = "character", latitude = "numeric",
      longitude = "numeric", elevation = "numeric", start_year = "integer"
    )
  )
  stations$start_date <- as.Date(paste0(stations$start_year, "-01-01"))
  stations$end_date <- as.Date(NA_character_)
  stations$status <- NA_character_
  stations$entity <- "INMET"
  stations$wsi_code <- NA_character_
  stations$oscar_code <- NA_character_
  order <- c("station_code", "station_name", "region", "state", "latitude",
             "longitude", "elevation", "start_date", "end_date", "start_year",
             "status", "entity", "wsi_code", "oscar_code")
  stations <- stations[order]
  attr(stations, "catalog_source") <- "bundled fallback"
  stations
}

.valid_station_catalog <- function(x) {
  is.data.frame(x) && nrow(x) > 0L &&
    all(c("station_code", "station_name", "state", "latitude", "longitude") %in% names(x))
}

.state_regions <- function() {
  c(
    AC = "N", AL = "NE", AP = "N", AM = "N", BA = "NE", CE = "NE",
    DF = "CO", ES = "SE", GO = "CO", MA = "NE", MT = "CO", MS = "CO",
    MG = "SE", PA = "N", PB = "NE", PR = "S", PE = "NE", PI = "NE",
    RJ = "SE", RN = "NE", RS = "S", RO = "N", RR = "N", SC = "S",
    SP = "SE", SE = "NE", TO = "N"
  )
}
