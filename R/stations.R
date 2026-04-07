#' List INMET automatic weather stations
#'
#' Returns a `data.frame` of all INMET automatic weather stations (*estações
#' automáticas*) with their metadata. The catalogue is bundled with the package
#' and reflects the stations as of the package release date.
#'
#' @param region Character vector. Filter by macro-region code(s):
#'   `"CO"`, `"N"`, `"NE"`, `"S"`, `"SE"`. Case-insensitive.
#'   If `NULL` (default), all regions are returned.
#' @param state Character vector. Filter by two-letter state abbreviation(s)
#'   (e.g., `"RS"`, `"SP"`, `"AM"`). Case-insensitive.
#'   If `NULL` (default), all states are returned.
#' @param search Character. A free-text search string applied to the station
#'   name column (case-insensitive, partial matching). If `NULL` (default),
#'   no name filtering is applied.
#'
#' @return A `data.frame` with columns:
#'   \describe{
#'     \item{`station_code`}{Character. INMET four-character station identifier.}
#'     \item{`station_name`}{Character. Station name.}
#'     \item{`region`}{Character. Macro-region code.}
#'     \item{`state`}{Character. Two-letter state abbreviation.}
#'     \item{`latitude`}{Numeric. Decimal degrees (negative = south).}
#'     \item{`longitude`}{Numeric. Decimal degrees (negative = west).}
#'     \item{`elevation`}{Numeric. Altitude in metres.}
#'     \item{`start_year`}{Integer. Year from which data is available.}
#'   }
#'
#' @examples
#' # All stations
#' st <- inmet_stations()
#' nrow(st)
#'
#' # Stations in Rio Grande do Sul
#' st_rs <- inmet_stations(state = "RS")
#'
#' # Search by name
#' inmet_stations(search = "porto alegre")
#'
#' @seealso [inmet_download()], [inmet_read()]
#' @export
inmet_stations <- function(region = NULL, state = NULL, search = NULL) {
  st <- .load_stations()

  if (!is.null(region)) {
    region <- toupper(region)
    st <- st[toupper(st$region) %in% region, , drop = FALSE]
  }
  if (!is.null(state)) {
    state <- toupper(state)
    st <- st[toupper(st$state) %in% state, , drop = FALSE]
  }
  if (!is.null(search)) {
    st <- st[grepl(search, st$station_name, ignore.case = TRUE), , drop = FALSE]
  }

  rownames(st) <- NULL
  st
}


# Internal: load bundled station catalogue from inst/extdata
.load_stations <- function() {
  path <- system.file("extdata", "stations.csv", package = "rmet", mustWork = TRUE)
  utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    fileEncoding     = "UTF-8",
    colClasses       = c(
      station_code = "character",
      station_name = "character",
      region       = "character",
      state        = "character",
      latitude     = "numeric",
      longitude    = "numeric",
      elevation    = "numeric",
      start_year   = "integer"
    )
  )
}
