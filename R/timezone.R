#' Local time zone of Brazilian stations
#'
#' Returns the Olson time-zone name for each station from its state and,
#' where a state spans more than one historical zone, its longitude. The
#' zones carry Brazil's full history of offsets and daylight saving time, so
#' they give correct local times for any year since 2000.
#'
#' @param state Two-letter state abbreviations, such as `"RS"`.
#' @param longitude Station longitudes in decimal degrees. Only used for
#'   Amazonas (west of 67.2 degrees W is UTC-5), Para (west of 52.5 degrees W
#'   was UTC-4 until 2008), and Pernambuco (Fernando de Noronha).
#'
#' @return A character vector of time-zone names, `NA` for unknown states.
#'
#' @details
#' Zone boundaries inside Amazonas and Para follow municipal borders; the
#' longitude limits reproduce them for INMET's stations but are an
#' approximation elsewhere.
#'
#' @examples
#' inmet_timezone(c("RS", "MT", "AC", "AM", "AM"),
#'                c(-51.2, -56.1, -67.8, -60.0, -69.9))
#' @export
inmet_timezone <- function(state, longitude = NA_real_) {
  state <- toupper(trimws(as.character(state)))
  longitude <- rep_len(suppressWarnings(as.numeric(longitude)), length(state))
  zones <- c(
    AC = "America/Rio_Branco", AL = "America/Maceio", AP = "America/Belem",
    AM = "America/Manaus", BA = "America/Bahia", CE = "America/Fortaleza",
    DF = "America/Sao_Paulo", ES = "America/Sao_Paulo", GO = "America/Sao_Paulo",
    MA = "America/Fortaleza", MT = "America/Cuiaba", MS = "America/Campo_Grande",
    MG = "America/Sao_Paulo", PA = "America/Belem", PB = "America/Fortaleza",
    PR = "America/Sao_Paulo", PE = "America/Recife", PI = "America/Fortaleza",
    RJ = "America/Sao_Paulo", RN = "America/Fortaleza", RS = "America/Sao_Paulo",
    RO = "America/Porto_Velho", RR = "America/Boa_Vista", SC = "America/Sao_Paulo",
    SP = "America/Sao_Paulo", SE = "America/Maceio", TO = "America/Araguaina"
  )
  out <- unname(zones[state])
  west <- function(limit) !is.na(longitude) & longitude < limit
  out[state %in% "AM" & west(-67.2)] <- "America/Eirunepe"
  out[state %in% "PA" & west(-52.5)] <- "America/Santarem"
  out[state %in% "PE" & !is.na(longitude) & longitude > -34] <- "America/Noronha"
  out
}
