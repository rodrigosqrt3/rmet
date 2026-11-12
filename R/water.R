#' Potential evapotranspiration by Thornthwaite
#'
#' Adds the monthly potential evapotranspiration `pet_mm` to the output of
#' [inmet_monthly()], from air temperature alone, with the method of
#' Thornthwaite (1948) corrected for the day length and the number of days of
#' each month.
#'
#' @param x A data frame as returned by [inmet_monthly()], with `month`,
#'   `temp_mean_c` and `latitude`.
#'
#' @return `x` with the new column `pet_mm` (mm per month).
#'
#' @details
#' The annual heat index of each station is computed from its mean
#' temperature of each calendar month over all the years in `x`, so `x` must
#' have all 12 calendar months for the station; otherwise `pet_mm` is `NA` for
#' it. Stations with several years give a stable index; with a single year the
#' index is that of the year. Thornthwaite's method underestimates in humid
#' tropical regions and overestimates in dry ones, and is best for monthly or
#' longer periods; for daily needs see [inmet_et0()].
#'
#' @examples
#' monthly <- inmet_monthly(inmet_daily(rmet_example))
#' monthly <- inmet_pet_thornthwaite(monthly)
#' monthly[, c("month", "temp_mean_c", "pet_mm")]
#' @seealso [inmet_water_balance()], [inmet_et0()]
#' @export
inmet_pet_thornthwaite <- function(x) {
  if (.is_no_data(x)) return(x)
  needed <- c("month", "temp_mean_c", "latitude")
  missing <- setdiff(needed, names(x))
  if (!is.data.frame(x) || length(missing)) {
    stop("`x` must be the output of inmet_monthly() with the columns: ",
         paste(missing, collapse = ", "), ".", call. = FALSE)
  }
  station <- if ("station_code" %in% names(x)) x$station_code else rep("", nrow(x))
  calendar <- as.integer(format(x$month, "%m"))
  temperature <- as.numeric(x$temp_mean_c)
  pet <- rep(NA_real_, nrow(x))

  for (rows in split(seq_len(nrow(x)), match(station, unique(station)))) {
    normal <- vapply(1:12, function(m) {
      mean(temperature[rows][calendar[rows] == m], na.rm = TRUE)
    }, numeric(1))
    if (anyNA(normal)) next
    heat <- sum((pmax(normal, 0) / 5)^1.514)
    a <- 6.75e-7 * heat^3 - 7.71e-5 * heat^2 + 1.792e-2 * heat + 0.49239
    t <- temperature[rows]
    unadjusted <- ifelse(
      t >= 26.5, -415.85 + 32.24 * t - 0.43 * t^2,
      ifelse(t <= 0 | heat == 0, 0, 16 * (10 * t / heat)^a)
    )
    middle <- as.numeric(format(x$month[rows] + 14, "%j"))
    lat <- as.numeric(x$latitude[rows][[1]]) * pi / 180
    declination <- 0.409 * sin(2 * pi * middle / 365 - 1.39)
    sunset <- acos(pmin(1, pmax(-1, -tan(lat) * tan(declination))))
    daylight <- 24 / pi * sunset
    days <- as.numeric(as.Date(format(x$month[rows] + 31, "%Y-%m-01")) - x$month[rows])
    pet[rows] <- unadjusted * daylight / 12 * days / 30
  }
  x$pet_mm <- pet
  x
}

#' Climatological water balance
#'
#' Sequential soil water balance of Thornthwaite and Mather (1955) for each
#' station, from precipitation and potential evapotranspiration.
#'
#' @param x A data frame in time order per station, with `precip_mm` and the
#'   column named in `pet`; typically the output of [inmet_monthly()] with
#'   [inmet_pet_thornthwaite()] or [inmet_et0()] already applied.
#' @param awc Available water capacity of the soil, in mm.
#' @param pet Name of the column with potential or reference
#'   evapotranspiration for the same period as `precip_mm`.
#' @param initial Soil water storage at the start of each station's series, in
#'   mm. Default: `awc` (soil at capacity).
#'
#' @return `x` with the columns `storage_mm` (soil water at the end of the
#'   period), `etr_mm` (actual evapotranspiration), `deficit_mm` (potential
#'   minus actual evapotranspiration) and `excess_mm` (surplus that percolates
#'   or runs off). Periods with missing precipitation or evapotranspiration
#'   have `NA` in the four columns and leave the storage unchanged.
#'
#' @details
#' When precipitation is below the evapotranspiration, the soil dries
#' exponentially: `storage = previous * exp((P - PET) / awc)`; otherwise it
#' recharges up to `awc` and the rest is excess. The first periods depend on
#' `initial`, so use a series of at least a year before the period of
#' interest. The result does not depend on the time step, but the usual
#' choice is monthly.
#'
#' @examples
#' monthly <- inmet_pet_thornthwaite(inmet_monthly(inmet_daily(rmet_example)))
#' balance <- inmet_water_balance(monthly, awc = 100)
#' balance[, c("month", "precip_mm", "pet_mm", "storage_mm", "deficit_mm", "excess_mm")]
#' @seealso [inmet_pet_thornthwaite()], [inmet_et0()]
#' @export
inmet_water_balance <- function(x, awc = 100, pet = "pet_mm", initial = awc) {
  if (.is_no_data(x)) return(x)
  awc <- .validate_number(awc, "awc", 1e-6, Inf)
  initial <- .validate_number(initial, "initial", 0, awc)
  if (!is.character(pet) || length(pet) != 1L) {
    stop("`pet` must be the name of one column.", call. = FALSE)
  }
  if (!is.data.frame(x) || !all(c("precip_mm", pet) %in% names(x))) {
    stop("`x` must be a data frame with `precip_mm` and `", pet, "`.", call. = FALSE)
  }
  station <- if ("station_code" %in% names(x)) x$station_code else rep("", nrow(x))
  rain <- as.numeric(x$precip_mm)
  demand <- as.numeric(x[[pet]])
  storage <- etr <- deficit <- excess <- rep(NA_real_, nrow(x))

  for (rows in split(seq_len(nrow(x)), match(station, unique(station)))) {
    previous <- initial
    for (i in rows) {
      if (is.na(rain[[i]]) || is.na(demand[[i]])) next
      balance <- rain[[i]] - demand[[i]]
      current <- if (balance < 0) previous * exp(balance / awc) else min(awc, previous + balance)
      change <- current - previous
      etr[[i]] <- if (balance < 0) rain[[i]] - change else demand[[i]]
      deficit[[i]] <- demand[[i]] - etr[[i]]
      excess[[i]] <- max(balance - change, 0)
      storage[[i]] <- current
      previous <- current
    }
  }
  x$storage_mm <- storage
  x$etr_mm <- etr
  x$deficit_mm <- deficit
  x$excess_mm <- excess
  x
}

#' Convert units of solar radiation
#'
#' Converts daily solar radiation between the units used in agrometeorology.
#'
#' @param x Numeric vector of daily radiation.
#' @param from,to Units: `"mj_m2"` (MJ per square metre per day), `"kj_m2"`,
#'   `"j_cm2"`, `"cal_cm2"`, `"w_m2"` (daily mean power) and `"mm"`
#'   (equivalent evaporation, mm per day).
#'
#' @return A numeric vector.
#'
#' @examples
#' inmet_convert_radiation(20, from = "mj_m2", to = "w_m2")
#' @seealso [inmet_daily()]
#' @export
inmet_convert_radiation <- function(x, from, to) {
  to_mj <- c(mj_m2 = 1, kj_m2 = 0.001, j_cm2 = 0.01, cal_cm2 = 0.04184,
             w_m2 = 0.0864, mm = 1 / 0.408)
  if (!is.numeric(x)) stop("`x` must be numeric.", call. = FALSE)
  from <- match.arg(from, names(to_mj))
  to <- match.arg(to, names(to_mj))
  x * to_mj[[from]] / to_mj[[to]]
}
