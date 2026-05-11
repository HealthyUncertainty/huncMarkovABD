# R/background_mortality.R
# Canadian life table (sex-weighted: 55% female) + background mortality lookup
# Author: Ian Cromwell <healthyuncertainty@gmail.com>

.mortality_rates <- data.frame(
  age = c(0, 1, 10, 20, 30, 40, 50, 60, 65, 70, 75, 80, 85, 90, 95, 100),
  qx  = c(
    0.55*0.005132 + 0.45*0.006023, 0.55*0.000394 + 0.45*0.000481,
    0.55*0.000098 + 0.45*0.000087, 0.55*0.000443 + 0.45*0.001175,
    0.55*0.000898 + 0.45*0.002127, 0.55*0.001556 + 0.45*0.003176,
    0.55*0.003028 + 0.45*0.005182, 0.55*0.006849 + 0.45*0.011457,
    0.55*0.009638 + 0.45*0.016196, 0.55*0.014453 + 0.45*0.022642,
    0.55*0.023241 + 0.45*0.033522, 0.55*0.039213 + 0.45*0.054164,
    0.55*0.069414 + 0.45*0.092160, 0.55*0.120291 + 0.45*0.159898,
    0.55*0.201888 + 0.45*0.256283, 1.000000
  ),
  stringsAsFactors = FALSE
)

#' Get Background Mortality Probability
#'
#' Returns the annual background mortality probability interpolated from a
#' sex-weighted Canadian life table (55\% female), then converted to a
#' per-cycle probability.
#'
#' @param age Numeric. Current age of the cohort.
#' @param cycle_length_years Numeric. Cycle length in years.
#' @return Numeric. Background mortality probability for one cycle.
#' @export
#' @examples
#' get_background_mortality(age = 48, cycle_length_years = 2/52)
get_background_mortality <- function(age, cycle_length_years) {
  ages <- .mortality_rates$age
  if (age <= min(ages)) return(1 - (1 - .mortality_rates$qx[which.min(ages)])^cycle_length_years)
  if (age >= max(ages)) return(1 - (1 - .mortality_rates$qx[which.max(ages)])^cycle_length_years)
  lower <- max(ages[ages <= age])
  upper <- min(ages[ages >= age])
  if (lower == upper) {
    qx_annual <- .mortality_rates$qx[which(ages == age)]
  } else {
    w  <- (age - lower) / (upper - lower)
    idx_lo <- which(ages == lower)
    idx_hi <- which(ages == upper)
    qx_annual <- .mortality_rates$qx[idx_lo] * (1 - w) + .mortality_rates$qx[idx_hi] * w
  }
  1 - (1 - qx_annual)^cycle_length_years
}
