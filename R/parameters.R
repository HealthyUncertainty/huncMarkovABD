# R/parameters.R
# Parameter management for huncMarkovABD
# Author: Ian Cromwell <healthyuncertainty@gmail.com>

#' Get Default Model Parameters
#'
#' Returns the full set of default parameters for the ABD cost-effectiveness
#' model. All monetary values are in Canadian dollars (CAD).
#'
#' @return Named list of model parameters.
#' @export
#' @examples
#' params <- get_default_parameters()
#' params$time_horizon_years
get_default_parameters <- function() {
  list(
    # ---- Structural ----
    cycle_length_years  = 2 / 52,
    time_horizon_years  = 50,
    discount_rate       = 0.03,
    starting_age        = 48,
    pct_female          = 0.55,
    wtp_threshold       = 100000,

    # ---- Annual flare-up event rate (SoC) ----
    aaer_soc = 1.65,

    # ---- Treatment effect RRs (Lunavar vs SoC, by flare severity) ----
    rr_mild     = 0.45,
    rr_moderate = 0.25,
    rr_severe   = 0.18,

    # ---- Flare-up type proportions (of total AAER) ----
    prop_mild     = 0.72,
    prop_moderate = 0.12,
    prop_severe   = 0.16,

    # ---- Additional mortality risk: severe flare-up ----
    prob_death_severe_flare = 0.0055,

    # ---- Utilities ----
    utility_stable_lunavar = 0.805,
    utility_stable_soc     = 0.760,
    disutility_mild        = 0.08,
    disutility_moderate    = 0.14,
    disutility_severe      = 0.22,

    # ---- Drug costs (annual, CAD) ----
    cost_drug_lunavar_annual = 4800,
    cost_drug_soc_annual     = 5200,

    # ---- Flare-up event costs (per event, CAD) ----
    cost_mild_event     = 1350,
    cost_moderate_event = 1850,
    cost_severe_event   = 8200
  )
}

#' Get Parameter Registry
#'
#' Returns a data.frame describing every user-adjustable model parameter,
#' including display labels, PSA distributions, and standard errors.
#'
#' @return A data.frame with columns: name, display_name, default_value,
#'   category, type, min, max, step, se, distribution, dist_note.
#' @export
#' @examples
#' reg <- get_parameter_registry()
#' head(reg)
get_parameter_registry <- function() {
  p <- function(name, display_name, default_value, category, type,
                min=NA, max=NA, step=NA, se=NA, distribution=NA, dist_note="") {
    data.frame(name=name, display_name=display_name, default_value=default_value,
               category=category, type=type, min=min, max=max, step=step,
               se=se, distribution=distribution, dist_note=dist_note,
               stringsAsFactors=FALSE)
  }
  registry <- rbind(
    # ---- Flare-up event rate ----
    p("aaer_soc","Annual Flare-Up Event Rate (SoC)",1.65,"Event Rate","rate",0,NA,0.01,
      se=(1.90-1.42)/(2*1.96),distribution="gamma",dist_note="95% CI 1.42-1.90"),
    # ---- Treatment effect RRs ----
    p("rr_mild","RR: Mild Flare-Up (Lunavar vs SoC)",0.45,"Treatment Effects","multiplier",0.01,NA,0.01,
      se=(0.58-0.34)/(2*1.96),distribution="lognormal",dist_note="SE on log scale; 95% CI 0.34-0.58"),
    p("rr_moderate","RR: Moderate Flare-Up (Lunavar vs SoC)",0.25,"Treatment Effects","multiplier",0.01,NA,0.01,
      se=(0.42-0.13)/(2*1.96),distribution="lognormal",dist_note="SE on log scale; 95% CI 0.13-0.42"),
    p("rr_severe","RR: Severe Flare-Up (Lunavar vs SoC)",0.18,"Treatment Effects","multiplier",0.01,NA,0.01,
      se=(0.35-0.08)/(2*1.96),distribution="lognormal",dist_note="SE on log scale; 95% CI 0.08-0.35"),
    # ---- Flare-up proportions ----
    p("prop_mild","Proportion: Mild Flare-Ups",0.72,"Flare-Up Mix","probability",0,1,0.01,
      se=0.01,distribution="beta",dist_note="Beta; SE=0.01"),
    p("prop_moderate","Proportion: Moderate Flare-Ups",0.12,"Flare-Up Mix","probability",0,1,0.01,
      se=0.01,distribution="beta",dist_note="Beta; SE=0.01"),
    p("prop_severe","Proportion: Severe Flare-Ups",0.16,"Flare-Up Mix","probability",0,1,0.01,
      se=0.01,distribution="beta",dist_note="Beta; SE=0.01"),
    # ---- Severe flare mortality ----
    p("prob_death_severe_flare","Prob: Death During Severe Flare-Up",0.0055,"Mortality","probability",0,1,0.0001,
      se=0.001,distribution="beta",dist_note="Beta; SE=0.001"),
    # ---- Utilities ----
    p("utility_stable_lunavar","Utility: Stable (Lunavar+SoC)",0.805,"Utilities","utility",0,1,0.005,
      se=(0.82-0.79)/(2*1.96),distribution="utility",dist_note="1 - Gamma method; 95% CI 0.790-0.820"),
    p("utility_stable_soc","Utility: Stable (SoC)",0.760,"Utilities","utility",0,1,0.005,
      se=0.01,distribution="utility",dist_note="1 - Gamma method; SE=0.01"),
    p("disutility_mild","Disutility: Mild Flare-Up",0.08,"Utilities","other",0,NA,0.005,
      se=NA,distribution="fixed",dist_note="Fixed in PSA"),
    p("disutility_moderate","Disutility: Moderate Flare-Up",0.14,"Utilities","other",0,NA,0.005,
      se=NA,distribution="fixed",dist_note="Fixed in PSA"),
    p("disutility_severe","Disutility: Severe Flare-Up",0.22,"Utilities","other",0,NA,0.01,
      se=NA,distribution="fixed",dist_note="Fixed in PSA"),
    # ---- Drug costs ----
    p("cost_drug_lunavar_annual","Drug Cost: Lunavar (annual, CAD)",4800,"Costs - Drug","cost",0,NA,100,
      se=4800*0.20,distribution="gamma",dist_note="20% relative SE"),
    p("cost_drug_soc_annual","Drug Cost: SoC (annual, CAD)",5200,"Costs - Drug","cost",0,NA,100,
      se=NA,distribution="fixed",dist_note="Fixed in PSA"),
    # ---- Event costs ----
    p("cost_mild_event","Cost: Mild Flare-Up Event (CAD)",1350,"Costs - Event","cost",0,NA,50,
      se=(1950-850)/(2*1.96),distribution="gamma",dist_note="95% CI 850-1950"),
    p("cost_moderate_event","Cost: Moderate Flare-Up Event (CAD)",1850,"Costs - Event","cost",0,NA,50,
      se=(2600-1200)/(2*1.96),distribution="gamma",dist_note="95% CI 1200-2600"),
    p("cost_severe_event","Cost: Severe Flare-Up Event (CAD)",8200,"Costs - Event","cost",0,NA,100,
      se=(11500-5500)/(2*1.96),distribution="gamma",dist_note="95% CI 5500-11500")
  )
  registry
}

#' Validate Model Parameters
#' @param params Named list of model parameters.
#' @return NULL (invisible). Throws error if validation fails.
#' @export
#' @examples
#' params <- get_default_parameters()
#' validate_parameters(params)
validate_parameters <- function(params) {
  required <- c("cycle_length_years","time_horizon_years","discount_rate","starting_age")
  missing_p <- setdiff(required, names(params))
  if (length(missing_p) > 0)
    stop("Missing required parameters: ", paste(missing_p, collapse=", "), call.=FALSE)
  if (params$time_horizon_years <= 0) stop("time_horizon_years must be positive", call.=FALSE)
  if (params$cycle_length_years <= 0) stop("cycle_length_years must be positive", call.=FALSE)
  invisible(NULL)
}

#' Generate PSA Parameters (Internal)
#'
#' Samples one draw from PSA distributions. Fixed-distribution parameters
#' are returned unchanged.
#'
#' @param params Base parameter list (means).
#' @param se_overrides Optional named list of SE overrides.
#' @return PSA-sampled named list.
#' @importFrom stats rbeta rgamma rnorm
#' @noRd
generate_psa_parameters <- function(params, se_overrides=NULL) {
  psa_params <- params
  reg <- get_parameter_registry()
  for (i in seq_len(nrow(reg))) {
    nm   <- reg$name[i]
    val  <- params[[nm]]
    dist <- tolower(reg$distribution[i])
    se   <- if (!is.null(se_overrides) && !is.null(se_overrides[[nm]])) se_overrides[[nm]] else reg$se[i]
    if (is.null(val) || is.na(dist) || dist=="fixed" || is.na(se) || se<=0) next
    psa_params[[nm]] <- switch(dist,
      "beta" = {
        var_val <- se^2
        if (val>0 && val<1 && var_val<val*(1-val)) {
          alpha  <- val*(val*(1-val)/var_val - 1)
          beta_p <- (1-val)*(val*(1-val)/var_val - 1)
          rbeta(1, alpha, beta_p)
        } else { val }
      },
      "gamma"    = { if (val>0) rgamma(1, shape=(val/se)^2, rate=val/se^2) else 0 },
      "lognormal"= { if (val>0) exp(rnorm(1, log(val), se)) else val },
      "utility"  = {
        one_minus <- 1 - val
        if (one_minus>0) 1 - rgamma(1, shape=(one_minus/se)^2, rate=one_minus/se^2) else val
      },
      "normal"   = rnorm(1, val, se),
      val
    )
  }
  psa_params
}
