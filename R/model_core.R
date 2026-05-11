# R/model_core.R
# Core model: flare probabilities, transition matrix, arm runner
# Author: Ian Cromwell <healthyuncertainty@gmail.com>

.state_names <- c("Stable","FlareUp_Mild","FlareUp_Moderate","FlareUp_Severe","Dead")
.n_states    <- length(.state_names)
.i_stable <- 1L; .i_mild <- 2L; .i_moderate <- 3L; .i_severe <- 4L; .i_dead <- 5L

#' Calculate Per-Cycle Flare-Up Probabilities
#'
#' Converts annual flare-up event rates to per-cycle probabilities using the
#' Poisson rate conversion (1 - exp(-rate * cycle_length)). Applies treatment
#' RRs when arm is Lunavar.
#'
#' @param params Named list of model parameters.
#' @param arm Character. `"Lunavar"` or `"SoC"`.
#' @return Named numeric vector with p_mild, p_mod, p_sev.
#' @export
#' @examples
#' params <- get_default_parameters()
#' calc_flare_probs(params, "Lunavar")
calc_flare_probs <- function(params, arm) {
  aaer <- params$aaer_soc
  cl   <- params$cycle_length_years
  if (arm == "Lunavar") {
    r_mild <- aaer * params$prop_mild     * params$rr_mild
    r_mod  <- aaer * params$prop_moderate * params$rr_moderate
    r_sev  <- aaer * params$prop_severe   * params$rr_severe
  } else {
    r_mild <- aaer * params$prop_mild
    r_mod  <- aaer * params$prop_moderate
    r_sev  <- aaer * params$prop_severe
  }
  c(p_mild = 1 - exp(-r_mild * cl),
    p_mod  = 1 - exp(-r_mod  * cl),
    p_sev  = 1 - exp(-r_sev  * cl))
}

#' Build Transition Matrix
#'
#' Constructs the 5x5 per-cycle transition probability matrix. Flare-up
#' states are tunnel states: patients always exit after exactly one cycle.
#' Background mortality is age-dependent; severe flare-ups carry additional
#' mortality risk.
#'
#' @param params Named list of model parameters.
#' @param arm Character. `"Lunavar"` or `"SoC"`.
#' @param cycle Integer. Current cycle number (1-based).
#' @return 5x5 numeric transition probability matrix.
#' @export
#' @examples
#' params <- get_default_parameters()
#' m <- build_tmat(params, "SoC", cycle = 1)
#' rowSums(m)
build_tmat <- function(params, arm, cycle) {
  age      <- params$starting_age + (cycle - 1) * params$cycle_length_years
  p_bg     <- get_background_mortality(age, params$cycle_length_years)
  fp       <- calc_flare_probs(params, arm)
  p_ds     <- params$prob_death_severe_flare
  m <- matrix(0, nrow=.n_states, ncol=.n_states, dimnames=list(.state_names,.state_names))
  # Stable -> {Stable, Mild, Moderate, Severe, Dead}
  p_any_flare <- fp["p_mild"] + fp["p_mod"] + fp["p_sev"]
  p_any_flare <- min(p_any_flare, 1 - p_bg)  # cap at surviving
  m[.i_stable, .i_stable]   <- (1 - p_any_flare) * (1 - p_bg)
  m[.i_stable, .i_mild]     <- fp["p_mild"] * (1 - p_bg)
  m[.i_stable, .i_moderate] <- fp["p_mod"]  * (1 - p_bg)
  m[.i_stable, .i_severe]   <- fp["p_sev"]  * (1 - p_bg)
  m[.i_stable, .i_dead]     <- p_bg
  # Tunnel states -> Stable (or Dead): forced exit after 1 cycle
  m[.i_mild,     .i_stable] <- 1 - p_bg; m[.i_mild,     .i_dead] <- p_bg
  m[.i_moderate, .i_stable] <- 1 - p_bg; m[.i_moderate, .i_dead] <- p_bg
  m[.i_severe,   .i_stable] <- (1 - p_ds) * (1 - p_bg)
  m[.i_severe,   .i_dead]   <- p_ds + (1 - p_ds) * p_bg
  # Dead: absorbing
  m[.i_dead, .i_dead] <- 1
  # Enforce row sums
  for (r in seq_len(.n_states)) {
    rs <- sum(m[r,])
    if (abs(rs - 1) > 1e-10) m[r, r] <- m[r, r] + (1 - rs)
  }
  m
}

#' Run One Treatment Arm
#'
#' Runs the full Markov simulation for one arm. Returns total discounted
#' costs, QALYs, and life years. Discounting uses the mid-cycle convention.
#'
#' @param params Named list of model parameters.
#' @param arm Character. `"Lunavar"` or `"SoC"`.
#' @return List with total_cost, total_qaly, total_ly, and trace matrix.
#' @export
#' @examples
#' params <- get_default_parameters()
#' res <- run_arm(params, "SoC")
#' res$total_cost
run_arm <- function(params, arm) {
  n_cycles <- ceiling(params$time_horizon_years / params$cycle_length_years)
  cl       <- params$cycle_length_years
  state    <- c(1, 0, 0, 0, 0)
  trace    <- matrix(0, nrow=n_cycles+1, ncol=.n_states,
                     dimnames=list(0:n_cycles, .state_names))
  trace[1,] <- state

  for (cycle in seq_len(n_cycles)) {
    m <- build_tmat(params, arm, cycle)
    trace[cycle+1,] <- trace[cycle,] %*% m
  }

  # Mid-cycle discounting (matches original model convention)
  disc_times <- (seq_len(n_cycles) - 0.5) * cl
  disc       <- 1 / (1 + params$discount_rate)^disc_times
  tr         <- trace[2:(n_cycles+1), , drop=FALSE]

  # Life years
  total_ly <- sum((1 - tr[, .i_dead]) * cl * disc)

  # Drug + event costs per cycle
  if (arm == "Lunavar") {
    cd_cyc <- (params$cost_drug_lunavar_annual + params$cost_drug_soc_annual) * cl
    u_base <- params$utility_stable_lunavar
  } else {
    cd_cyc <- params$cost_drug_soc_annual * cl
    u_base <- params$utility_stable_soc
  }
  cost_per_cycle <- cd_cyc * (1 - tr[, .i_dead]) +
    tr[, .i_mild]     * params$cost_mild_event +
    tr[, .i_moderate] * params$cost_moderate_event +
    tr[, .i_severe]   * params$cost_severe_event
  total_cost <- sum(cost_per_cycle * disc)

  # QALYs
  qaly_per_cycle <- (
    tr[, .i_stable]   * u_base +
    tr[, .i_mild]     * (u_base - params$disutility_mild) +
    tr[, .i_moderate] * (u_base - params$disutility_moderate) +
    tr[, .i_severe]   * (u_base - params$disutility_severe)
  ) * cl
  total_qaly <- sum(qaly_per_cycle * disc)

  list(total_cost=total_cost, total_qaly=total_qaly, total_ly=total_ly, trace=trace)
}
