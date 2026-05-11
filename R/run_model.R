# R/run_model.R
# Author: Ian Cromwell <healthyuncertainty@gmail.com>

#' Run ABD Cost-Effectiveness Model
#'
#' Runs the ABD Markov model for both arms (Lunavar+SoC and SoC) and
#' returns costs, QALYs, life years, and ICER. Supports deterministic
#' (n_sim = 1) and probabilistic sensitivity analysis (n_sim > 1).
#'
#' @param parameters Named list of model parameters. NULL uses defaults.
#' @param n_sim Integer. PSA iterations (1 = deterministic).
#' @param return_trace Logical. Include Markov traces in output?
#' @param seed Optional integer random seed.
#' @param se_overrides Optional named list of SE overrides for PSA.
#' @return List with results_summary, psa_results, icer, parameters,
#'   and optionally trace_data.
#' @export
#' @examples
#' res <- run_model()
#' res$icer
run_model <- function(parameters=NULL, n_sim=1, return_trace=FALSE,
                      seed=NULL, se_overrides=NULL) {
  if (is.null(parameters)) parameters <- get_default_parameters()
  validate_parameters(parameters)
  if (!is.null(seed)) set.seed(seed)

  res_l <- run_arm(parameters, "Lunavar")
  res_s <- run_arm(parameters, "SoC")

  results_summary <- data.frame(
    treatment  = c("Lunavar", "SoC"),
    total_cost = c(res_l$total_cost, res_s$total_cost),
    total_qaly = c(res_l$total_qaly, res_s$total_qaly),
    total_ly   = c(res_l$total_ly,   res_s$total_ly),
    stringsAsFactors = FALSE
  )
  inc_cost <- res_l$total_cost  - res_s$total_cost
  inc_qaly <- res_l$total_qaly  - res_s$total_qaly
  icer_val <- if (abs(inc_qaly) > 1e-6) inc_cost / inc_qaly else NA_real_

  psa_results <- data.frame(
    Lunavar_cost = res_l$total_cost, Lunavar_qaly = res_l$total_qaly,
    SoC_cost     = res_s$total_cost, SoC_qaly     = res_s$total_qaly
  )

  if (n_sim > 1) {
    psa_mat <- matrix(NA_real_, nrow=n_sim, ncol=4,
      dimnames=list(NULL, c("Lunavar_cost","Lunavar_qaly","SoC_cost","SoC_qaly")))
    for (j in seq_len(n_sim)) {
      p_j  <- generate_psa_parameters(parameters, se_overrides=se_overrides)
      rl_j <- run_arm(p_j, "Lunavar")
      rs_j <- run_arm(p_j, "SoC")
      psa_mat[j,] <- c(rl_j$total_cost, rl_j$total_qaly,
                        rs_j$total_cost, rs_j$total_qaly)
    }
    psa_results <- as.data.frame(psa_mat)
  }

  out <- list(results_summary=results_summary, psa_results=psa_results,
              icer=icer_val, parameters=parameters)
  if (return_trace)
    out$trace_data <- list(Lunavar=res_l$trace, SoC=res_s$trace)
  out
}
