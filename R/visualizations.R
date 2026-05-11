# R/visualizations.R
# CEA visualisations: CE plane, CEAC, Markov trace, ICER table
# Author: Ian Cromwell <healthyuncertainty@gmail.com>

#' Plot Cost-Effectiveness Plane
#' @param psa_results Data frame from run_model().
#' @param wtp Numeric. WTP threshold (default 100000).
#' @return A ggplot2 object.
#' @importFrom stats median
#' @export
#' @examples
#' \dontrun{
#' res <- run_model(n_sim=500)
#' plot_ce_plane(res$psa_results)
#' }
plot_ce_plane <- function(psa_results, wtp=100000) {
  if (!requireNamespace("ggplot2", quietly=TRUE)) stop("ggplot2 is required", call.=FALSE)
  inc_cost <- psa_results$Lunavar_cost - psa_results$SoC_cost
  inc_qaly <- psa_results$Lunavar_qaly - psa_results$SoC_qaly
  df <- data.frame(inc_qaly=inc_qaly, inc_cost=inc_cost)
  ggplot2::ggplot(df, ggplot2::aes(x=inc_qaly, y=inc_cost)) +
    ggplot2::geom_point(alpha=0.25, size=0.8, colour="steelblue") +
    ggplot2::stat_ellipse(level=0.95, colour="steelblue", linewidth=0.8, linetype="dotted") +
    ggplot2::geom_point(data=data.frame(inc_qaly=mean(inc_qaly),inc_cost=mean(inc_cost)),
                        ggplot2::aes(x=inc_qaly,y=inc_cost),colour="navy",shape=17,size=4) +
    ggplot2::geom_hline(yintercept=0,linetype="dashed",colour="grey50") +
    ggplot2::geom_vline(xintercept=0,linetype="dashed",colour="grey50") +
    ggplot2::geom_abline(slope=wtp,intercept=0,colour="red",linewidth=0.6) +
    ggplot2::scale_y_continuous(
      labels=function(x) paste0("$",formatC(x,format="f",digits=0,big.mark=","))) +
    ggplot2::labs(title="Cost-Effectiveness Plane",
                  subtitle=paste0("Lunavar+SoC vs SoC | WTP = $",format(wtp,big.mark=",")),
                  x="Incremental QALYs", y="Incremental Costs (CAD)") +
    ggplot2::theme_minimal(base_size=13) +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold"),
                   panel.grid.minor=ggplot2::element_blank())
}

#' Plot Cost-Effectiveness Acceptability Curve
#' @param psa_results Data frame from run_model().
#' @param wtp_range Numeric vector of WTP thresholds.
#' @return A ggplot2 object.
#' @export
#' @examples
#' \dontrun{
#' res <- run_model(n_sim=500)
#' plot_ceac(res$psa_results)
#' }
plot_ceac <- function(psa_results, wtp_range=seq(0,300000,by=5000)) {
  if (!requireNamespace("ggplot2", quietly=TRUE)) stop("ggplot2 is required", call.=FALSE)
  prob_ce <- vapply(wtp_range, function(wtp) {
    nmb_l <- psa_results$Lunavar_qaly*wtp - psa_results$Lunavar_cost
    nmb_s <- psa_results$SoC_qaly*wtp    - psa_results$SoC_cost
    mean(nmb_l > nmb_s)
  }, numeric(1))
  df_ceac <- data.frame(wtp=rep(wtp_range,2), prob=c(prob_ce,1-prob_ce),
                        arm=rep(c("Lunavar+SoC","SoC"),each=length(wtp_range)))
  ggplot2::ggplot(df_ceac, ggplot2::aes(x=wtp, y=prob, colour=arm)) +
    ggplot2::geom_line(linewidth=1.1) +
    ggplot2::scale_y_continuous(
      labels=function(x) paste0(round(x*100),"%%"), limits=c(0,1)) +
    ggplot2::scale_x_continuous(
      labels=function(x) paste0("$",format(x,big.mark=",",scientific=FALSE))) +
    ggplot2::labs(title="Cost-Effectiveness Acceptability Curve",
                  subtitle="Probability each treatment has highest net monetary benefit",
                  x="Willingness-to-Pay Threshold (CAD)",
                  y="Probability Cost-Effective", colour="Treatment") +
    ggplot2::theme_minimal(base_size=13) +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold"),
                   panel.grid.minor=ggplot2::element_blank())
}

#' Plot Markov Trace
#'
#' State occupancy over time. Tunnel states are shown so their brief
#' occupancy (one cycle each) is visible. Dead state excluded.
#'
#' @param trace_data List with Lunavar and SoC trace matrices.
#' @param treatment Character. `"Lunavar"`, `"SoC"`, or `"both"`.
#' @return A ggplot2 object.
#' @export
#' @examples
#' \dontrun{
#' res <- run_model(return_trace=TRUE)
#' plot_trace(res$trace_data)
#' }
plot_trace <- function(trace_data, treatment="both") {
  if (!requireNamespace("ggplot2", quietly=TRUE)) stop("ggplot2 is required", call.=FALSE)
  state_names_alive <- c("Stable","FlareUp_Mild","FlareUp_Moderate","FlareUp_Severe")
  arms <- if (treatment == "both") names(trace_data) else treatment
  df <- do.call(rbind, lapply(arms, function(arm) {
    tr <- trace_data[[arm]]
    df_arm <- as.data.frame(tr)
    df_arm$cycle <- as.integer(rownames(tr))
    df_arm$year  <- df_arm$cycle * (2/52)
    df_arm$arm   <- arm
    df_arm
  }))
  df_long <- do.call(rbind, lapply(state_names_alive, function(s)
    data.frame(year=df$year, arm=df$arm, state=s, occupancy=df[[s]], stringsAsFactors=FALSE)))
  df_long$state <- factor(df_long$state, levels=state_names_alive)
  p <- ggplot2::ggplot(df_long, ggplot2::aes(x=year, y=occupancy, colour=state)) +
    ggplot2::geom_line(linewidth=0.7) +
    ggplot2::scale_y_continuous(
      labels=function(x) paste0(round(x*100),"%%"), limits=c(0,NA)) +
    ggplot2::labs(title="Markov Trace - State Occupancy Over Time",
                  x="Year", y="Proportion of Cohort", colour="Health State") +
    ggplot2::theme_minimal(base_size=13) +
    ggplot2::theme(plot.title=ggplot2::element_text(face="bold"),
                   panel.grid.minor=ggplot2::element_blank())
  if (length(arms) > 1) p <- p + ggplot2::facet_wrap(~ arm, ncol=1)
  p
}

#' Create ICER Summary Table
#' @param psa_results Data frame from run_model().
#' @return Data frame with ICER table.
#' @export
#' @examples
#' res <- run_model()
#' create_icer_table(res$psa_results)
create_icer_table <- function(psa_results) {
  cost_l <- mean(psa_results$Lunavar_cost); qaly_l <- mean(psa_results$Lunavar_qaly)
  cost_s <- mean(psa_results$SoC_cost);     qaly_s <- mean(psa_results$SoC_qaly)
  if (requireNamespace("dampack", quietly=TRUE)) {
    tbl <- dampack::calculate_icers(cost=c(cost_s,cost_l), effect=c(qaly_s,qaly_l),
                                    strategies=c("SoC","Lunavar+SoC"))
    return(as.data.frame(tbl))
  }
  data.frame(Strategy=c("SoC","Lunavar+SoC"), Cost=c(cost_s,cost_l),
             Effect=c(qaly_s,qaly_l), Inc_Cost=c(NA,cost_l-cost_s),
             Inc_Effect=c(NA,qaly_l-qaly_s), ICER=c(NA,(cost_l-cost_s)/(qaly_l-qaly_s)),
             stringsAsFactors=FALSE)
}
