# R/app_launcher.R
#' Launch Interactive ABD Cost-Effectiveness Model
#' @param ... Arguments passed to shiny::runApp.
#' @return Invisible NULL.
#' @export
#' @examples
#' \dontrun{ launch_app() }
launch_app <- function(...) {
  if (!requireNamespace("shiny", quietly=TRUE))
    stop("shiny must be installed. Run: install.packages('shiny')", call.=FALSE)
  app_dir <- system.file("shiny-app", package="huncMarkovABD")
  if (!nzchar(app_dir) || !file.exists(app_dir))
    stop("Shiny app directory not found.", call.=FALSE)
  shiny::runApp(app_dir, ...)
}
