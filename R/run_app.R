#' Run the StatDesign Shiny Application
#'
#' @description Launches the StatDesign interactive study design and sampling
#'   platform in your default web browser.
#'
#' @param ... Arguments passed to \code{\link[golem]{with_golem_options}}.
#'
#' @return No return value, called for side effects.
#'
#' @examples
#' \dontrun{
#'   StatDesign::run_app()
#' }
#'
#' @export
run_app <- function(...) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui     = app_ui(),
      server = app_server
    ),
    golem_opts = list(...)
  )
}
