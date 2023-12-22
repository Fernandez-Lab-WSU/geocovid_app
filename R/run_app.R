#' Launches the app
#'
#' @return GeoCovid app
#' @importFrom shiny shinyApp
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_app()
#' }
run_app <- function() {
  
  shiny::shinyApp(ui = ui, server = server)
}