#' Launches the app
#'
#' @return GeoCovid app
#' @importFrom shiny shinyApp
#'
#' @export
#'
#' @examples
#' \dontrun{
#' runapp()
#' }
runapp <- function() {
  
  shiny::shinyApp(ui = ui, server = server)
}