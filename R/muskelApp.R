#' Run the muskel Shiny Application
#'
#' @return An object representing the muskel app
#' @export

muskelApp <- function(logAsJson = FALSE) {
  if (logAsJson) {
    rapbase::loggerSetup()
  }
  shiny::shinyApp(ui = appUi, server = appServer)
}
