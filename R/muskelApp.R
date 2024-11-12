#' Run the muskel Shiny Application
#'
#' @return An object representing the muskel app
#' @export

muskelApp <- function() {
  shiny::shinyApp(ui = appUi, server = appServer)
}
