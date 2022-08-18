#' Demo app
#'
#' Come in handy when no data...
#'
#' @return Shiny app object
#' @export
#'

noDataApp <- function() {

  regTitle <-  "RAPPORTEKET MUSKELREGISTERET"

  ui <- shiny::navbarPage(
    title = shiny::div(
      shiny::a(
        shiny::includeHTML(
          system.file("www/logo.svg", package = "rapbase")
        )
      ),
      regTitle
    ),
    id = "muskel_app_id",
    windowTitle = regTitle,
    theme = "rap/bootstrap.css",
    tabPanel(
      "Startside",
      rapbase::navbarWidgetInput("muskelNavbarWidget")
    ),

    tabPanel(
      "Bruksstatistikk",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::statsInput("muskelStats"),
          rapbase::statsGuideUI("muskelStatsGuide")
        ),
        shiny::mainPanel(
          rapbase::statsUI("muskelStats")
        )
      )
    )
  )

  server <- function(input, output, session) {
    rapbase::navbarWidgetServer("muskelNavbarWidget", "Test Org")
    rapbase::statsServer("muskelStats", "muskel")
    rapbase::statsGuideServer("muskelStatsGuide", "muskel")
  }

  shiny::shinyApp(ui, server)
}
