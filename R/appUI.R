#' Client (ui) for the muskel app
#'
#' @return An shiny app ui object
#' @export

appUi <- function() {

  regTitle = "NORNMD"

  # Define UI for application
  ui <- shiny::navbarPage(
    id = "muskel_app_id",
    title = rapbase::title(regTitle),
    windowTitle = regTitle,
    theme = rapbase::theme(),

    shiny::tabPanel(
      "Startside",
      rapbase::navbarWidgetInput("navbar-widget", selectOrganization = TRUE),

      muskel::startside_ui()
    ),

    shiny::tabPanel(
      "Fordelingsfigur",
      muskel::fordelingsfig_ui("fordeling_id")
    ),

    shiny::tabPanel("Fordelinger etter grupperingsvariabler",
             fordeling_grvar_ui(id = "forgrvar")
    ),
    tabPanel("Kumulative andeler",
             muskel::kumulativAndel_ui(id = "kumAnd")
    ),
    shiny::tabPanel(
      "SMA-rapport",
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          muskel::defaultReportInput("smarapp")
        ),
        shiny::mainPanel(
          muskel::defaultReportUI("smarapp")
        )
      )
    ),
    tabPanel("Administrative tabeller",
             muskel::admtab_ui("muskeltabell")
    ),
    tabPanel("Datadump",
             muskel::datadump_ui("dataDumpMuskel")
    ),

    shiny::tabPanel(
      shiny::span("Abonnement",
                  title="Bestill tilsending av rapporter på e-post"),
      shiny::sidebarLayout(
        shiny::sidebarPanel(
          rapbase::autoReportInput("muskelSubscription")
        ),
        shiny::mainPanel(
          rapbase::autoReportUI("muskelSubscription")
        )
      )
    ),

    shiny::navbarMenu(
      "Verktøy",
      shiny::tabPanel(
        "Utsending",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::autoReportOrgInput("muskelDispatch"),
            rapbase::autoReportInput("muskelDispatch")
          ),
          shiny::mainPanel(
            rapbase::autoReportUI("muskelDispatch")
          )
        )
      ),

      shiny::tabPanel(
        "Eksport",
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            rapbase::exportUCInput("muskelExport")
          ),
          shiny::mainPanel(
            rapbase::exportGuideUI("muskelExportGuide")
          )
        )
      ),

      shiny::tabPanel(
        "Bruksstatistikk",
        shiny::sidebarLayout(
          shiny::sidebarPanel(rapbase::statsInput("muskelStats")),
          shiny::mainPanel(
            rapbase::statsUI("muskelStats"),
            rapbase::statsGuideUI("muskelStatsGuide")
          )
        )
      )
    )

  )
}
