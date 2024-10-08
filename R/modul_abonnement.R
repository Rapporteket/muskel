#' UI-del av modul for abonnement i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul abonnement
#'
#' @export
abonnement_UI <- function(id, BrValg){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    sidebarPanel(width = 3,
                 selectInput(ns("subscriptionRep"), "Rapport:",
                             c("Kvartalsrapport")),
                 selectInput(ns("subscriptionFreq"), "Frekvens:",
                             list(Årlig="Årlig-year",
                                   Kvartalsvis="Kvartalsvis-quarter",
                                   Månedlig="Månedlig-month",
                                   Ukentlig="Ukentlig-week",
                                   Daglig="Daglig-DSTday"),
                             selected = "Månedlig-month"),
                 #selectInput("subscriptionFileFormat", "Format:",
                 #            c("html", "pdf")),
                 actionButton(ns("subscribe"), "Bestill!")
    ),
    mainPanel(
      uiOutput(outputId = ns('subscriptionContent'))
    )
  )

}



#' Server-del av modul for abonnement i NORGAST sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul abonnement
#'
#' @export
abonnement <- function(input, output, session, reshID, userRole, hvd_session){


  ## reaktive verdier for å holde rede på endringer som skjer mens
  ## applikasjonen kjører
  rv <- reactiveValues(
    subscriptionTab = rapbase::makeUserSubscriptionTab(hvd_session))

  ## lag tabell over gjeldende status for abonnement
  output$activeSubscriptions <- DT::renderDataTable(
    rv$subscriptionTab, server = FALSE, escape = FALSE, selection = 'none',
    rownames = FALSE, options = list(dom = 't')
  )

  ## lag side som viser status for abonnement, også når det ikke finnes noen
  output$subscriptionContent <- renderUI({
    ns <- session$ns
    fullName <- rapbase::getUserFullName(hvd_session)
    if (length(rv$subscriptionTab) == 0) {
      p(paste("Ingen aktive abonnement for", fullName))
    } else {
      tagList(
        p(paste("Aktive abonnement for", fullName, "som sendes per epost til ",
                rapbase::getUserEmail(hvd_session), ":")),
        DT::dataTableOutput(ns("activeSubscriptions"))
      )
    }
  })
  ## nye abonnement
  observeEvent (input$subscribe, { #MÅ HA
    owner <- rapbase::getUserName(hvd_session)
    interval <- strsplit(input$subscriptionFreq, "-")[[1]][2]
    intervalName <- strsplit(input$subscriptionFreq, "-")[[1]][1]
    organization <- rapbase::getUserReshId(hvd_session)
    runDayOfYear <- rapbase::makeRunDayOfYearSequence(interval = interval)
    email <- rapbase::getUserEmail(hvd_session)
    if (input$subscriptionRep == "Kvartalsrapport") {
      synopsis <- "norgast/Rapporteket: kvartalsrapport"
      baseName <- "NorgastKvartalsrapport_abonnement" #Navn på fila
      #print(rnwFil)
    }

    fun <- "abonnement_kvartal_norgast"  #"henteSamlerapporter"

    paramNames <- c('baseName', "reshID")
    paramValues <- c(baseName, reshID) #input$subscriptionFileFormat)

    rapbase::createAutoReport(synopsis = synopsis, package = 'norgast',
                              fun = fun, paramNames = paramNames,
                              paramValues = paramValues, owner = owner,
                              email = email, organization = organization,
                              runDayOfYear = runDayOfYear, interval = interval,
                              intervalName = intervalName)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(hvd_session)
  })


  ## slett eksisterende abonnement
  observeEvent(input$del_button, {
    selectedRepId <- strsplit(input$del_button, "_")[[1]][2]
    rapbase::deleteAutoReport(selectedRepId)
    rv$subscriptionTab <- rapbase::makeUserSubscriptionTab(hvd_session)
  })



  shiny::observe({
    if (rapbase::isRapContext()) {

      shinyjs::onclick(
        "subscribe",
        rapbase::repLogger(
          session = hvd_session,
          msg = paste0(
            "NORGAST: abonnement bestilt: ",
            input$subscriptionRep
          )
        )
      )

    }
  })





}
