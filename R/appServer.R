#' Server logic for the muskel app
#'
#' @param input shiny input object
#' @param output shiny output object
#' @param session shiny session object
#'
#' @return A shiny app server object
#' @export

appServer <- function(input, output, session) {

  RegData <- muskel::MuskelHentRegData() |>
    muskel::MuskelPreprosess()
  SkjemaOversikt <- rapbase::loadRegData(
    registryName = "data",
    dbType = "mysql",
    query = "SELECT *
             FROM skjemaoversikt"
  )
  SMAoversikt <- rapbase::loadRegData(
    registryName = "data",
    dbType = "mysql",
    query = "SELECT sma.*, m.PATIENT_ID
             FROM smafollowup sma LEFT JOIN mce m ON sma.MCEID = m.MCEID"
  ) |> dplyr::relocate(PATIENT_ID) |>
    merge(
      RegData |>
        dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
        dplyr::select(PasientID, Foedselsdato),
      by.x = "PATIENT_ID", by.y = "PasientID", all.x = TRUE) |>
    dplyr::mutate(alder_v_reg =
                    muskel::age(Foedselsdato, ASSESSMENT_DATE))

  map_avdeling <- data.frame(
    UnitId = unique(RegData$AvdRESH),
    orgname = RegData$SykehusNavn[
      match(unique(RegData$AvdRESH), RegData$AvdRESH)]
  )

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "NORNMD",
    caller = "muskel",
    map_orgname = shiny::req(map_avdeling)
  )

  # Legg til SC-spesifikke faner, og fjern dem for andre roller
  tabs_added <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(
    shiny::req(user$role()),
    {
      if (user$role() %in% c("SC", "LC")) {
        if (!tabs_added()) {
          shiny::insertTab(
            "muskel_app_id",
            tab = shiny::tabPanel(
              "Datadump",
              muskel::datadump_ui("dataDumpMuskel"),
              value = "dataDumpMuskel"
            ),
            target = "abonnement_id", position = "before"
          )
          shiny::insertTab(
            "muskel_app_id",
            tab = shiny::tabPanel(
              "Medikamentforløp SMA",
              muskel::medikament_sma_ui("medikament_sma_id"),
              value = "medikament_sma_id"
            ),
            target = "SMA-rapport", position = "after"
          )
          tabs_added(TRUE)
        }
      } else {
        if (tabs_added()) {
          shiny::removeTab("muskel_app_id",
                           target = "dataDumpMuskel")
          shiny::removeTab("muskel_app_id",
                           target = "medikament_sma_id")
          tabs_added(FALSE)
        }
      }
    }
  )

  # Legg til verktøy-fanen for SC-brukere, og fjern den for andre roller
  tool_tabs_added <- shiny::reactiveVal(FALSE)

  shiny::observeEvent(shiny::req(user$role()), {
    if (user$role() == "SC") {
      if (!tool_tabs_added()) {
        shiny::appendTab(
          inputId = "muskel_app_id",
          tab = shiny::navbarMenu(
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
              "Metadata",
              shiny::sidebarLayout(
                shiny::sidebarPanel(shiny::uiOutput("metaControl")),
                shiny::mainPanel(shiny::htmlOutput("metaData"))
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
        tool_tabs_added(TRUE)
      }
    } else {
      if (tool_tabs_added()) {
        shiny::removeTab("muskel_app_id", target = "Verktøy")
        tool_tabs_added(FALSE)
      }
    }
  })

  muskel::fordelingsfig_server("fordeling_id",
                               RegData=RegData,
                               reshID = user$org)

  muskel::fordeling_grvar_server("forgrvar",
                                 RegData=RegData,
                                 reshID = user$org,
                                 ss = session)

  muskel::kumulativAndel_server("kumAnd",
                                RegData=RegData,
                                reshID = user$org,
                                ss = session)

  reportParams <- shiny::reactive(
    list(
      reshID = user$org(),
      userRole = user$role(),
      shinySession = session
    )
  )

  muskel::defaultReportServer(
    id = "smarapp",
    reportFileName = reactiveVal("SMArapport_abo_v2.Rmd"),
    reportParams = reportParams,
    avdeling = setNames(map_avdeling$UnitId,
                        map_avdeling$orgname)
  )

  muskel::medikament_sma_server(
    id = "medikament_sma_id",
    SMAoversikt = SMAoversikt,
    user = user
  )

  muskel::admtab_server(
    "muskeltabell", RegData=RegData,
    SkjemaOversikt=SkjemaOversikt,
    SMAoversikt=SMAoversikt, ss = session,
    userRole=user$role)

  muskel::datadump_server(
    "dataDumpMuskel", userRole=user$role,
    reshID = user$org, mainSession = session)



  ##############################################################################
  ################ Subscription, Dispatchment and Stats ########################
  orgs <- as.list(setNames(
    as.numeric(unique(RegData$AvdRESH)),
    RegData$SykehusNavn[match(unique(RegData$AvdRESH), RegData$AvdRESH)]))
  org <- rapbase::autoReportOrgServer("muskelDispatch", orgs)

  subParamNames <- shiny::reactive(c("reshID"))
  subParamValues <- shiny::reactive(user$org())

  ## Subscription

  rapbase::autoReportServer(
    id = "muskelSubscription",
    registryName = "muskel",
    type = "subscription",
    paramNames = subParamNames,
    paramValues = subParamValues,
    reports = list(
      "SMA-rapport - pdf" = list(
        synopsis = "NORNMD: SMA-rapport - pdf",
        fun = "muskel_kjor_autorapport",
        paramNames = c("report", "outputType", "abonnement", "reshID"),
        paramValues = c("SMArapport_abo_v2.Rmd",
                        "pdf_document", TRUE, 999999)
      ),
      "SMA-rapport - html" = list(
        synopsis = "NORNMD: SMA-rapport - html",
        fun = "muskel_kjor_autorapport",
        paramNames = c("report", "outputType", "abonnement", "reshID"),
        paramValues = c("SMArapport_abo_v2.Rmd",
                        "html_document", TRUE, 999999)
      )
    ),
    orgs = orgs,
    freq = "quarter",
    user = user,
    runAutoReportButton = FALSE
  )

  ## Dispatchment


  vis_rapp <- reactiveVal(FALSE)
  observeEvent(user$role(), {
    vis_rapp(user$role() == "SC")
  })
  disParamNames <- shiny::reactive(c("reshID"))
  disParamValues <- shiny::reactive(c(org$value()))

  rapbase::autoReportServer(
    id = "muskelDispatch",
    registryName = "muskel",
    type = "dispatchment",
    org = org$value,
    paramNames = disParamNames,
    paramValues = disParamValues,
    reports = list(
      "SMA-rapport - pdf" = list(
        synopsis = "NORNMD: SMA-rapport - pdf",
        fun = "muskel_kjor_autorapport",
        paramNames = c("report", "outputType", "abonnement", "reshID"),
        paramValues = c("SMArapport_abo_v2.Rmd",
                        "pdf_document", TRUE, 999999)
      ),
      "SMA-rapport - html" = list(
        synopsis = "NORNMD: SMA-rapport - html",
        fun = "muskel_kjor_autorapport",
        paramNames = c("report", "outputType", "abonnement", "reshID"),
        paramValues = c("SMArapport_abo_v2.Rmd",
                        "html_document", TRUE, 999999)
      )
    ),
    orgs = orgs,
    eligible = vis_rapp,
    freq = "quarter",
    user = user,
    runAutoReportButton = FALSE
  )

  ## Metadata
  meta <- shiny::reactive({
    rapbase::describeRegistryDb("data")
  })

  output$metaControl <- shiny::renderUI({
    tabs <- names(meta())
    selectInput("metaTab", "Velg tabell:", tabs)
  })


  output$metaDataTable <- DT::renderDataTable(
    meta()[[input$metaTab]], rownames = FALSE,
    options = list(lengthMenu=c(25, 50, 100, 200, 400))
  )

  output$metaData <- shiny::renderUI({
    DT::dataTableOutput("metaDataTable")
  })

  ##############################################################################
  # Eksport  #
  rapbase::exportUCServer("muskelExport", "muskel")
  ## veileding
  rapbase::exportGuideServer("muskelExportGuide", "muskel")

  ## Stats
  shiny::observe(
    rapbase::statsServer("muskelStats", registryName = "muskel",
                         app_id = Sys.getenv("FALK_APP_ID"),
                         eligible = (user$role() == "SC"))
  )
  rapbase::statsGuideServer("muskelStatsGuide", registryName = "muskel")






}
