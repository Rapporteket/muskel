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
    merge(RegData |>
            dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
            dplyr::select(PasientID, Foedselsdato),
          by.x = "PATIENT_ID", by.y = "PasientID", all.x = TRUE) |>
    dplyr::mutate(alder_v_reg = muskel::age(Foedselsdato, ASSESSMENT_DATE))

  map_avdeling <- data.frame(
    UnitId = unique(RegData$AvdRESH),
    orgname = RegData$SykehusNavn[
      match(unique(RegData$AvdRESH), RegData$AvdRESH)]
  )

  rapbase::appLogger(session = session, msg = "Muskel: shiny app starter")

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "NORNMD",
    caller = "muskel",
    map_orgname = shiny::req(map_avdeling)
  )

  shiny::observeEvent(
    shiny::req(user$role()), {
      if (user$role() != 'SC') {
        shiny::hideTab("muskel_app_id", target = "Eksport")
        shiny::hideTab("muskel_app_id", target = "Verktøy")
      } else {
        shiny::showTab("muskel_app_id", target = "Eksport")
        shiny::showTab("muskel_app_id", target = "Verktøy")
      }
    })


  muskel::fordelingsfig_server("fordeling_id", RegData=RegData, reshID = user$org)

  muskel::fordeling_grvar_server("forgrvar", RegData=RegData, reshID = user$org,
                                 ss = session)

  muskel::kumulativAndel_server("kumAnd", RegData=RegData,
                                reshID = user$org, ss = session)

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
    reportParams = reportParams
  )

  muskel::admtab_server("muskeltabell", RegData=RegData,
                        SkjemaOversikt=SkjemaOversikt,
                        SMAoversikt=SMAoversikt, ss = session,
                        userRole=user$role)

  muskel::datadump_server("dataDumpMuskel", userRole=user$role,
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
    runAutoReportButton = TRUE
  )

  # ## Dispatchment
  #
  #
  # vis_rapp <- reactiveVal(FALSE)
  # observeEvent(user$role(), {
  #   vis_rapp(user$role() == "SC")
  # })
  # disParamNames <- shiny::reactive(c("reshID"))
  # disParamValues <- shiny::reactive(c(org$value()))
  #
  # rapbase::autoReportServer(
  #   id = "muskelDispatch",
  #   registryName = "muskel",
  #   type = "dispatchment",
  #   org = org$value,
  #   paramNames = disParamNames,
  #   paramValues = disParamValues,
  #   reports = list(
  #     "SMA-rapport" = list(
  #       synopsis = "NORNMD: SMA-rapport",
  #       fun = "lag_SMArapport",
  #       paramNames = c("baseName", "reshID"),
  #       paramValues = c("SMArapport", 999999)
  #     )
  #   ),
  #   orgs = orgs,
  #   eligible = vis_rapp,
  #   freq = "quarter",
  #   user = user
  # )

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
