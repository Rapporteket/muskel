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
             FROM SkjemaOversikt"
  )
  SMAoversikt <- rapbase::loadRegData(
    registryName = "data",
    dbType = "mysql",
    query = "SELECT *
             FROM SMAoversikt"
  )
  map_avdeling <- data.frame(
    UnitId = unique(RegData$AvdRESH),
    orgname = RegData$SykehusNavn[match(unique(RegData$AvdRESH), RegData$AvdRESH)])

  rapbase::appLogger(session = session, msg = "Muskel: shiny app starter")
  # reshID <- rapbase::getUserReshId(session)
  # userRole <- rapbase::getUserRole(session)

  # rapbase::navbarWidgetServer("navbar-widget", "NORNMD")

  user <- rapbase::navbarWidgetServer2(
    "navbar-widget",
    orgName = "NORNMD",
    caller = "NORNMD",
    map_orgname = shiny::req(map_avdeling)
  )

  muskel::fordelingsfig_server("fordeling_id", RegData=RegData, reshID = user$org)

  muskel::fordeling_grvar_server("forgrvar", RegData=RegData, reshID = user$org,
                                 ss = session)

  muskel::kumulativAndel_server("kumAnd", RegData=RegData,
                                reshID = user$org, ss = session)

  muskel::admtab_server("muskeltabell", RegData=RegData,
                        SkjemaOversikt=SkjemaOversikt,
                        SMAoversikt=SMAoversikt, ss = session,
                        userRole=user$role)

  muskel::datadump_server("dataDumpMuskel", userRole=user$role,
                          reshID = user$org, mainSession = session)

  # Eksport  #
  rapbase::exportUCServer("muskelExport", "muskel")
  ## veileding
  rapbase::exportGuideServer("muskelExportGuide", "muskel")

  ## Stats
  shiny::observe(
    rapbase::statsServer("muskelStats", registryName = "muskel",
                         eligible = (user$role() == "SC"))
  )
  rapbase::statsGuideServer("muskelStatsGuide", registryName = "muskel")






}
