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
    registryName = "muskel",
    dbType = "mysql",
    query = "SELECT *
             FROM SkjemaOversikt"
  )
  SMAoversikt <- rapbase::loadRegData(
    registryName = "muskel",
    dbType = "mysql",
    query = "SELECT *
             FROM SMAoversikt"
  )

  rapbase::appLogger(session = session, msg = "Muskel: shiny app starter")
  reshID <- rapbase::getUserReshId(session)
  userRole <- rapbase::getUserRole(session)

  rapbase::navbarWidgetServer("navbar-widget", "NORNMD")

  muskel::fordelingsfig_server("fordeling_id", RegData=RegData, reshID = reshID)

  muskel::fordeling_grvar_server("forgrvar", RegData=RegData, reshID = reshID,
                                 ss = session)

  muskel::kumulativAndel_server("kumAnd", RegData=RegData,
                                reshID = reshID, ss = session)

  muskel::admtab_server("muskeltabell", RegData=RegData,
                        SkjemaOversikt=SkjemaOversikt,
                        SMAoversikt=SMAoversikt, ss = session,
                        userRole=userRole)

  muskel::datadump_server("dataDumpMuskel", userRole=userRole,
                          reshID = reshID, mainSession = session)

# Eksport  #
rapbase::exportUCServer("muskelExport", "muskel")
## veileding
rapbase::exportGuideServer("muskelExportGuide", "muskel")

## Stats
rapbase::statsServer("muskelStats", registryName = "muskel",
                     eligible = (userRole == "SC"))
rapbase::statsGuideServer("muskelStatsGuide", registryName = "muskel")






}
