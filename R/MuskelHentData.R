#' Provide global dataframe for NoRGast
#'
#' Provides NoRGast data from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export

MuskelHentRegData <- function() {

  registryName <- "Muskel"
  dbType <- "mysql"

  query <- paste0("SELECT AlleVarNum.DiagICD10,
                    AlleVarNum.ForlopsID,
                    AlleVarNum.FodselsDato,
                    AlleVarNum.DiagICD10,
                    AlleVarNum.DebutAlder,
                    AlleVarNum.DiagnoseAar,
                    AlleVarNum.Utredningsstart,
                    AlleVarNum.Utfyllingsdato,
                    AlleVarNum.DiagnoseStiltAvPrim,

                    ForlopsOversikt.AvdRESH,
                    ForlopsOversikt.HovedDato,
                    ForlopsOversikt.SykehusNavn,
                    ForlopsOversikt.ErMann,
                    ForlopsOversikt.BasisRegStatus,
                    ForlopsOversikt.PasientAlder,
                    ForlopsOversikt.PasientID,
                    ForlopsOversikt.ForlopsType1Num,
                    ForlopsOversikt.ForlopsType1,
                    AlleVar.DiagnoseStiltAvPrim AS DiagnoseStiltAvPrim_label
                    FROM AlleVarNum INNER JOIN ForlopsOversikt
                    ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
                    INNER JOIN AlleVar ON AlleVarNum.ForlopsID = AlleVar.ForlopsID")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
