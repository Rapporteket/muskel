#' Provide global dataframe for NoRGast
#'
#' Provides NoRGast data from staging
#'
#' @inheritParams FigAndeler
#'
#' @return RegData data frame
#' @export

MuskelHentRegData <- function() {

  registryName <- "muskel"
  dbType <- "mysql"

  query <- paste0("SELECT
                  AlleVarNum.ForlopsID,
                  AlleVarNum.Foedselsdato,
                  AlleVarNum.DiagICD10,
                  AlleVarNum.DebutAlder,
                  AlleVarNum.DiagnoseAar,
                  AlleVarNum.Utredningsstart,
                  AlleVarNum.Utfyllingsdato,
                  AlleVarNum.DiagnoseStiltAv,
                  AlleVarNum.Undergruppe,
                  AlleVarNum.Undergruppe2,
                  AlleVarNum.GenetiskAarsakPaavist,
                  AlleVarNum.DiagEndret,
                  AlleVarNum.FoelgesOppAvIns,
                  AlleVarNum.HjerteAffAlder,
                  AlleVarNum.Fysioterapi,
                  AlleVarNum.Ergoterapi,
                  AlleVarNum.UndergruppeSpes,
                  AlleVarNum.Undergruppe2Spes,
                  AlleVarNum.HjerteAff,
                  AlleVarNum.HjerteAffAnnetSpes,
                  AlleVarNum.DiagAnamese,
                  AlleVarNum.DiagEMG,
                  AlleVarNum.DiagCK,
                  AlleVarNum.DiagDNA,
                  AlleVarNum.DiagBiopsi,
                  AlleVarNum.Gangfunksjon,
                  AlleVarNum.AlderTapGang,
                  AlleVarNum.RespStotte,
                  AlleVarNum.AlderRespStotte,
                  AlleVarNum.TrygdFraAlder,
                  AlleVarNum.Uforetrygd,
                  AlleVarNum.FysioManglerAarsak,
                  AlleVarNum.KognitivSvikt,
                  AlleVarNum.Utdanning,
                  AlleVarNum.Sivilstatus,
                  AlleVarNum.Delesjon,
                  AlleVarNum.PunktMutasjon,
                  AlleVarNum.Duplikasjon,
                  AlleVarNum.Arvegang,
                  AlleVarNum.Steroider,
                  AlleVarNum.PsykiskHelsetjeneste,
                  AlleVarNum.Smertestillende,
                  AlleVarNum.Kardiomyopati,
                  AlleVarNum.Hjertearytmi,
                  AlleVarNum.HjerteAffAnnet,
                  AlleVarNum.Hjerteoppfoelging,
                  AlleVarNum.Arbeid,
                  AlleVarNum.SympFamilie,
                  AlleVar.DiagnoseStiltAv AS DiagnoseStiltAv_label,
                  AlleVar.Undergruppe AS Undergruppe_label,
                  AlleVar.Undergruppe2 AS Undergruppe2_label,
                  AlleVar.FoelgesOppAvIns AS FoelgesOppAvIns_label,
                  AlleVar.Utdanning AS Utdanning_label,
                  AlleVar.Sivilstatus AS Sivilstatus_label,
                  AlleVar.Arvegang AS Arvegang_label,
                  AlleVar.Gangfunksjon AS Gangfunksjon_label,
                  ForlopsOversikt.AvdRESH,
                  ForlopsOversikt.HovedDato,
                  ForlopsOversikt.SykehusNavn,
                  ForlopsOversikt.erMann,
                  ForlopsOversikt.BasisRegStatus,
                  ForlopsOversikt.PasientAlder,
                  ForlopsOversikt.PasientID,
                  ForlopsOversikt.ForlopsType1Num,
                  ForlopsOversikt.ForlopsType1,
                  ForlopsOversikt.Fylke,
                  ForlopsOversikt.Fylkenr,
                  ForlopsOversikt.Avdod,
                  ForlopsOversikt.AvdodDato
                  FROM AlleVarNum INNER JOIN ForlopsOversikt
                  ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID
                  INNER JOIN AlleVar ON AlleVarNum.ForlopsID = AlleVar.ForlopsID")

  RegData <- rapbase::LoadRegData(registryName, query, dbType)

  return(RegData)
}
