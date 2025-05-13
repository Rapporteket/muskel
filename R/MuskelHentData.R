#' Provide global dataframe for Muskel
#'
#' Provides muskel data from staging
#'
#'
#'
#' @return RegData data frame
#' @export

MuskelHentRegData <- function() {

  registryName <- "data"
  dbType <- "mysql"

  query <- paste0("SELECT
                  allevarnum.ForlopsID,
                  allevarnum.Foedselsdato,
                  allevarnum.DiagICD10,
                  allevarnum.DebutAlder,
                  allevarnum.DiagnoseAar,
                  allevarnum.Utredningsstart,
                  allevarnum.Utfyllingsdato,
                  allevarnum.DiagnoseStiltAv,
                  allevarnum.Undergruppe,
                  allevarnum.Undergruppe2,
                  allevarnum.GenetiskAarsakPaavist,
                  allevarnum.DiagEndret,
                  allevarnum.FoelgesOppAvIns,
                  allevarnum.HjerteAffAlder,
                  allevarnum.Fysioterapi,
                  allevarnum.Ergoterapi,
                  allevarnum.UndergruppeSpes,
                  allevarnum.Undergruppe2Spes,
                  allevarnum.HjerteAff,
                  allevarnum.HjerteAffAnnetSpes,
                  allevarnum.DiagAnamese,
                  allevarnum.DiagEMG,
                  allevarnum.DiagCK,
                  allevarnum.DiagDNA,
                  allevarnum.DiagBiopsi,
                  allevarnum.Gangfunksjon,
                  allevarnum.AlderTapGang,
                  allevarnum.RespStotte,
                  allevarnum.AlderRespStotte,
                  allevarnum.TrygdFraAlder,
                  allevarnum.Uforetrygd,
                  allevarnum.FysioManglerAarsak,
                  allevarnum.KognitivSvikt,
                  allevarnum.Utdanning,
                  allevarnum.Sivilstatus,
                  allevarnum.Delesjon,
                  allevarnum.PunktMutasjon,
                  allevarnum.Duplikasjon,
                  allevarnum.Arvegang,
                  allevarnum.Steroider,
                  allevarnum.Smertestillende,
                  allevarnum.Kardiomyopati,
                  allevarnum.Hjertearytmi,
                  allevarnum.HjerteAffAnnet,
                  allevarnum.Hjerteoppfoelging,
                  allevarnum.Arbeid,
                  allevarnum.SympFamilie,
                  allevarnum.Antiarytmika,
                  allevarnum.ACEHemmer,
                  allevarnum.AnnetHjerteMed,
                  allevarnum.MedikBehandling,
                  allevarnum.AnnenMedikBeh,
                  allevarnum.OppfolgBarnelegeNevrolog,
                  allevarnum.PsykiskHelsetjeneste,
                  allevarnum.OppholdRehab,
                  allevarnum.TilbudKostveiledning,
                  allevarnum.TilbudGenetiskVeiledning,
                  allevarnum.AnsvarsgruppeIP,
                  allevarnum.BPA,
                  forlopsoversikt.AvdRESH,
                  forlopsoversikt.HovedDato,
                  forlopsoversikt.SykehusNavn,
                  forlopsoversikt.erMann,
                  forlopsoversikt.BasisRegStatus,
                  forlopsoversikt.PasientAlder,
                  forlopsoversikt.PasientID,
                  forlopsoversikt.ForlopsType1Num,
                  forlopsoversikt.ForlopsType1,
                  forlopsoversikt.Fylke,
                  forlopsoversikt.Fylkenr,
                  forlopsoversikt.Avdod,
                  forlopsoversikt.AvdodDato
                  FROM allevarnum INNER JOIN forlopsoversikt
                  ON allevarnum.ForlopsID = forlopsoversikt.ForlopsID")

  RegData <- rapbase::loadRegData(registryName, query, dbType)

  return(RegData)
}
