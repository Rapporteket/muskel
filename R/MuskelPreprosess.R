#' Preprossesser data for Muskelregisterets rapporter
#'
#' Denne rensker og definerer opp nødvendige variabler
#'
#' Her kan detaljer skrives
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return Et preprossessert datasett
#'
#' @export
#'
MuskelPreprosess <- function(RegData)
{
  RegData <- RegData[which(RegData$BasisRegStatus==1), ]

  RegData$HovedDato <- as.Date(RegData$HovedDato, format="%Y-%m-%d") # Ordne datoformat
  RegData$AvdodDato <- as.Date(RegData$AvdodDato, format="%Y-%m-%d")
  RegData$Foedselsdato <- as.Date(RegData$Foedselsdato, format="%Y-%m-%d")
  RegData$Alder <- age(RegData$Foedselsdato, Sys.Date())
  RegData$AlderVreg <- age(RegData$Foedselsdato, RegData$HovedDato)

  RegData$Diagnosegr <- 99
  RegData$Diagnosegr[RegData$DiagICD10 %in%
                       c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G71.8',
                         'G71.9', 'G72.3', 'G73.6')] <- 1
  RegData$Diagnosegr[substr(RegData$DiagICD10, 1, 3) == 'G12'] <- 2
  RegData$Diagnosegr[substr(RegData$DiagICD10, 1, 3) == 'G60'] <- 3

  RegData$Diagnosegr_label <- factor(
    RegData$Diagnosegr, levels = c(1:3, 99),
    labels = c('Muskelsykdommer', 'Spinal muskelatrofi',
               'Polynevropati', 'Annet/Ikke reg.'))
  RegData$Utdanning_label <- factor(
    RegData$Utdanning,
    levels = muskel::klokebok$listeverdier[
      muskel::klokebok$fysisk_feltnavn == "EDUCATION" &
        muskel::klokebok$skjemanavn == "Registreringsskjema"],
    labels = muskel::klokebok$listetekst[
      muskel::klokebok$fysisk_feltnavn == "EDUCATION" &
        muskel::klokebok$skjemanavn == "Registreringsskjema"]
  )
  RegData$Sivilstatus_label <- factor(
    RegData$Sivilstatus,
    levels = muskel::klokebok$listeverdier[
      muskel::klokebok$fysisk_feltnavn == "MARITAL_STATUS" &
        muskel::klokebok$skjemanavn == "Registreringsskjema"],
    labels = muskel::klokebok$listetekst[
      muskel::klokebok$fysisk_feltnavn == "MARITAL_STATUS" &
        muskel::klokebok$skjemanavn == "Registreringsskjema"]
  )
  RegData$Arvegang_label <- factor(
    RegData$Arvegang,
    levels = muskel::klokebok$listeverdier[
      muskel::klokebok$fysisk_feltnavn == "INHERITANCE_PATTERN" &
        muskel::klokebok$skjemanavn == "Registreringsskjema"],
    labels = muskel::klokebok$listetekst[
      muskel::klokebok$fysisk_feltnavn == "INHERITANCE_PATTERN" &
        muskel::klokebok$skjemanavn == "Registreringsskjema"]
  )
  RegData$Gangfunksjon_label <- factor(
    RegData$Gangfunksjon,
    levels = muskel::klokebok$listeverdier[
      muskel::klokebok$fysisk_feltnavn == "WALKING" &
        muskel::klokebok$skjemanavn == "Registreringsskjema"],
    labels = muskel::klokebok$listetekst[
      muskel::klokebok$fysisk_feltnavn == "WALKING" &
        muskel::klokebok$skjemanavn == "Registreringsskjema"]
  )

  RegData$Undergruppe_label <- factor(
    RegData$Undergruppe,
    levels = muskel::map_undergruppe$kode,
    labels = muskel::map_undergruppe$label
  )
  RegData$Undergruppe2_label <- factor(
    RegData$Undergruppe2,
    levels = muskel::map_undergruppe2$kode,
    labels = muskel::map_undergruppe2$label
  )

  RegData$Debut <- as.numeric(format(RegData$Foedselsdato, '%Y')) +
    RegData$DebutAlder
  RegData$DiagnoseAlder <- RegData$DiagnoseAar -
    as.numeric(format(RegData$Foedselsdato, '%Y'))
  RegData$TidDebDiag <- RegData$DiagnoseAar - RegData$Debut
  RegData$TidDebUtred <- RegData$Utredningsstart - RegData$Debut
  RegData$TidUtredDiag <- RegData$DiagnoseAar - RegData$Utredningsstart

  RegData$Aar <- as.numeric(format(RegData$HovedDato, '%Y'))


  return(invisible(RegData))
}
