#' Gjør utvalg av dataene
#'
#' Denne funksjonen gjør utvalg av dataene og returnerer det filtrerte datasettet, utvalgsteksten
#' og fargepaletten for bruk i figuren
#'
#' @inheritParams MuskelFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet, utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

MuskelUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, diagnoseSatt = 99, diagnosegr='', forlop,
                         diagnose='', undergr='', undergr2='', fargepalett='BlaaRapp')
{
  # Definerer intersect-operator
  "%i%" <- intersect

  # diagnosegr <- as.numeric(diagnosegr)
  # mapping <- data.frame(AvdRESH = unique(RegData$AvdRESH), SykehusNavn = RegData$SykehusNavn[match(unique(RegData$AvdRESH), RegData$AvdRESH)])

  Ninn <- dim(RegData)[1]
  indVarMed <- 1:Ninn
  indAld <- which(RegData$AlderVreg >= minald & RegData$AlderVreg <= maxald) # Filtrerer på alder ved registrering
  indDato <- which(RegData$HovedDato >= as.POSIXlt(datoFra) & RegData$HovedDato <= as.POSIXlt(datoTil))
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  indDiagSatt <- if (diagnoseSatt != 99) {which(RegData$DiagnoseStiltAv == diagnoseSatt)} else {indDiagSatt <- 1:Ninn}
  indDiagnosegr <- if (diagnosegr[1] != '') {which(RegData$Diagnosegr %in% as.numeric(diagnosegr))} else {indDiagnosegr <- 1:Ninn}
  indDiagnose <- if (diagnose[1] != '') {which(RegData$DiagICD10 %in% diagnose)} else {indDiagnose <- 1:Ninn}
  indUndergr <- if (undergr[1] != '') {which(RegData$Undergruppe %in% as.numeric(undergr))} else {indUndergr <- 1:Ninn}
  indUndergr2 <- if (undergr2[1] != '') {which(RegData$Undergruppe2 %in% as.numeric(undergr2))} else {indUndergr2 <- 1:Ninn}
  indForlop <- if (forlop %in% c(1:3)) {which(RegData$ForlopsType1Num == forlop)} else {indForlop <- 1:Ninn}

  indMed <- indVarMed %i% indAld %i% indDato %i% indKj %i% indDiagSatt %i% indDiagnosegr %i% indForlop %i% indUndergr %i% indUndergr2 %i% indDiagnose
  RegData <- RegData[indMed,]

  if (dim(RegData)[1] > 0){
  utvalgTxt <- c(paste('Registrert: ',
                       min(RegData$HovedDato, na.rm=T), ' til ', max(RegData$HovedDato, na.rm=T), sep='' ),
                 if ((minald>0) | (maxald<120)) {
                   paste('Pasienter fra ', min(RegData$AlderVreg, na.rm=T), ' til ', max(RegData$AlderVreg, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (diagnoseSatt != 99){paste0('Først diagnostisert: ', RegData$SykehusNavn[match(diagnoseSatt, RegData$AvdRESH)])},
                 if (diagnosegr %in% c(1:3)) {paste0('Diagnosegruppe: ', paste(c('Muskelsykdommer', 'Spinal muskelatrofi',
                                                                                'Polynevropati')[diagnosegr], collapse = ', '))},
                 if (undergr[1] != ''){paste0('Diagnose(r): ', paste(diagnose, collapse=', '))},
                 if (undergr[1] != ''){paste0('Undergruppe(r): ', paste(RegData$Undergruppe_label[match(as.numeric(undergr),
                                                                                      RegData$Undergruppe)], collapse=', '))},
                 if (undergr2[1] != ''){paste0('Undergruppe(r) nivå 2: ', paste(RegData$Undergruppe2_label[match(as.numeric(undergr2),
                                                                                                        RegData$Undergruppe2)], collapse=', '))},
                 if (forlop %in% c(1:3)) {paste0('Forl\370pstype: ', RegData$ForlopsType1[match(forlop, RegData$ForlopsType1Num)])}
  )
  } else {
    utvalgTxt <- paste0('Dato: ', datoFra, ' til ', datoTil)
  }


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}
