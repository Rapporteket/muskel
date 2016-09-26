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

MuskelUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, diagnoseSatt, diagnosegr, forlop, fargepalett='BlaaRapp')
{
  # Definerer intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]
  indVarMed <- 1:Ninn
  indAld <- which(RegData$AlderVreg >= minald & RegData$AlderVreg <= maxald)
  indDato <- which(RegData$HovedDato >= as.POSIXlt(datoFra) & RegData$HovedDato <= as.POSIXlt(datoTil))
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  indDiagSatt <- if (diagnoseSatt %in% c(1:13, 99)) {which(RegData$DiagnoseStiltAvPrim == diagnoseSatt)} else {indDiagSatt <- 1:Ninn}
  indDiagnosegr <- if (diagnosegr %in% c(1:3)) {which(RegData$Diagnosegr == diagnosegr)} else {indDiagnosegr <- 1:Ninn}
  indForlop <- if (forlop %in% c(1:3)) {which(RegData$ForlopsType1Num == forlop)} else {indForlop <- 1:Ninn}

  indMed <- indVarMed %i% indAld %i% indDato %i% indKj %i% indDiagSatt %i% indDiagnosegr %i% indForlop
  RegData <- RegData[indMed,]

  if (dim(RegData)[1] > 0){
  utvalgTxt <- c(paste('Registrert: ',
                       min(RegData$HovedDato, na.rm=T), ' til ', max(RegData$HovedDato, na.rm=T), sep='' ),
                 if ((minald>0) | (maxald<120)) {
                   paste('Pasienter fra ', min(RegData$AlderVreg, na.rm=T), ' til ', max(RegData$AlderVreg, na.rm=T), ' år', sep='')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (diagnoseSatt %in% c(1:13, 99)){paste0('Først diagnostisert: ', RegData$DiagnoseStiltAvPrim_label
                                                           [match(diagnoseSatt, RegData$DiagnoseStiltAvPrim)])},
                 if (diagnosegr %in% c(1:3)) {paste0('Diagnosegruppe: ', c('Muskelsykdommer', 'Spinal muskelatrofi',
                                                                                'Polynevropati')[diagnosegr])},
                 if (forlop %in% c(1:3)) {paste0('Forl\370pstype: ', RegData$ForlopsType1[match(forlop, RegData$ForlopsType1Num)])}
  )
  } else {
    utvalgTxt <- paste0('Dato: ', datoFra, ' til ', datoTil)
  }


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}
