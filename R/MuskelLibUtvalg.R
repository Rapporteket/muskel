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

MuskelUtvalg <- function(RegData, datoFra, datoTil, minald, maxald, erMann, egenavd = 0, enhetsUtvalg, diagnosegr='', forlop,
                         diagnose='', undergr='', undergr2='', avdod='', fargepalett='BlaaRapp', reshID, UtredningsaarFra=1900,
                         UtredningsaarTil=2100)
{
  # Definerer intersect-operator
  "%i%" <- intersect
  fylke<-''

  mapping_avdresh_shusnavn <- data.frame(AvdRESH = unique(RegData$AvdRESH), SykehusNavn = RegData$SykehusNavn[match(unique(RegData$AvdRESH), RegData$AvdRESH)])
  map_shus_fylke <- aggregate(RegData$Fylke, by=list(avdresh=RegData$FoelgesOppAvIns), function(x){names(table(x))[which(table(x)==max(table(x)))[1]]})

  if (egenavd==0) {
    shtxt <- paste0('Registrert ved: ', as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)]))
  }

  if (egenavd==1) { # Oppfølgende institusjon HF
    RegData$AvdRESH <- RegData$FoelgesOppAvIns
    RegData <- RegData[!is.na(RegData$AvdRESH), ]
    RegData$SykehusNavn <- mapping_avdresh_shusnavn$SykehusNavn[match(RegData$AvdRESH, mapping_avdresh_shusnavn$AvdRESH)]
    shtxt <- paste0('Følges opp: ', as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)]))
  }
  if (egenavd==2) { # Diagnostiserende institusjon HF
    RegData$AvdRESH <- RegData$DiagnoseStiltAv
    RegData <- RegData[!is.na(RegData$AvdRESH), ]
    RegData$SykehusNavn <- mapping_avdresh_shusnavn$SykehusNavn[match(RegData$AvdRESH, mapping_avdresh_shusnavn$AvdRESH)]
    shtxt <- paste0('Diagnostisert: ', as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)]))
  }
  if (egenavd==3) { # Bosatt i samme fylke som aktuelt HF
    fylke <- map_shus_fylke$x[match(reshID, map_shus_fylke$avdresh)]
    # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
    if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$Fylke == fylke), ]}
    # Sykehustekst avhengig av bruker og brukervalg
    if (enhetsUtvalg==0) {
      shtxt <- 'Hele landet'
    } else {
      shtxt <- paste0('Bosatt: ', fylke)
    }
  }

  if (egenavd %in% 0:2) {
    # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
    if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}

    # Sykehustekst avhengig av bruker og brukervalg
    if (enhetsUtvalg==0) {
      shtxt <- 'Hele landet'
    }
  }

  Ninn <- dim(RegData)[1]
  indVarMed <- 1:Ninn
  # indAld <- which(RegData$AlderVreg >= minald & RegData$AlderVreg <= maxald) # Filtrerer på alder ved registrering
  indAld <- which(RegData$Alder >= minald & RegData$Alder <= maxald)
  indUtredningAar <- if (UtredningsaarFra != 1900 | UtredningsaarTil != 2100) {
    which(RegData$Utredningsstart >= UtredningsaarFra & RegData$Utredningsstart <= UtredningsaarTil)} else {indUtredningAar <- 1:Ninn}
  indDato <- which(RegData$HovedDato >= as.POSIXlt(datoFra) & RegData$HovedDato <= as.POSIXlt(datoTil))
  indAvdod <- if (avdod %in% c('Ja', 'Nei')) {which(RegData$Avdod == avdod)} else {indAvdod <- 1:Ninn}
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  # indDiagSatt <- if (diagnoseSatt != 99) {which(RegData$DiagnoseStiltAv == diagnoseSatt)} else {indDiagSatt <- 1:Ninn}
  indDiagnosegr <- if (diagnosegr[1] != '') {which(RegData$Diagnosegr %in% as.numeric(diagnosegr))} else {indDiagnosegr <- 1:Ninn}
  indDiagnose <- if (diagnose[1] != '') {which(RegData$DiagICD10 %in% diagnose)} else {indDiagnose <- 1:Ninn}
  indUndergr <- if (undergr[1] != '') {which(RegData$Undergruppe %in% as.numeric(undergr))} else {indUndergr <- 1:Ninn}
  indUndergr2 <- if (undergr2[1] != '') {which(RegData$Undergruppe2 %in% as.numeric(undergr2))} else {indUndergr2 <- 1:Ninn}
  indForlop <- if (forlop %in% c(1:3)) {which(RegData$ForlopsType1Num == forlop)} else {indForlop <- 1:Ninn}

  indMed <- indVarMed %i% indAld %i% indDato %i% indKj %i% indDiagnosegr %i% indForlop %i% indUndergr %i% indUndergr2 %i% indDiagnose %i% indAvdod %i% indUtredningAar
  RegData <- RegData[indMed,]

  if (dim(RegData)[1] > 0){
  utvalgTxt <- c(paste('Registrert: ',
                       min(RegData$HovedDato, na.rm=T), ' til ', max(RegData$HovedDato, na.rm=T), sep='' ),
                 # if ((minald>0) | (maxald<120)) {
                 #   paste('Pasienter fra ', min(RegData$AlderVreg, na.rm=T), ' til ', max(RegData$AlderVreg, na.rm=T), ' år', sep='')},
                 if ((minald>0) | (maxald<120)) {
                   paste0('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år')},
                 if (UtredningsaarFra != 1900 | UtredningsaarTil != 2100){
                   paste0('Utredningstart fra år ', min(RegData$Utredningsstart, na.rm=T), ' til ', max(RegData$Utredningsstart, na.rm=T))},
                 if (avdod %in% c('Ja', 'Nei')) {paste0('Inkluder avdøde: ', avdod)}, # Ikke korrekt for valg "Ja"
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 # if (diagnoseSatt != 99){paste0('Først diagnostisert: ', RegData$SykehusNavn[match(diagnoseSatt, RegData$AvdRESH)])},
                 if (diagnosegr[1] != '') {paste0('Diagnosegruppe(r): ', paste(sort(unique(RegData$Diagnosegr_label)), collapse = ', '))},
                 if (diagnose[1] != ''){paste0('Diagnose(r): ', paste(sort(unique(RegData$DiagICD10)), collapse=', '))},
                 if (undergr[1] != ''){paste0('Undergruppe(r): ', paste(na.omit(RegData$Undergruppe_label[match(undergr, RegData$Undergruppe)]),
                                                                        collapse=', '))},
                 if (undergr2[1] != ''){paste0('Undergruppe(r) nivå 2: ', paste(na.omit(RegData$Undergruppe2_label[match(undergr2, RegData$Undergruppe2)]),
                                                                                collapse=', '))},
                 if (forlop %in% c(1:3)) {paste0('Forl\370pstype: ', RegData$ForlopsType1[match(forlop, RegData$ForlopsType1Num)])}
  )
  } else {
    utvalgTxt <- paste0('Dato: ', datoFra, ' til ', datoTil)
  }

  if (egenavd ==3) {
    ind <- list(Hoved=which(RegData$Fylke == fylke), Rest=which(RegData$Fylke != fylke))
  } else {
    ind <- list(Hoved=which(RegData$AvdRESH == reshID), Rest=which(RegData$AvdRESH != reshID))
  }


  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett, shtxt=shtxt, ind=ind, fylke=fylke) #GronnHNpms624,
  return(invisible(UtData))
}
