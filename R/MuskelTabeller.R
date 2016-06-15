#' Tabeller for Muskelregisteret
#'
#' Denne funksjonen lager et utvalg av tabeller for muskelregisteret
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return Tabeller til bruk i samlerapport eller annet
#'
#' @export
#'
MuskelTabeller <- function(RegData, datoFra, datoTil, minald, maxald, erMann, enhetsUtvalg)

{
  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- MuskelHentRegData()
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- MuskelPreprosess(RegData=RegData)
  }

  # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}

  # Sykehustekst avhengig av bruker og brukervalg
  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  }


  HovedDiagnoser <- table(RegData$RegInst_label, RegData$DiagICD10)











}


