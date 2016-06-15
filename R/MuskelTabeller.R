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

  RegData$UtfyltAar <- as.POSIXlt(RegData$Utfyllingsdato, format="%Y-%m-%d")$year + 1900


  ##### Kun Basisregistrering #############################################################################
  BasisregPrAar <- addmargins(table(as.character(RegData$RegInst_label[RegData$ForlopsType1Num==1]),
                                    RegData$UtfyltAar[RegData$ForlopsType1Num==1], useNA = 'ifany'))
  HovedDiagnoser <- addmargins(table(as.character(RegData$RegInst_label[RegData$ForlopsType1Num==1]),
                                     as.character(RegData$DiagICD10[RegData$ForlopsType1Num==1]), useNA = 'ifany'))


  ############  UNDER ARBEID  ###################################################
  ##### Nyeste registrering   #############################################################################
  Nyeste <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
  Nyeste <- Nyeste[match(unique(Nyeste$PasientID), Nyeste$PasientID), ]

  Oppfolges <- table(Nyeste$OppfInst_label)

  Oppfolges2 <- table(RegData$OppfInst_label)




}


