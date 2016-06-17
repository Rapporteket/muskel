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
  HovedDiagnoserVedInkl <- addmargins(table(as.character(RegData$RegInst_label[RegData$ForlopsType1Num==1]),
                                     as.character(RegData$DiagICD10[RegData$ForlopsType1Num==1]), useNA = 'ifany'))


  ############  UNDER ARBEID  ###################################################
  ##### Nyeste ikke-tomme registrering   #############################################################################
  Nyeste <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
  Nyeste <- Nyeste[!is.na(Nyeste$OppfInst_label) & Nyeste$OppfInst_label != '', ]
  Nyeste <- Nyeste[match(unique(Nyeste$PasientID), Nyeste$PasientID), ]

  OppfolgingVed <- table(Nyeste$OppfInst_label, useNA = 'ifany')
  OppfolgingVed[length(OppfolgingVed)+1] <- length(setdiff(RegData$PasientID, Nyeste$PasientID))
  names(OppfolgingVed)[length(OppfolgingVed)] <- 'Ikke registrert'


  Nyeste <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
  Nyeste <- Nyeste[!is.na(Nyeste$DiagICD10) & Nyeste$DiagICD10 != '', ]
  Nyeste <- Nyeste[match(unique(Nyeste$PasientID), Nyeste$PasientID), ]
  pid_tommeICD10 <- setdiff(RegData$PasientID, Nyeste$PasientID)
  Nyeste <- rbind(Nyeste, RegData[match(pid_tommeICD10, RegData$PasientID),])

  HovedDiagnoserNyest <- addmargins(table(as.character(Nyeste$RegInst_label),
                                          as.character(Nyeste$DiagICD10), useNA = 'ifany'))
  colnames(HovedDiagnoserNyest)[colnames(HovedDiagnoserNyest)==''] <- 'Ikke reg.'



  ## Diagnose stilt
  Nyeste <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
  Nyeste <- Nyeste[!is.na(Nyeste$DiagnoseStiltAvPrim_label) & Nyeste$DiagnoseStiltAvPrim_label != '', ]
  Nyeste <- Nyeste[match(unique(Nyeste$PasientID), Nyeste$PasientID), ]
  pid_tommeDiagnoseStiltAv <- setdiff(RegData$PasientID, Nyeste$PasientID)
  Nyeste <- rbind(Nyeste, RegData[match(pid_tommeICD10, RegData$PasientID),])

  HovedDiagnoserNyest <- addmargins(table(as.character(Nyeste$DiagnoseStiltAvPrim_label),
                                          as.character(Nyeste$Diagnosegr_label), useNA = 'ifany'))
  colnames(HovedDiagnoserNyest)[colnames(HovedDiagnoserNyest)==''] <- 'Ikke reg.'

}


