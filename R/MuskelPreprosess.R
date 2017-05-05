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

  # datoVar <- 'HovedDato'
  RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d") # Ordne datoformat
  RegData$AvdodDato <- as.character(as.POSIXlt(RegData$AvdodDato, format="%Y-%m-%d"))
  RegData$RegInst_label <- as.character(RegData$RegInst_label)  # Kombinere RegInst og RegInstAndre
  RegData$RegInstAndre_label <- as.character(RegData$RegInstAndre_label)
  RegData$RegInst_label[RegData$RegInst==99] <- RegData$RegInstAndre_label[RegData$RegInst==99]
  RegData$RegInst[RegData$RegInst==99 & RegData$RegInstAndre!=99] <-
    RegData$RegInstAndre[RegData$RegInst==99 & RegData$RegInstAndre!=99]+13
  RegData$OppfInst_label <- as.character(RegData$OppfInst_label)  # Kombinere OppfInst og OppfInstAndre
  RegData$OppfInstAndre_label <- as.character(RegData$OppfInstAndre_label)
  RegData$OppfInst_label[which(RegData$OppfInst==99)] <- RegData$OppfInstAndre_label[which(RegData$OppfInst==99)]
  RegData$OppfInst[which(RegData$OppfInst==99 & RegData$OppfInstAndre!=99)] <-
    RegData$OppfInstAndre[which(RegData$OppfInst==99 & RegData$OppfInstAndre!=99)]+13
  RegData$DiagnoseStiltAvPrim_label <- as.character(RegData$DiagnoseStiltAvPrim_label)  # Kombinere DiagnoseStiltAvPrim og DiagnoseStiltAvSek
  RegData$DiagnoseStiltAvSek_label <- as.character(RegData$DiagnoseStiltAvSek_label)
  RegData$DiagnoseStiltAvPrim_label[which(RegData$DiagnoseStiltAvPrim==99)] <- RegData$DiagnoseStiltAvSek_label[which(RegData$DiagnoseStiltAvPrim==99)]
  RegData$DiagnoseStiltAvPrim[which(RegData$DiagnoseStiltAvPrim==99 & RegData$DiagnoseStiltAvSek!=99)] <-
    RegData$DiagnoseStiltAvSek[which(RegData$DiagnoseStiltAvPrim==99 & RegData$DiagnoseStiltAvSek!=99)]+13


  RegData$Alder <- round(as.numeric(difftime(Sys.Date(),
                                             strptime(RegData$FodselsDato, format="%Y-%m-%d" ))/365.25),0)
  RegData$AlderVreg <- round(as.numeric(difftime(RegData$HovedDato,
                                             strptime(RegData$FodselsDato, format="%Y-%m-%d" ))/365.25),0)

  RegData$Diagnosegr <- 99
  RegData$Diagnosegr[RegData$DiagICD10 %in%
                       c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G71.8', 'G71.9', 'G72.3', 'G73.6')] <- 1
  RegData$Diagnosegr[substr(RegData$DiagICD10, 1, 3) == 'G12'] <- 2
  RegData$Diagnosegr[substr(RegData$DiagICD10, 1, 3) == 'G60'] <- 3

  RegData$Diagnosegr_label <- factor(RegData$Diagnosegr, levels = c(1:3, 99),
                               labels = c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati', 'Ikke reg.'))
  # RegData$DiagnoserHyppige <- 99
  # RegData$DiagnoserHyppige[RegData$Undergruppe == 1] <- 1 #Duchenne
  # RegData$DiagnoserHyppige[RegData$Undergruppe == 2] <- 2 #Becker
  # RegData$DiagnoserHyppige[RegData$Undergruppe == 4] <- 3 #Limb-girdle (LGMD)
  # RegData$DiagnoserHyppige[RegData$Undergruppe == 20] <- 4 #DM 1
  # RegData$DiagnoserHyppige[RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13] <- 5 #LGMD 2I (FKRP-mutasjon)

  RegData$Debut <- as.POSIXlt(RegData$FodselsDato, format="%Y-%m-%d")$year+1900 + RegData$DebutAlder
  RegData$DiagnoseAlder <- RegData$DiagnoseAar - (as.POSIXlt(RegData$FodselsDato, format="%Y-%m-%d")$year+1900)
  RegData$TidDebDiag <- RegData$DiagnoseAar - RegData$Debut
  RegData$TidDebUtred <- RegData$Utredningsstart - RegData$Debut
  RegData$TidUtredDiag <- RegData$DiagnoseAar - RegData$Utredningsstart

  RegData$Aar <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d")$year+1900

  RegData$NyRESH <- RegData$ORG_RESH
  RegData$NyRESH[RegData$RegInst %in% c(14,15)] <- 100065 # Helgelandssykehuset HF
  RegData$NyRESH[RegData$RegInst %in% c(2)] <- 100082 # Helse Bergen HF
  RegData$NyRESH[RegData$RegInst %in% c(7)] <- 100083 # Helse Stavanger HF
  RegData$NyRESH[RegData$RegInst %in% c(17)] <- 100084 # Helse Fonna HF
  RegData$NyRESH[RegData$RegInst %in% c(18)] <- 100085 # Helse Førde HF
  RegData$NyRESH[RegData$RegInst %in% c(1)] <- 100089 # Akershus universitetssykehus HF
  RegData$NyRESH[RegData$RegInst %in% c(8,26)] <- 100091 # Sykehuset Innlandet HF
  RegData$NyRESH[RegData$RegInst %in% c(11)] <- 100092 # Sykehuset Østfold HF
  RegData$NyRESH[RegData$RegInst %in% c(24)] <- 100093 # SUNNAAS SYKEHUS HF
  RegData$NyRESH[RegData$RegInst %in% c(10)] <- 100100 # Sykehuset i Vestfold HF
  RegData$NyRESH[RegData$RegInst %in% c(9)] <- 100132 # Sykehuset Telemark HF
  RegData$NyRESH[RegData$RegInst %in% c(12,25)] <- 100133 # Sørlandet Sykehus HF
  RegData$NyRESH[RegData$RegInst %in% c(21,22)] <- 100317 # Helse Nord-Trøndelag HF
  RegData$NyRESH[RegData$RegInst %in% c(6)] <- 100320 # St Olavs Hospital HF
  RegData$NyRESH[RegData$RegInst %in% c(3,23)] <- 101051 # Nordlandssykehuset HF
  RegData$NyRESH[RegData$RegInst %in% c(13,27)] <- 101719 # Universitetssykehuset Nord-Norge HF
  RegData$NyRESH[RegData$RegInst %in% c(16)] <- 101971 # Finnmarkssykehuset HF
  RegData$NyRESH[RegData$RegInst %in% c(28,29)] <- 700272 # Vestre Viken HF
  RegData$NyRESH[RegData$RegInst %in% c(4,5)] <- 4001031 # Oslo universitetssykehus HF
  RegData$NyRESH[RegData$RegInst %in% c(19,20)] <- 4201115 # Helse Møre og Romsdal HF
  RegData$NyRESH[RegData$RegInst %in% c(99)] <- 9999999 #

  aux <- RegData[which(RegData$ForlopsType1Num == 1), c("PasientID", "NyRESH")]
  RegData <- merge(RegData, aux, by.x = 'PasientID', by.y = 'PasientID', suffixes = c('_opprinnelig', ''))

  orgresh <- c(100065, 100082, 100083, 100084, 100085, 100089, 100091, 100092, 100093, 100100,
               100132, 100133, 100317, 100320, 101051, 101719, 101971, 700272, 4001031, 4201115, 9999999)
  shusnavn <- c('Helgelandssykehuset', 'Helse Bergen', 'Helse Stavanger', 'Helse Fonna', 'Helse Førde', 'Ahus',
                'Sykehuset Innlandet', 'Sykehuset Østfold', 'Sunnaas sykehus', 'Sykehuset i Vestfold', 'Sykehuset Telemark',
                'Sørlandet Sykehus', 'Helse Nord-Trøndelag', 'St Olavs Hospital', 'Nordlandssykehuset', 'UNN',
                'Finnmarkssykehuset', 'Vestre Viken', 'OUS', 'Helse Møre og Romsdal', 'Andre')
  RegData$HFreg <- factor(RegData$NyRESH, levels = orgresh, labels = shusnavn)

  RegData$HFdiag <- RegData$ORG_RESH
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(14,15)] <- 100065
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(2)] <- 100082
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(7)] <- 100083
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(17)] <- 100084
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(18)] <- 100085
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(1)] <- 100089
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(8,26)] <- 100091
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(11)] <- 100092
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(24)] <- 100093
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(10)] <- 100100
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(9)] <- 100132
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(12,25)] <- 100133
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(21,22)] <- 100317
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(6)] <- 100320
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(3,23)] <- 101051
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(13,27)] <- 101719
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(16)] <- 101971
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(28,29)] <- 700272
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(4,5)] <- 4001031
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(19,20)] <- 4201115
  RegData$HFdiag[RegData$DiagnoseStiltAvPrim %in% c(99)] <- 9999999

  RegData$HFdiag <- factor(RegData$HFdiag, levels = orgresh, labels = shusnavn)

  RegData$HFoppf <- RegData$ORG_RESH
  RegData$HFoppf[RegData$OppfInst %in% c(14,15)] <- 100065
  RegData$HFoppf[RegData$OppfInst %in% c(2)] <- 100082
  RegData$HFoppf[RegData$OppfInst %in% c(7)] <- 100083
  RegData$HFoppf[RegData$OppfInst %in% c(17)] <- 100084
  RegData$HFoppf[RegData$OppfInst %in% c(18)] <- 100085
  RegData$HFoppf[RegData$OppfInst %in% c(1)] <- 100089
  RegData$HFoppf[RegData$OppfInst %in% c(8,26)] <- 100091
  RegData$HFoppf[RegData$OppfInst %in% c(11)] <- 100092
  RegData$HFoppf[RegData$OppfInst %in% c(24)] <- 100093
  RegData$HFoppf[RegData$OppfInst %in% c(10)] <- 100100
  RegData$HFoppf[RegData$OppfInst %in% c(9)] <- 100132
  RegData$HFoppf[RegData$OppfInst %in% c(12,25)] <- 100133
  RegData$HFoppf[RegData$OppfInst %in% c(21,22)] <- 100317
  RegData$HFoppf[RegData$OppfInst %in% c(6)] <- 100320
  RegData$HFoppf[RegData$OppfInst %in% c(3,23)] <- 101051
  RegData$HFoppf[RegData$OppfInst %in% c(13,27)] <- 101719
  RegData$HFoppf[RegData$OppfInst %in% c(16)] <- 101971
  RegData$HFoppf[RegData$OppfInst %in% c(28,29)] <- 700272
  RegData$HFoppf[RegData$OppfInst %in% c(4,5)] <- 4001031
  RegData$HFoppf[RegData$OppfInst %in% c(19,20)] <- 4201115
  RegData$HFoppf[RegData$OppfInst %in% c(99)] <- 9999999

  RegData$HFoppf <- factor(RegData$HFoppf, levels = orgresh, labels = shusnavn)

  return(invisible(RegData))
}

