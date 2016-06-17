MuskelPreprosess <- function(RegData)
{
  RegData <- RegData[RegData$BasisRegStatus==1, ]

  # datoVar <- 'HovedDato'
  RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d") # Ordne datoformat
  RegData$RegInst_label <- as.character(RegData$RegInst_label)  # Kombinere RegInst og RegInstAndre
  RegData$RegInstAndre_label <- as.character(RegData$RegInstAndre_label)
  RegData$RegInst_label[RegData$RegInst==99] <- RegData$RegInstAndre_label[RegData$RegInst==99]
  RegData$RegInst[RegData$RegInst==99 & RegData$RegInstAndre!=99] <-
    RegData$RegInstAndre[RegData$RegInst==99 & RegData$RegInstAndre!=99]+13
  RegData$OppfInst_label <- as.character(RegData$OppfInst_label)  # Kombinere RegInst og RegInstAndre
  RegData$OppfInstAndre_label <- as.character(RegData$OppfInstAndre_label)
  RegData$OppfInst_label[which(RegData$OppfInst==99)] <- RegData$OppfInstAndre_label[which(RegData$OppfInst==99)]
  RegData$OppfInst[which(RegData$OppfInst==99 & RegData$OppfInstAndre!=99)] <-
    RegData$OppfInstAndre[which(RegData$OppfInst==99 & RegData$OppfInstAndre!=99)]+13
  RegData$DiagnoseStiltAvPrim_label <- as.character(RegData$DiagnoseStiltAvPrim_label)  # Kombinere RegInst og RegInstAndre
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


  RegData$Debut <- as.POSIXlt(RegData$FodselsDato, format="%Y-%m-%d")$year+1900 + RegData$DebutAlder
  RegData$TidDebDiag <- RegData$DiagnoseAar - RegData$Debut
  RegData$TidDebUtred <- RegData$Utredningsstart - RegData$Debut
  RegData$TidUtredDiag <- RegData$DiagnoseAar - RegData$Utredningsstart

  return(invisible(RegData))
}

