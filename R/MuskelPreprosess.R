MuskelPreprosess <- function(RegData)
{
  RegData <- RegData[RegData$BasisRegStatus==1, ]

  # datoVar <- 'HovedDato'
  RegData$HovedDato <- as.POSIXlt(RegData$HovedDato, format="%Y-%m-%d")
  RegData$Alder <- round(as.numeric(difftime(Sys.Date(),
                                             strptime(RegData$FodselsDato, format="%Y-%m-%d" ))/365.25),0)
  RegData$AlderVreg <- round(as.numeric(difftime(RegData$HovedDato,
                                             strptime(RegData$FodselsDato, format="%Y-%m-%d" ))/365.25),0)

  RegData$Diagnosegr <- 9
  RegData$Diagnosegr[RegData$DiagICD10 %in%
                       c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G71.8', 'G71.9', 'G72.3', 'G73.6')] <- 1
  RegData$Diagnosegr[substr(RegData$DiagICD10, 1, 3) == 'G12'] <- 2
  RegData$Diagnosegr[substr(RegData$DiagICD10, 1, 3) == 'G60'] <- 3
  RegData$Diagnosegr[RegData$DiagICD10 == ''] <- 99

  RegData$Debut <- as.POSIXlt(RegData$FodselsDato, format="%Y-%m-%d")$year+1900 + RegData$DebutAlder
  RegData$TidDebDiag <- RegData$DiagnoseAar - RegData$Debut
  RegData$TidDebUtred <- RegData$Utredningsstart - RegData$Debut
  RegData$TidUtredDiag <- RegData$DiagnoseAar - RegData$Utredningsstart

  return(invisible(RegData))
}

