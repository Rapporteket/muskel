#' Preparer variabler for plotting
#'
#' Denne funksjonen grupperer og klargjør variabler for andelsplot
#'
#' Her kan detaljer skrives
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return PrepData En liste med plotrelevante størrelser
#'
#' @export
#'
MuskelPrepVar <- function(RegData, valgtVar, enhetsUtvalg=1)
{
  retn= 'V'; tittel <- ''; AntVar <- NA; NVar <- NA;
  cexgr <- 1.0; grtxt <- ''; grtxt2 <- ''; subtxt <- '';



  if (valgtVar=='Alder') {
    RegData$Variabel <- RegData$AlderVreg
    RegData <- RegData[RegData$ForlopsType1Num == 1, ]
    tittel <- 'Alder ved førstegangsregistrering'	#bør ha med at alder per rapportdata sys.date...
    gr <- c(0, seq(10, 80, 10), 120)	#c(0,16,31,46,61,76,200)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
  }

  if (valgtVar %in% c('TidDebDiag', 'TidDebUtred', 'TidUtredDiag')) {
    RegData$Variabel <- RegData[, valgtVar]
    tittel <- switch(valgtVar,
                     TidDebDiag = 'Tid fra symptomdebut til diagnose',
                     TidDebUtred = 'Tid fra symptomdebut til utredningsstart',
                     TidUtredDiag = 'Tid fra utredningsstart til diagnose')
    gr <- switch(valgtVar,
                 TidDebDiag = c(0,1,2,3,4,5,6,10,20,30,40,50, 120),	#c(0,16,31,46,61,76,200)
                 TidDebUtred = c(0,1,2,3,4,5,6,10,20,30,40,50, 120),	#c(0,1,2,10,120),
                 TidUtredDiag = c(0,1,5,10,20, 120))	#c(0, 6,10,120))
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- switch(valgtVar,
                    TidDebDiag = c(0,1,2,3,4,5,levels(RegData$VariabelGr)[7:(length(gr)-2)], '50+'),
                    TidDebUtred = c(0,1,2,3,4,5,levels(RegData$VariabelGr)[7:(length(gr)-2)], '50+'),
                    TidUtredDiag = c(0,levels(RegData$VariabelGr)[2:(length(gr)-2)], '20+'))
    subtxt <- 'Antall år'
  }

  if (valgtVar == 'Diagnosegr') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    gr <- c(1:3)
    grtxt <- c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Fordeling av diagnosegrupper'
    subtxt <- 'Diagnosegrupper'
    retn <- 'H'
    # cexgr <- .8
  }

  if (valgtVar == 'Diagnoser_muskel') {
    RegData$Variabel <- as.character(RegData$DiagICD10)
    RegData <- RegData[RegData$Diagnosegr == 1, ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Fordeling av diagnoser blandt muskelsykdommer'
    subtxt <- 'Diagnosekoder'
    # cexgr <- .8
  }

  if (valgtVar == 'Diagnoser_atrofier') {
    RegData$Variabel <- as.character(RegData$DiagICD10)
    RegData <- RegData[RegData$Diagnosegr == 2, ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Fordeling av diagnoser blandt spinale muskelatrofier'
    subtxt <- 'Diagnosekoder'
    # cexgr <- .8
  }

  if (valgtVar == 'Diagnoser_nevropatier') {
    RegData$Variabel <- as.character(RegData$DiagICD10)
    RegData <- RegData[RegData$Diagnosegr == 3, ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Fordeling av diagnoser blandt polynevropatier'
    subtxt <- 'Diagnosekoder'
    # cexgr <- .8
  }


  if (valgtVar == 'DiagICD10') {
    RegData$VariabelGr <- RegData$DiagICD10
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Fordeling av diagnoser'
    subtxt <- 'Diagnosekoder'
    cexgr <- .6
  }

  if (valgtVar == 'Muskeldystrofier') {
    RegData <- RegData[RegData$DiagICD10 == c('G71.0') | RegData$Undergruppe %in% c(20, 21),]
    RegData$Undergruppe[is.na(RegData$Undergruppe)] <- 9999
    RegData$Undergruppe_label <- as.character(RegData$Undergruppe_label)
    RegData$Undergruppe_label[RegData$Undergruppe==9999] <- 'Ikke registrert'
    gr <- sort(unique(RegData$Undergruppe))
    grtxt <- RegData$Undergruppe_label[match(gr, RegData$Undergruppe)]
    RegData$VariabelGr <- factor(RegData$Undergruppe, levels=gr, labels=grtxt)
    tittel <- 'Fordeling av undergrupper av muskeldystrofier'
    subtxt <- 'Diagnoser'
    cexgr <- .8
    retn='H'
  }

  if (valgtVar == 'LGMD') {
    RegData <- RegData[RegData$Undergruppe == 4,]
    RegData$Undergruppe2[is.na(RegData$Undergruppe2)] <- 9999
    RegData$Undergruppe2_label <- as.character(RegData$Undergruppe2_label)
    RegData$Undergruppe2_label[RegData$Undergruppe2==9999] <- 'Ikke registrert'
    gr <- sort(unique(RegData$Undergruppe2))
    grtxt <- RegData$Undergruppe2_label[match(gr, RegData$Undergruppe2)]
    RegData$VariabelGr <- factor(RegData$Undergruppe2, levels=gr, labels=grtxt)
    tittel <- 'Fordeling av undergrupper av LGMD'
    subtxt <- 'Diagnoser'
    cexgr <- .8
    retn='H'
  }


  if (valgtVar == 'AndelGenVerifisert') { # per pasient
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$GenetiskAarsakPaavist[RegData$GenetiskAarsakPaavist != 1 | is.na(RegData$GenetiskAarsakPaavist)] <- 0
    AntVar <- tapply(RegData$GenetiskAarsakPaavist, RegData$Diagnosegr, sum)
    NVar <- tapply(RegData$GenetiskAarsakPaavist, RegData$Diagnosegr, length)
    grtxt <- c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati')
    tittel <- c('Andel pasienter med genetisk verifisert diagnose')
    retn='H'
  }

  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt,
                     retn=retn, cexgr=cexgr, AntVar=AntVar, NVar=NVar)

  return(invisible(PlotParams))


}

#     Nyeste <- aggregate(RegData[, c("HovedDato")], by=list(RegData$PasientID),
#                         function(x){y <- max(x)})
#
#     SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")], by=list(RegData$PasientID),
#                               function(x){if (1 %in% x) {y<-1} else {y<-0}})
#     test <- merge(SamletPrPID, RegData[, c("PasientID", "Diagnosegr", "DiagICD10", "GenetiskAarsakPaavist", "HovedDato", "DiagEndret", "ForlopsType1")],
#                   by.x = 'Group.1', by.y = 'PasientID')
#
#
#     AntVar
#     NVar


#   SamletPrPID <- aggregate(RegData[, c("Konservativ", "Irrigasjon", "Tibialisstimulering", "AnalInjection", "SNM", "Sfinkterplastikk",
#                                        "Rectopexi", "KirurgiForRectumprolaps", "Gracilisplastikk", "Stomi", "AnnetTidligereBeh")],
#                            by=list(RegData$PasientID), max, na.rm = TRUE)
#   if (valgtVar == 'GenUndergr') {
#     #... og for følgende undergrupper: G71.0, G71.1, G71.2, G71.3, G12.0, G12.1, G60.0
#     tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'av de med spesifikk diagnostisk undergruppe')
#     subtxt <- 'Diagnoser'
#     grtxt <- sort(c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G12.0', 'G12.1', 'G60.0'))
#     indUndergr <- which(as.character(RegData$DiagICD10) %in% grtxt)
#     RegData$Undergr <- 'dum'
#     RegData$Undergr[indUndergr] <- as.character(RegData$DiagICD10[indUndergr])
#     RegData <- RegData[which(RegData$Undergr != 'dum'), ]	#RegData[-which(RegData$Undergr == 'dum'), ]
#     RegData$Undergr <- as.factor(RegData$Undergr)
#     #RegData <- RegData[grtxt, ]
#     #Telle opp GenetiskAarsakPaavist == 1 for hver gruppe. Vil ha andel av alle.
#     Ngr <- table(RegData$Undergr)
#     Ngen <- table(RegData$Undergr[which(RegData$GenetiskAarsakPaavist==1)])
#     topsoyletxt <- paste('N=',Ngr, sep='')	#paste(Ngen,'/',Ngr, sep='')
#     Andeler <- Ngen/Ngr*100
#     grtxt <- names(Andeler)
#   }




#   if (valgtVar == 'GenHovedgr') {
#     #1.	Andelen med genetisk verifisert diagnose i de 3 gruppene Muskelsykdommer Spinal muskelatrofi
#     # Polynevropati,
#     tittel <- c('Andel pasienter med genetisk verifisert diagnose')
#     subtxt <- 'Diagnosegrupper'
#     #Telle opp GenetiskAarsakPaavist == 1 for hver gruppe. Vil ha andel av alle.
#     Ngr <- table(RegData$Diagnosegr)
#     Ngen <- table(as.factor(RegData$Diagnosegr)[which(RegData$GenetiskAarsakPaavist==1)])
#     topsoyletxt <- paste('N=',Ngr, sep='')	#paste(Ngen,'/',Ngr, sep='')
#     Andeler <- Ngen/Ngr*100
#     grtxt <- names(Andeler)
#   }
#
#   if (valgtVar == 'GenUndergr') {
#     #... og for følgende undergrupper: G71.0, G71.1, G71.2, G71.3, G12.0, G12.1, G60.0
#     tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'av de med spesifikk diagnostisk undergruppe')
#     subtxt <- 'Diagnoser'
#     grtxt <- sort(c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G12.0', 'G12.1', 'G60.0'))
#     indUndergr <- which(as.character(RegData$DiagICD10) %in% grtxt)
#     RegData$Undergr <- 'dum'
#     RegData$Undergr[indUndergr] <- as.character(RegData$DiagICD10[indUndergr])
#     RegData <- RegData[which(RegData$Undergr != 'dum'), ]	#RegData[-which(RegData$Undergr == 'dum'), ]
#     RegData$Undergr <- as.factor(RegData$Undergr)
#     #RegData <- RegData[grtxt, ]
#     #Telle opp GenetiskAarsakPaavist == 1 for hver gruppe. Vil ha andel av alle.
#     Ngr <- table(RegData$Undergr)
#     Ngen <- table(RegData$Undergr[which(RegData$GenetiskAarsakPaavist==1)])
#     topsoyletxt <- paste('N=',Ngr, sep='')	#paste(Ngen,'/',Ngr, sep='')
#     Andeler <- Ngen/Ngr*100
#     grtxt <- names(Andeler)
#   }
#
#   if (valgtVar == 'BoHF') {
#     retn <- 'H'
#     type <- 'ant'
#     tittel <- 'Hvilke HF-områder kommer pasientene fra?'
#     subtxt <- 'BoHF'
#     Ngr <- sort(table(RegData$Variabel))
#     Ngr <- Ngr[names(Ngr) != '']
#     grtxt <- names(Ngr)
#     topsoyletxt <- as.numeric(Ngr)
#     Andeler <- Ngr	#round(Ngr/N*100,2)
#   }
#
