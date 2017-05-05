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
MuskelPrepVar <- function(RegData, valgtVar)
{
  retn= 'V'; tittel <- ''; AntVar <- NA; NVar <- NA; stabel <- 1
  cexgr <- 1.0; grtxt <- ''; grtxt2 <- ''; subtxt <- ''; incl_N=F; N_colwise=F



  if (valgtVar=='Alder') {
    RegData$Variabel <- RegData$AlderVreg
    RegData <- RegData[RegData$ForlopsType1Num == 1, ]
    tittel <- 'Alder ved førstegangsregistrering'	#bør ha med at alder per rapportdata sys.date...
    gr <- c(0, seq(10, 80, 10), 120)	#c(0,16,31,46,61,76,200)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
  }

  if (valgtVar=='DebutAlder') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering
    # RegData <- RegData[RegData$ForlopsType1Num == 1, ]
    tittel <- 'Alder ved debut'	#bør ha med at alder per rapportdata sys.date...
    gr <- c(0, seq(10, 80, 10), 120)	#c(0,16,31,46,61,76,200)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
  }

  if (valgtVar=='DiagnoseAlder') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering
    # RegData <- RegData[RegData$ForlopsType1Num == 1, ]
    tittel <- 'Alder ved diagnose'	#bør ha med at alder per rapportdata sys.date...
    gr <- c(0, seq(10, 80, 10), 120)	#c(0,16,31,46,61,76,200)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
  }


  if (valgtVar == 'Fysioterapi') {
    tittel <- 'Andel som får fysioterapi'
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    # grtxt <- c('Nei, men behov', 'Ja', 'Ikke behov', 'Ukjent')
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    # gr <- c(0:2, 9)
    # gr <- sort(as.numeric(Klokebok$listeverdier[Klokebok$navn_i_rapporteket == valgtVar]))
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- factor(RegData$Diagnosegr_label)
  }

  if (valgtVar == 'AndelSteroider') { #### Hvilken alder?!!!!!!
    tittel <- c('Andel pr. aldersgruppe med steroidbehandling')
    RegData <- RegData[!is.na(RegData$Sterioider), ]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- c(0,4,6,9,13,18,25,31,120)	#c(0,16,31,46,61,76,200)
    RegData$aldersgr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
    AntVar <- tapply(RegData$Sterioider, RegData$aldersgr, sum)
    NVar <- tapply(RegData$Sterioider, RegData$aldersgr, length)
    # grtxt <- names(AntVar)
    grtxt <- c('<4', '4-5', '6-8', '9-12', '13-17', '18-24', '25-30', '>30')
    retn='H'
    incl_N <- T
    N_colwise <- T
  }

  if (valgtVar == 'DiagBiopsi') {
    grtxt <- c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati')
    RegData <- RegData[which(RegData$Diagnosegr %in% c(1,2,3)), ]
    SamletPrPID <- aggregate(RegData[, valgtVar],
                             by=list(RegData$PasientID), max, na.rm = TRUE)
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0), labels = c('Ja', 'Nei'))
    RegData$Gr <- RegData$Diagnosegr
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = grtxt)
    tittel <- c('Andel pasienter som har fått muskelbiopsi')
  }


  if (valgtVar == 'DiagDNA') {
    grtxt <- c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati')
    RegData <- RegData[which(RegData$Diagnosegr %in% c(1,2,3)), ]
    SamletPrPID <- aggregate(RegData[, valgtVar],
                             by=list(RegData$PasientID), max, na.rm = TRUE)
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0), labels = c('Ja', 'Nei'))
    RegData$Gr <- RegData$Diagnosegr
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = grtxt)
    tittel <- c('Andel pasienter som har fått DNA-undersøkelse')

  }

  if (valgtVar == 'Utdanning') {
    # RegData <- RegData[which(RegData$Diagnosegr %in% c(1,2,3)), ]
    grtxt <- c('Grunnskole', 'Videregående skole, \n studieforberedende program','Videregående skole, \n yrkesfaglig program',
               'Høyskole eller \n universitetet', 'Ukjent', 'Ikke registrert')
    RegData$Variabel <- RegData[, valgtVar]
    RegData$Variabel[is.na(RegData$Variabel)] <- 99
    SamletPrPID <- aggregate(RegData$Variabel,
                             by=list(RegData$PasientID), function(x){if (max(x %in% 1:4)==1){y <- max(x)} else {y <- min(x)}})
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4, 9, 99), labels = grtxt)
#     RegData$Gr <- RegData$Diagnosegr
#     RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati'))
    tittel <- c('Utdanningsnivå')
    retn= 'H'
  }

  if (valgtVar == 'Sivilstatus') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData$Sivilstatus_label <- as.character(RegData$Sivilstatus_label)
    RegData$Sivilstatus_label[is.na(RegData$Variabel)] <- 'Ikke registrert'
    RegData$Variabel[is.na(RegData$Variabel)] <- 99
#     SamletPrPID <- aggregate(RegData$Variabel,
#                              by=list(RegData$PasientID), function(x){if (max(x %in% 1:4)==1){y <- max(x)} else {y <- min(x)}})
#     names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
#     RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- sort(unique(RegData$Variabel))
    grtxt <- RegData$Sivilstatus_label[match(gr, RegData$Variabel)]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- c('Sivilstatus')
    retn= 'H'
  }

  if (valgtVar == 'Arvegang') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData$Arvegang_label <- as.character(RegData$Arvegang_label)
    RegData$Arvegang_label[is.na(RegData$Variabel)] <- 'Ikke registrert'
    RegData$Variabel[is.na(RegData$Variabel)] <- 99
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- sort(unique(RegData$Variabel))
    grtxt <- RegData$Arvegang_label[match(gr, RegData$Variabel)]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- c('Arvegang')
    retn= 'H'
  }

  if (valgtVar == 'Gangfunksjon') {
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData$Variabel <- RegData[, valgtVar]
    RegData$Gangfunksjon_label <- as.character(RegData$Gangfunksjon_label)
    RegData$Gangfunksjon_label[is.na(RegData$Variabel)] <- 'Ikke registrert'
    RegData$Variabel[is.na(RegData$Variabel)] <- 99
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- sort(unique(RegData$Variabel))
    grtxt <- RegData$Gangfunksjon_label[match(gr, RegData$Variabel)]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- c('Gangfunksjon')
    RegData$Gr <- factor(RegData$Diagnosegr_label)
    retn= 'H'
  }


  if (valgtVar == 'Gangfunksjon_LGMD_DM1') {
    RegData <- RegData[which(RegData$Undergruppe == 20 | RegData$Undergruppe == 4), ]
    RegData$Variabel <- RegData[, 'Gangfunksjon']
    RegData$Gangfunksjon_label <- as.character(RegData$Gangfunksjon_label)
    RegData$Gangfunksjon_label[is.na(RegData$Variabel)] <- 'Ikke registrert'
    RegData$Variabel[is.na(RegData$Variabel)] <- 99
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- sort(unique(RegData$Variabel))
    grtxt <- RegData$Gangfunksjon_label[match(gr, RegData$Variabel)]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- c('Gangfunksjon')
    RegData$Gr <- factor(RegData$Undergruppe, levels = c(4,20), labels=c('LGMD', 'DM1'))
  }


  if (valgtVar == 'Smertestillende_LGMD_DM1') {
    RegData <- RegData[which(RegData$Undergruppe == 20 | RegData$Undergruppe == 4), ]
    RegData$Variabel <- RegData[, "Smertestillende"]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- 0:1
    grtxt <- c('Nei', 'Ja')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- c('Bruk av smertestillende')
    RegData$Gr <- factor(RegData$Undergruppe, levels = c(4,20), labels=c('LGMD', 'DM1'))
    stabel <- TRUE
  }


  if (valgtVar == 'UtdanningDM1') {
    grtxt <- c('Grunnskole', 'Videregående skole, \n studieforberedende program','Videregående skole, \n yrkesfaglig program',
               'Høyskole eller \n universitetet', 'Ukjent', 'Ikke registrert')
    RegData <- RegData[which(RegData$Undergruppe %in% c(20)), ]
    RegData$Variabel <- RegData[, 'Utdanning']
    RegData$Variabel[is.na(RegData$Variabel)] <- 99
    SamletPrPID <- aggregate(RegData$Variabel,
                             by=list(RegData$PasientID), function(x){if (max(x %in% 1:4)==1){y <- max(x)} else {y <- min(x)}})
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4, 9, 99), labels = grtxt)
    #     RegData$Gr <- RegData$Diagnosegr
    #     RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = grtxt)
    tittel <- c('Utdanningsnivå DM1')
    retn= 'H'
  }

  if (valgtVar == 'UtdanningLGMD') {
    grtxt <- c('Grunnskole', 'Videregående skole, \n studieforberedende program','Videregående skole, \n yrkesfaglig program',
               'Høyskole eller \n universitetet', 'Ukjent', 'Ikke registrert')
    RegData <- RegData[which(RegData$Undergruppe==4), ]
    RegData$Variabel <- RegData[, 'Utdanning']
    RegData$Variabel[is.na(RegData$Variabel)] <- 99
    SamletPrPID <- aggregate(RegData$Variabel,
                             by=list(RegData$PasientID), function(x){if (max(x %in% 1:4)==1){y <- max(x)} else {y <- min(x)}})
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1:4, 9, 99), labels = grtxt)
    #     RegData$Gr <- RegData$Diagnosegr
    #     RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = grtxt)
    tittel <- c('Utdanningsnivå LGMD')
    retn= 'H'
  }



  if (valgtVar == 'FysioManglerAarsak') {
    tittel <- 'Årsak til manglende fysioterapi'
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    grtxt <- c('Står på venteliste', 'Ikke fysiotilbud\n i kommunen', 'Ingen effekt av\n eksisterende tilbud',
               'For lang reisevei', 'For kostbart', 'Annet', 'Ukjent')
    gr <- c(1:6, 9)
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- factor(RegData$Diagnosegr_label)
    retn= 'H'
  }

  if (valgtVar == 'Ergoterapi') {
    tittel <- 'Andel som får ergoterapi'
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    grtxt <- c('Nei, men behov', 'Ja', 'Ikke behov', 'Ukjent')
    gr <- c(0:2, 9)
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- factor(RegData$Diagnosegr_label)
  }


  if (valgtVar=='HjerteAffAlder') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    tittel <- 'Alder ved hjerteaffeksjon'	#bør ha med at alder per rapportdata sys.date...
    gr <- c(0, seq(10, 80, 10), 120)	#c(0,16,31,46,61,76,200)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
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
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
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
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData <- RegData[RegData$Diagnosegr == 1, ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Fordeling av diagnoser blandt muskelsykdommer'
    subtxt <- 'Diagnosekoder'
    retn <- 'H'
    # cexgr <- .8
  }

  if (valgtVar == 'Diagnoser_atrofier') {
    RegData$Variabel <- as.character(RegData$DiagICD10)
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData <- RegData[RegData$Diagnosegr == 2, ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Fordeling av diagnoser blandt spinale muskelatrofier'
    subtxt <- 'Diagnosekoder'
    # cexgr <- .8
  }

  if (valgtVar == 'Diagnoser_nevropatier') {
    RegData$Variabel <- as.character(RegData$DiagICD10)
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData <- RegData[RegData$Diagnosegr == 3, ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Fordeling av diagnoser blandt polynevropatier'
    subtxt <- 'Diagnosekoder'
    # cexgr <- .8
  }


  if (valgtVar == 'DiagICD10') {
    RegData$VariabelGr <- as.character(RegData$DiagICD10)
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr[RegData$VariabelGr==''] <- 'Ikke registrert'
    RegData$VariabelGr <- as.factor(RegData$VariabelGr)
    grtxt <- levels(RegData$VariabelGr)
    tittel <- 'Fordeling av diagnoser'
    subtxt <- 'Diagnosekoder'
    retn <- 'H'
    # cexgr <- .6
  }

  if (valgtVar == 'Muskeldystrofier') {
    RegData <- RegData[RegData$DiagICD10 == c('G71.0') | RegData$Undergruppe %in% c(20, 21),]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
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

  if (valgtVar == 'SMA') {
    RegData <- RegData[which(RegData$DiagICD10 %in% c('G12.0', 'G12.1', 'G12.8', 'G12.9')), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Undergruppe_label <- as.character(RegData$Undergruppe_label)
    RegData$Undergruppe[which(RegData$Undergruppe == 1090)] <- 1080
    RegData$Undergruppe[which(RegData$DiagICD10 == 'G12.9')] <- 10000
    RegData$Undergruppe_label[which(RegData$DiagICD10 == 'G12.9')] <- 'G12.9 Uspesifisert SMA'
    RegData$Undergruppe_label[which(RegData$Undergruppe == 2080)] <- 'G12.1 - Usikker undergruppe'
    gr <- sort(unique(RegData$Undergruppe))
    grtxt <- RegData$Undergruppe_label[match(gr, RegData$Undergruppe)]
    RegData$VariabelGr <- factor(RegData$Undergruppe, levels=gr, labels=grtxt)
    tittel <- 'Fordeling av spinal muskelatrofi'
    subtxt <- 'Diagnoser'
    cexgr <- .8
    retn='H'
  }


  if (valgtVar == 'LGMD') {
    RegData <- RegData[RegData$Undergruppe == 4,]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
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

  if (valgtVar == 'CMT') {  ### Må avklare behandling av "Usikker undergruppe"
    RegData <- RegData[RegData$Undergruppe %in% c(100:104, 1100, 2100),]
    RegData$Undergruppe2[which(RegData$Undergruppe == 1100)] <- 1100
    RegData$Undergruppe2[which(RegData$Undergruppe == 2100)] <- 2100
    RegData$Undergruppe2[which(RegData$Undergruppe2 %in% 1100:1104)] <- 1100
    RegData$Undergruppe2[which(RegData$Undergruppe2 %in% 2100:2104)] <- 2100
    RegData$Undergruppe2_label <- as.character(RegData$Undergruppe2_label)
    RegData$Undergruppe2_label[which(RegData$Undergruppe == 1100)] <- 'Annen'
    RegData$Undergruppe2_label[which(RegData$Undergruppe == 2100)] <- 'Ukjent undergruppe'
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- sort(unique(RegData$Undergruppe2))
    grtxt <- RegData$Undergruppe2_label[match(gr, RegData$Undergruppe2)]
    RegData$VariabelGr <- factor(RegData$Undergruppe2, levels=gr, labels=grtxt)
    tittel <- 'Fordeling av undergrupper av CMT'
    subtxt <- 'Diagnoser'
    cexgr <- .8
    retn='H'
  }


  if (valgtVar == 'HjerteAff_DM1') {
    # RegData <- RegData[which(RegData$Undergruppe %in% c(20, 21) | (RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13)),]
    RegData <- RegData[which(RegData$Undergruppe %in% c(20)), ]

    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData$HjerteAff
    grtxt <- c('Nei', 'Ja', 'Er henvist kardiolog', 'Ukjent')
    gr <- c(0:2, 9)
    RegData <- RegData[RegData$Variabel %in% gr, ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Hjerteaffeksjon (%) ved DM 1'
#     RegData$Gr <- RegData$Undergruppe
#     RegData$Gr <- factor(RegData$Undergruppe, levels = c(20, 21, 4), labels = c('DM1', 'DM2', 'FKRP'))
    stabel <- F
  }

  if (valgtVar == 'HjerteAff_DM2') {
    # RegData <- RegData[which(RegData$Undergruppe %in% c(20, 21) | (RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13)),]
    RegData <- RegData[which(RegData$Undergruppe %in% c(21)), ]

    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData$HjerteAff
    grtxt <- c('Nei', 'Ja', 'Er henvist kardiolog', 'Ukjent')
    gr <- c(0:2, 9)
    RegData <- RegData[RegData$Variabel %in% gr, ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Hjerteaffeksjon (%) ved DM 2'
    #     RegData$Gr <- RegData$Undergruppe
    #     RegData$Gr <- factor(RegData$Undergruppe, levels = c(20, 21, 4), labels = c('DM1', 'DM2', 'FKRP'))
    stabel <- F
  }



  if (valgtVar == 'HjerteAff_LGMD2I') {
    # RegData <- RegData[which(RegData$Undergruppe %in% c(20, 21) | (RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13)),]
    RegData <- RegData[which(RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13), ]

    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData$HjerteAff
    grtxt <- c('Nei', 'Ja', 'Er henvist kardiolog', 'Ukjent')
    gr <- c(0:2, 9)
    RegData <- RegData[RegData$Variabel %in% gr, ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Hjerteaffeksjon (%) ved LGMD2I (Fkrp)'
    #     RegData$Gr <- RegData$Undergruppe
    #     RegData$Gr <- factor(RegData$Undergruppe, levels = c(20, 21, 4), labels = c('DM1', 'DM2', 'FKRP'))
    stabel <- F
  }


  #################################################################################################################
  #################################################################################################################
  #################################################################################################################

  # if (valgtVar == 'Hjerteaff_DM1_LGMD2I') { # per pasient
  #
  #   grtxt <- c('DM1', 'LGMD2I')
  #   RegData <- RegData[which((RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13) | RegData$Undergruppe %in% c(20)), ]
  #   RegData$Gr <- RegData$Undergruppe
  #   RegData$Gr[RegData$Gr==20] <- 1
  #   RegData$Gr[RegData$Gr==4] <- 2
  #   RegData$Gr <- factor(RegData$Gr, levels = 1:2, labels = grtxt)
  #
  #   # RegData <- RegData[which(!(RegData$DiagICD10 %in% c('G71.9', 'G12.9', 'G60.9'))), ]
  #   SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")],
  #                            by=list(RegData$PasientID), function(x){if (1 %in% x) {y <- 1} else
  #                            {if (0 %in% x) {y <- 0} else {if (9 %in% x) {y <- 9} else {y <- 99}}}})
  #   names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
  #   RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
  #   RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
  #   RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
  #   RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = c('Ja', 'Nei', 'Ukjent', 'Ikke registrert'))
  #   RegData$Gr <- RegData$Diagnosegr
  #   RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = grtxt)
  #   tittel <- c('Andel pasienter med genetisk verifisert diagnose')
  # }

  #################################################################################################################
  #################################################################################################################
  #################################################################################################################


  if (valgtVar == 'Arbeid_LGMD2I') {
    RegData <- RegData[which(RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData$Uforetrygd
    RegData$Variabel[RegData$Variabel %in% 2:6] <- 2
    gr <- c(0, 1, 2, 7, 9)
    grtxt <- c('Arbeidsfør', 'Helt ufør', 'Delvis ufør','Langtidssykemelding/\n arbeidsavklaringspenger', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Arbeidsuførhet: LGMD2I'
    stabel <- F
    retn='H'
  }

  if (valgtVar == 'Arbeid_LGMD') {
    RegData <- RegData[which(RegData$Undergruppe==4), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData$Uforetrygd
    RegData$Variabel[RegData$Variabel %in% 2:6] <- 2
    gr <- c(0, 1, 2, 7, 9)
    grtxt <- c('Arbeidsfør', 'Helt ufør', 'Delvis ufør','Langtidssykemelding/\n arbeidsavklaringspenger', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Arbeidsuførhet: LGMD'
    stabel <- F
    retn='H'
  }


  if (valgtVar == 'Arbeid_DM1') {
    RegData <- RegData[which(RegData$Undergruppe %in% c(20)), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData$Uforetrygd
    RegData$Variabel[RegData$Variabel %in% 2:6] <- 2
    gr <- c(0, 1, 2, 7, 9)
    grtxt <- c('Arbeidsfør', 'Helt ufør', 'Delvis ufør','Langtidssykemelding/\n arbeidsavklaringspenger', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Arbeidsuførhet: DM1'
    stabel <- F
    retn='H'
  }

  if (valgtVar == 'Arbeid_DM2') {
    RegData <- RegData[which(RegData$Undergruppe %in% c(21)), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData$Uforetrygd
    RegData$Variabel[RegData$Variabel %in% 2:6] <- 2
    gr <- c(0, 1, 2, 7, 9)
    grtxt <- c('Arbeidsfør', 'Helt ufør', 'Delvis ufør','Langtidssykemelding/\n arbeidsavklaringspenger', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Arbeidsuførhet: DM2'
    stabel <- F
    retn='H'
  }

  if (valgtVar == 'Arbeid_CMT') {
    RegData <- RegData[which(RegData$DiagICD10 == 'G60.0'), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData$Uforetrygd
    RegData$Variabel[RegData$Variabel %in% 2:6] <- 2
    gr <- c(0, 1, 2, 7, 9)
    grtxt <- c('Arbeidsfør', 'Helt ufør', 'Delvis ufør','Langtidssykemelding/\n arbeidsavklaringspenger', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Arbeidsuførhet: CMT'
    stabel <- F
    retn='H'
  }


  if (valgtVar == 'AndelGenVerifisert') { # per pasient
    grtxt <- c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati')
    RegData <- RegData[which(RegData$Diagnosegr %in% c(1,2,3)), ]
    # RegData <- RegData[which(!(RegData$DiagICD10 %in% c('G71.9', 'G12.9', 'G60.9'))), ]
    SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")],
                             by=list(RegData$PasientID), function(x){if (1 %in% x) {y <- 1} else
                             {if (0 %in% x) {y <- 0} else {if (9 %in% x) {y <- 9} else {y <- 99}}}})
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = c('Ja', 'Nei', 'Ukjent', 'Ikke registrert'))
    RegData$Gr <- RegData$Diagnosegr
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = grtxt)
    tittel <- c('Andel pasienter med genetisk verifisert diagnose')
  }

  if (valgtVar == 'AndelGenVerifisertSpes') { # per pasient
    grtxt <- c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati')
    RegData <- RegData[which(RegData$Diagnosegr %in% c(1,2,3)), ]
    RegData <- RegData[which(!(RegData$DiagICD10 %in% c('G71.9', 'G12.9', 'G60.9'))), ]
    SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")],
                             by=list(RegData$PasientID), function(x){if (1 %in% x) {y <- 1} else
                               {if (0 %in% x) {y <- 0} else {if (9 %in% x) {y <- 9} else {y <- 99}}}})
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = c('Ja', 'Nei', 'Ukjent', 'Ikke registrert'))
    RegData$Gr <- RegData$Diagnosegr
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = grtxt)
    tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'av de med spesifikk diagnose')
  }

  if (valgtVar == 'AndelGenVerifisertSpesUndergr') { # per pasient
    tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'av de med spesifikk diagnose')
    subtxt <- 'Diagnoser'
    grtxt <- sort(c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G12.0', 'G12.1', 'G60.0'))
    RegData <- RegData[which(as.character(RegData$DiagICD10) %in% grtxt), ]
    SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")],
                             by=list(RegData$PasientID), function(x){if (1 %in% x) {y <- 1} else
                             {if (0 %in% x) {y <- 0} else {if (9 %in% x) {y <- 9} else {y <- 99}}}})
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = c('Ja', 'Nei', 'Ukjent', 'Ikke registrert'))
    RegData$Gr <- as.factor(as.character(RegData$DiagICD10))
    # RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = grtxt)
  }



  if (valgtVar == 'DiagByggerPaa') { # per pasient
    SamletPrPID <- aggregate(RegData[, c("DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA', 'DiagBiopsi')],
                             by=list(RegData$PasientID), max, na.rm = TRUE)
    N <- dim(SamletPrPID)[1]
    AntVar <- colSums(SamletPrPID[,-1], na.rm = T)
    NVar<-rep(N, length(AntVar))
    grtxt <- c('Anamnese/klinisk\n undersøkelse', 'EMG/Nevrografi', 'CK', 'DNA-undersøkelse', 'Muskelbiopsi')
    tittel <- c('Diagnose bygger på')
    retn='H'
  }



  if (valgtVar == 'DiagByggerPaa_v2') { # Kun for DNA og Biopsi
    RegData <- RegData[which(RegData$Undergruppe %in% 1:2), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- 99
    RegData$Variabel[RegData$DiagBiopsi == 1] <- 1
    RegData$Variabel[RegData$DiagDNA == 1] <- 2
    RegData$Variabel[RegData$DiagBiopsi == 1 & RegData$DiagDNA == 1] <- 3
    RegData$VariabelGr <- factor(RegData$Variabel, levels = c(1,2,3,99), labels = c('Biopsi', 'Gentest', 'Biopsi og gentest',
                                                                                      'Ingen/ukjent'))
    grtxt <- c('Biopsi alene', 'Gentest alene', 'Biopsi og gentest', 'Ingen/annen/ukjent')
    tittel <- c('DMD/BMD-diagnose bygger på')
    retn='H'
  }


  if (valgtVar == 'KonkrUndGrDuchBeck') { # per pasient
    RegData <- RegData[which(RegData$Undergruppe %in% 1:2), ]
    SamletPrPID <- aggregate(RegData[, c("Delesjon", "PunktMutasjon", "Duplikasjon")],  #, 'GenetiskAarsakPaavist'
                             by=list(RegData$PasientID), max, na.rm = TRUE)
    N <- dim(SamletPrPID)[1]
    AntVar <- colSums(SamletPrPID[,-1], na.rm = T)
    NVar<-rep(N, length(AntVar))
    grtxt <- names(AntVar)
    tittel <- c('Mutasjonstype ved Duchenne/Becker')
    retn='H'
  }





#   if (valgtVar == 'DiagGenVerifisert') { # per pasient
#
#     SamletPrPID <- aggregate(RegData[, c("DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA', 'DiagBiopsi')],
#                              by=list(RegData$PasientID), max, na.rm = TRUE)
#     N <- dim(SamletPrPID)[1]
#     AntVar <- colSums(SamletPrPID[,-1], na.rm = T)
#     NVar<-rep(N, length(AntVar))
#     grtxt <- c('Anamnese/klinisk\n undersøkelse', 'EMG/Nevrografi', 'CK', 'DNA-undersøkelse', 'Muskelbiopsi')
#     tittel <- c('Diagnose bygger på')
#     retn='H'
#   }


  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt,
                     retn=retn, cexgr=cexgr, AntVar=AntVar, NVar=NVar, stabel=stabel, incl_N=incl_N, N_colwise=N_colwise)

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
