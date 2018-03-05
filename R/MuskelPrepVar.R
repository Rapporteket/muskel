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
  retn= 'V'; tittel <- ''; AntVar <- NA; NVar <- NA; stabel <- 1; flerevar <- 0
  cexgr <- 1.0; grtxt <- ''; grtxt2 <- ''; subtxt <- ''; incl_N=F; N_colwise=F;
  if (valgtVar == 'OppfAar') {
    tittel <- 'Oppfølginger etter år'
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- sort(unique(RegData$Aar))
    grtxt <- as.character(gr)
    # aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    # aux <- aux[order(as.numeric(aux$listeverdier)), ]
    # gr <- as.numeric(aux$listeverdier)
    # grtxt <- aux$listetekst
    # gr <- c(0:2, 9)
    # gr <- sort(as.numeric(Klokebok$listeverdier[Klokebok$navn_i_rapporteket == valgtVar]))
    RegData$Variabel <- RegData[, 'Aar']
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    # RegData$Gr <- factor(RegData$Diagnosegr_label)
  }

  if (valgtVar=='PeriodiskeParalyser') {
    tittel <- 'Myotonier/Periodiske paralyser'
    RegData$Variabel <- RegData$Undergruppe
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering
    gr <- c(22, 23, 24, 25, 60, 61, 1060, 2060)
    RegData <- RegData[RegData$Variabel %in% gr, ]
    grtxt <- RegData$Undergruppe_label[match(gr, RegData$Undergruppe)]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    retn= 'H'
    incl_N=T
  }

  if (valgtVar=='Alder') {
    RegData$Variabel <- RegData$AlderVreg
    RegData <- RegData[RegData$ForlopsType1Num == 1, ]
    tittel <- 'Alder ved førstegangsregistrering'	#bør ha med at alder per rapportdata sys.date...
    gr <- c(0, seq(10, 80, 10), 120)	#c(0,16,31,46,61,76,200)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
    grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
    subtxt <- 'Aldersgrupper'
  }

  if (valgtVar=='AlderDagens') {
    RegData$Variabel <- RegData$Alder
    RegData <- RegData[RegData$ForlopsType1Num == 1, ]
    tittel <- 'Dagens alder'	#bør ha med at alder per rapportdata sys.date...
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

  if (valgtVar == 'Fysioterapi_nord') {
    tittel <- 'Andel som får fysioterapi'
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData=RegData[which(RegData$Fylke %in% c('TROMS', 'FINNMARK', 'NORDLAND')), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    # grtxt <- c('Nei, men behov', 'Ja', 'Ikke behov', 'Ukjent')
    aux <- Klokebok[Klokebok$navn_i_rapporteket == 'Fysioterapi', c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    # gr <- c(0:2, 9)
    # gr <- sort(as.numeric(Klokebok$listeverdier[Klokebok$navn_i_rapporteket == valgtVar]))
    RegData$Variabel <- RegData[, 'Fysioterapi']
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- as.factor(as.character(RegData$Fylke))
    stabel <- 0
  }

  if (valgtVar == 'Fysioterapi_sor') {
    tittel <- 'Andel som får fysioterapi'
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData=RegData[which(RegData$Fylke %in% c('OSLO', 'AKERSHUS')), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    # grtxt <- c('Nei, men behov', 'Ja', 'Ikke behov', 'Ukjent')
    aux <- Klokebok[Klokebok$navn_i_rapporteket == 'Fysioterapi', c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    # gr <- c(0:2, 9)
    # gr <- sort(as.numeric(Klokebok$listeverdier[Klokebok$navn_i_rapporteket == valgtVar]))
    RegData$Variabel <- RegData[, 'Fysioterapi']
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- as.factor(as.character(RegData$Fylke))
    stabel <- 0
  }


  if (valgtVar == 'AndelSteroider') { #### Hvilken alder?!!!!!!
    tittel <- c('Andel pr. aldersgruppe med steroidbehandling')
    RegData <- RegData[!is.na(RegData$Steroider), ]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    gr <- c(0,4,6,9,13,18,25,31,120)	#c(0,16,31,46,61,76,200)
    RegData$aldersgr <- cut(RegData$Alder, breaks=gr, include.lowest=TRUE, right=FALSE)
    AntVar <- tapply(RegData$Steroider, RegData$aldersgr, sum)
    NVar <- tapply(RegData$Steroider, RegData$aldersgr, length)
    # grtxt <- names(AntVar)
    grtxt <- c('<4', '4-5', '6-8', '9-12', '13-17', '18-24', '25-30', '>30')
    retn='H'
    incl_N <- F
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


  if (valgtVar == 'HoyesteUtdanning') {
    tittel <- c('Høyest oppnådde utdanningsnivå')
    # RegData <- RegData[which(RegData$PasientAlder>25), ]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Variabel <- RegData[, "Utdanning"]
    RegData$Variabel[RegData$Variabel==3] <- 2
    gr <- c(1,2,4,9)
    grtxt <- c('Grunnskole', 'Videregående skole', 'Høyskole eller universitetet', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- NA
    RegData$Gr[which(RegData$Undergruppe == 20)] <- 1
    RegData$Gr[which(RegData$Undergruppe == 4)] <- 2
    RegData$Gr[which(RegData$DiagICD10 == 'G60.0')] <- 3
    RegData <- RegData[which(RegData$Gr %in% 1:3), ]
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = c('Dystrophia myotonica 1', 'Limb-girdle muskeldystrofi', 'Charcot Marie Tooth'))
    stabel <- 0
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
    RegData$Gr <- factor(RegData$Undergruppe, levels = c(4,20), labels=c('Limb−girdle muskeldystrofi', 'Dystrophia myotonica 1'))
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
    RegData$Gr <- factor(RegData$Undergruppe, levels = c(4,20), labels=c('LGMD', 'Dystrophia myotonica 1'))
    stabel <- TRUE
  }

  if (valgtVar == 'Smertestillende_MD_CMT_SMA') {
    RegData <- RegData[which(RegData$DiagICD10 %in% c('G60.0', 'G71.0', 'G12.0', 'G12.1', 'G12.8', 'G12.9')), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Gr <- NA
    RegData$Gr[RegData$DiagICD10 == 'G60.0'] <- 2
    RegData$Gr[RegData$DiagICD10 == 'G71.0'] <- 1
    RegData$Gr[RegData$DiagICD10 %in% c('G12.0', 'G12.1', 'G12.8', 'G12.9')] <- 3
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels=c('Muskeldystrofi', 'Charcot Marie Tooth', 'Spinal muskelatrofi'))
    RegData$Variabel <- RegData[, "Smertestillende"]
    gr <- 0:1
    grtxt <- c('Nei', 'Ja')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- c('Bruk av smertestillende')
    # RegData$Gr <- factor(RegData$Undergruppe, levels = c(4,20), labels=c('LGMD', 'DM1'))
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
    tittel <- c('Utdanningsnivå Dystrophia myotonica 1')
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
    # RegData$Gr <- factor(RegData$Diagnosegr_label)
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
    N_colwise <- T
    # cexgr <- .8
  }

  if (valgtVar == 'KognitivSvikt') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Kognitiv svikt'
  }


  if (valgtVar == 'OppfolgBarnelegeNevrolog') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Oppfølging hos barnelege/nevrolog'
  }

  if (valgtVar == 'PsykiskHelsetjeneste') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Psykisk helsetjeneste'
  }

  if (valgtVar == 'OppholdRehab') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- c('Hatt/venter opphold på', 'rehabiliteringsinstitusjon')
  }

  if (valgtVar == 'TilbudKostveiledning') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Tilbud om kostveiledning'
  }

  if (valgtVar == 'TilbudGenetiskVeiledning') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Tilbud om genetisk veiledning'
  }

  if (valgtVar == 'AnsvarsgruppeIP') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Ansvarsgruppe/Individuell plan'
  }

  if (valgtVar == 'BPA') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Brukerstyrt personlig assistent (BPA)'
  }

  if (valgtVar == 'Arbeid') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'I arbeid/utdanning'
    retn <- 'H'
  }

  if (valgtVar == 'Uforetrygd') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Uføretrygd'
    retn <- 'H'
  }

  if (valgtVar == 'Sivilstatus') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Sivilstatus'
    retn <- 'H'
  }

  if (valgtVar == 'MedikBehandling') {
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- Klokebok[Klokebok$navn_i_rapporteket == valgtVar, c("listeverdier", "listetekst")]
    aux <- aux[order(as.numeric(aux$listeverdier)), ]
    gr <- as.numeric(aux$listeverdier)
    grtxt <- aux$listetekst
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    tittel <- 'Medikamentell behandling'
  }

  if (valgtVar == 'TypeMedikBehandling') {
    tittel <- c('Type medikamentell behandling')
    RegData <- RegData[which(RegData$MedikBehandling==1), ] # Kun for de med medikamentell behandling
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Per pasient, velger nyeste registrering
    AntVar <- colSums(RegData[, c("Steroider", "Smertestillende", "Antiarytmika", "ACEHemmer", "AnnetHjerteMed", "AnnenMedikBeh")], na.rm = T)
    N <- dim(RegData)[1]
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Steroider", "Smertestillende", "Antiarytmika", "ACE-hemmer", "Andre hjertemed.", "Annet")
    retn='H'
  }



  if (valgtVar == 'Diagnoser_muskel') {
    RegData$Variabel <- as.character(RegData$DiagICD10)
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData <- RegData[RegData$Diagnosegr == 1, ]

    aux <- sort(table(RegData$Variabel), decreasing = T)
    grtxt <- names(aux)
    RegData$VariabelGr <- factor(RegData$Variabel, levels=grtxt)
    N_colwise <- T

    # RegData$VariabelGr <- as.factor(RegData$Variabel)
    # grtxt <- levels(RegData$VariabelGr)
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
    # RegData$VariabelGr <- as.character(RegData$DiagICD10)
    RegData$VariabelGr <- RegData$DiagICD10
    RegData <- RegData[RegData$VariabelGr!='', ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    aux <- sort(table(RegData$VariabelGr[RegData$VariabelGr!='']), decreasing = T)
    # RegData$VariabelGr[RegData$VariabelGr==''] <- 'Ikke registrert'
    # grtxt <- c(names(aux), 'Ikke registrert')
    grtxt <- names(aux)
    # gr <- 1:length(grtxt)
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels=grtxt)
    N_colwise <- T
    # RegData$VariabelGr <- as.factor(RegData$VariabelGr)
    # grtxt <- levels(RegData$VariabelGr)
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
    aux <- sort(table(RegData$Undergruppe_label), decreasing = T)
    grtxt <- names(aux)
    RegData$VariabelGr <- factor(RegData$Undergruppe_label, levels=grtxt)
    N_colwise <- T
    tittel <- 'Fordeling av undergrupper av muskeldystrofier'
    subtxt <- 'Diagnoser'
    cexgr <- .8
    retn='H'
  }

  if (valgtVar == 'SMA') {
    RegData <- RegData[which(RegData$DiagICD10 %in% c('G12.0', 'G12.1', 'G12.8', 'G12.9')), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    # RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    # RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$Undergruppe_label <- as.character(RegData$Undergruppe_label)
    RegData$Undergruppe[which(RegData$Undergruppe == 1090)] <- 1080
    RegData$Undergruppe[which(RegData$DiagICD10 == 'G12.9')] <- 10000
    RegData$Undergruppe_label[which(RegData$DiagICD10 == 'G12.9')] <- 'Uspesifisert SMA'
    RegData$Undergruppe_label[which(RegData$Undergruppe == 2080)] <- 'Usikker undergruppe'
    RegData$Undergruppe_label[which(RegData$Undergruppe_label == '')] <- 'Ingen undergruppe'
    aux <- sort(table(RegData$Undergruppe_label), decreasing = T)
    grtxt <- names(aux)
    RegData$VariabelGr <- factor(RegData$Undergruppe_label, levels=grtxt)
    N_colwise <- T
    # gr <- sort(unique(RegData$Undergruppe))
    # grtxt <- RegData$Undergruppe_label[match(gr, RegData$Undergruppe)]
    # RegData$VariabelGr <- factor(RegData$Undergruppe, levels=gr, labels=grtxt)
    tittel <- 'Fordeling av spinal muskelatrofi'
    subtxt <- 'Diagnoser'
    cexgr <- .8
    retn='H'
  }

  if (valgtVar == 'SMA_periodiskepar') {
    RegData <- RegData[which(RegData$Undergruppe %in% c(70, 81, 82, 83)), ]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]

    gr <- c(70, 81, 82, 83)
    grtxt <- RegData$Undergruppe_label[match(gr, RegData$Undergruppe)]
    RegData$VariabelGr <- factor(RegData$Undergruppe, levels = gr, labels = grtxt)
    retn= 'H'
    incl_N=T
    tittel <- 'Fordeling av spinal muskelatrofi'
    subtxt <- 'Diagnoser'
    cexgr <- .8
    retn='H'
  }


  if (valgtVar == 'LGMD') {
    # RegData <- RegData[RegData$Undergruppe_label == 'Limb-girdle muskeldystrofi',]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData <- RegData[RegData$Undergruppe_label == 'Limb-girdle muskeldystrofi',]
    RegData$Undergruppe2[is.na(RegData$Undergruppe2)] <- 9999
    RegData$Undergruppe2_label <- as.character(RegData$Undergruppe2_label)
    RegData$Undergruppe2_label[RegData$Undergruppe2==9999] <- 'Ikke registrert'
    RegData$Undergruppe2_label[RegData$Undergruppe2==7] <- 'LGMD2C (gamma-sarkoglykan)'  ## Ad-hoc løsning til årsrapport
    aux <- sort(table(RegData$Undergruppe2_label), decreasing = T)
    grtxt <- names(aux)
    RegData$VariabelGr <- factor(RegData$Undergruppe2_label, levels=grtxt)
    N_colwise <- T
    tittel <- c('Fordeling av undergrupper av', 'Limb-girdle muskeldystrofi')
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

    aux <- sort(table(RegData$Undergruppe2_label), decreasing = T)
    grtxt <- names(aux)
    RegData$VariabelGr <- factor(RegData$Undergruppe2_label, levels=grtxt)
    N_colwise <- T
    # gr <- sort(unique(RegData$Undergruppe2))
    # grtxt <- RegData$Undergruppe2_label[match(gr, RegData$Undergruppe2)]
    # RegData$VariabelGr <- factor(RegData$Undergruppe2, levels=gr, labels=grtxt)
    tittel <- c('Fordeling av undergrupper av', 'Charcot Marie Tooth')
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

  if (valgtVar == 'HjerteAff_samlet') {
    tittel <- c('Hjerteaffeksjon')
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData$Variabel <- RegData[, "HjerteAff"]
    gr <- c(0,1,2,9)
    grtxt <- c('Nei', 'Ja', 'Henvist kardiolog', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData$Gr <- NA
    RegData$Gr[which(RegData$Undergruppe %in% c(20))] <- 1
    RegData$Gr[which(RegData$Undergruppe %in% c(21))] <- 2
    RegData$Gr[which(RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13)] <- 3
    RegData <- RegData[which(RegData$Gr %in% 1:3), ]
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = c('Dystrophia myotonica 1', 'Dystrophia myotonica 2', 'LGMD 2I'))
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    stabel <- 0
  }

  if (valgtVar == 'HjerteAff') {
    tittel <- c('Hjerteaffeksjon')
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData$Variabel <- RegData[, "HjerteAff"]
    gr <- c(0,1,2,9)
    grtxt <- c('Nei', 'Ja', 'Henvist kardiolog', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    stabel <- 0
  }


  if (valgtVar == 'Hjerteoppf') {
    tittel <- c('Hjerteoppfølging')
    RegData <- RegData[RegData$Diagnosegr %in% c(1,2,3), ]
    RegData$Variabel <- RegData[ , "Hjerteoppfoelging"]
    gr <- c(0,1,8,9)
    grtxt <- c('Nei', 'Ja', 'Ikke relevant', 'Ukjent')
    RegData <- RegData[which(RegData$Variabel %in% gr), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    stabel <- 0
  }

  if (valgtVar == 'SympFamilie') {
    tittel <- c('Tilsvarende sykdom/symptomer i familien')
    RegData$Variabel <- RegData[ ,valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ] # Fjerner tomme reg.
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Per pasient, velger nyeste registrering
    gr <- c(0,1,9)
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    # retn='H'
  }

  if (valgtVar == 'RespStotte') {
    tittel <- c('Respirasjonsstøtte')
    RegData$Variabel <- RegData$RespStotte
    RegData <- RegData[!is.na(RegData$Variabel), ] # Fjerner tomme reg.
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Per pasient, velger nyeste registrering
    gr <- c(0,1,9)
    grtxt <- c('Nei', 'Ja', 'Ukjent')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = gr, labels = grtxt)
    # retn='H'
  }

  if (valgtVar == 'TypeHjerteaffeksjon') {
    tittel <- c('Type hjerteaffeksjon')
    RegData <- RegData[which(RegData$HjerteAff==1), ] # Kun for de med hjerteaffekson
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Per pasient, velger nyeste registrering
    AntVar <- colSums(RegData[, c("Kardiomyopati", "Hjertearytmi", "HjerteAffAnnet")])
    N <- dim(RegData)[1]
    NVar<-rep(N, length(AntVar))
    grtxt <- c("Kardiomyopati", "Hjertearytmi", "Annen hjerteaffeksjon")
    retn='H'
  }

  if (valgtVar == 'TypeHjerteaffeksjonSamletDM1_LGMD2I') {
    tittel <- c('Type hjerteaffeksjon')
    RegData <- RegData[which(RegData$HjerteAff==1), ] # Kun for de med hjerteaffekson
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Per pasient, velger nyeste registrering
    RegData$Gr <- NA
    RegData$Gr[which(RegData$Undergruppe %in% c(20))] <- 1
    RegData$Gr[which(RegData$Undergruppe == 4 & RegData$Undergruppe2 == 13)] <- 2
    RegData <- RegData[which(RegData$Gr %in% 1:2), ]
    RegData$Gr <- factor(RegData$Gr, levels = 1:2, labels = c('Dystrophia myotonica 1', 'LGMD 2I'))
    AntVar <- aggregate(RegData[, c("Kardiomyopati", "Hjertearytmi", "HjerteAffAnnet")], by=list(RegData$Gr), sum)
    names(AntVar)[names(AntVar)=="HjerteAffAnnet"]<- "Annen hjerteaffeksjon"
    NVar<-tapply(RegData$Gr, RegData$Gr, length)
    # N <- dim(RegData)[1]
    # NVar<-rep(N, length(AntVar)-1)
    # grtxt <- c("Kardiomyopati", "Hjertearytmi", "HjerteAffAnnet")
    retn='H'
    flerevar <- 1
    stabel <- 0
  }

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
    N_colwise <- T
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
    tittel <- 'Arbeidsuførhet: Limb−girdle muskeldystrofi'
    stabel <- F
    retn='H'
    N_colwise <- T
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
    tittel <- 'Arbeidsuførhet: Dystrophia myotonica type 1'
    stabel <- F
    retn='H'
    N_colwise <- T
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
    tittel <- 'Arbeidsuførhet: Dystrophia myotonica type2 (PROMM)'
    stabel <- F
    retn='H'
    N_colwise <- T
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
    tittel <- 'Arbeidsuførhet: Charcot Marie Tooth'
    stabel <- F
    retn='H'
    N_colwise <- T
  }


  if (valgtVar == 'AndelGenVerifisert') { # per pasient
    grtxt <- c('Ja', 'Nei', 'Ukjent', 'Ikke registrert')
    RegData <- RegData[which(RegData$Diagnosegr %in% c(1,2,3)), ]
    # SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")],   ####### KOMMENTER INN HVIS MAN DET HOLDER AT GENETISK VERIFISERING
    #                          by=list(RegData$PasientID), function(x){if (1 %in% x) {y <- 1} else  ####### NOEN GANG ER REGISTRERT
    #                          {if (0 %in% x) {y <- 0} else {if (9 %in% x) {y <- 9} else {y <- 99}}}})
    # names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    # RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData$VariabelGr <- RegData$GenetiskAarsakPaavist
    RegData$VariabelGr[is.na(RegData$VariabelGr)] <- 99
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = grtxt)
    RegData$Gr <- RegData$Diagnosegr
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati'))
    tittel <- c('Andel pasienter med genetisk verifisert diagnose')
  }


  if (valgtVar == 'AndelGenVerifisertSpes') { # per pasient
    grtxt <- c('Ja', 'Nei', 'Ukjent', 'Ikke registrert')
    RegData <- RegData[which(RegData$Diagnosegr %in% c(1,2,3)), ]
    RegData <- RegData[which(!(RegData$DiagICD10 %in% c('G71.9', 'G12.9', 'G60.9'))), ]
    SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")],
                             by=list(RegData$PasientID), function(x){if (1 %in% x) {y <- 1} else
                             {if (0 %in% x) {y <- 0} else {if (9 %in% x) {y <- 9} else {y <- 99}}}})
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = grtxt)
    RegData$Gr <- RegData$Diagnosegr
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati'))
    tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'av de med spesifikk diagnose')
    # stabel <- 0
  }

  if (valgtVar == 'AndelGenVerifisert_subgr') { # per pasient
    grtxt <- c('Ja', 'Nei', 'Ukjent', 'Ikke registrert')
    RegData$VariabelGr <- RegData$GenetiskAarsakPaavist
    RegData$VariabelGr[is.na(RegData$VariabelGr)] <- 99
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = grtxt)
    RegData$Gr <- NA
    RegData$Gr[which(RegData$Undergruppe %in% c(70,81:83))] <- 1
    RegData$Gr[which(RegData$Undergruppe == 4)] <- 2
    RegData$Gr[which(RegData$DiagICD10 == 'G60.0')] <- 3
    RegData <- RegData[!is.na(RegData$Gr), ]

    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = c('SMA 1-4', 'LGMD', 'CMT'))
    tittel <- c('Andel pasienter med genetisk verifisert diagnose')
  }

  if (valgtVar == 'AndelGenVerifisertSpesUndergr') { # per pasient
    tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'pr. diagnose')
    subtxt <- 'Diagnoser'
    grtxt <- c('Ja', 'Nei', 'Ukjent', 'Ikke registrert')
    RegData <- RegData[which(as.character(RegData$DiagICD10) %in% grtxt), ]
    SamletPrPID <- aggregate(RegData[, c("GenetiskAarsakPaavist")],
                             by=list(RegData$PasientID), function(x){if (1 %in% x) {y <- 1} else
                             {if (0 %in% x) {y <- 0} else {if (9 %in% x) {y <- 9} else {y <- 99}}}})
    names(SamletPrPID)[names(SamletPrPID)=='x'] <- 'VariabelGr'
    RegData <- merge(RegData, SamletPrPID, by.x = 'PasientID', by.y = 'Group.1')
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,9,99), labels = grtxt)
    RegData$Gr <- as.factor(as.character(RegData$DiagICD10))
    # RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = sort(c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G12.0', 'G12.1', 'G60.0')))
  }

  if (valgtVar == 'PsykiskHelsetjeneste_subgr') { # per pasient
    grtxt <- c('Ja', 'Nei', 'Ikke relevant', 'Ukjent')
    RegData$VariabelGr <- RegData$PsykiskHelsetjeneste
    RegData <- RegData[!is.na(RegData$VariabelGr), ]
    # RegData$VariabelGr[is.na(RegData$VariabelGr)] <- 99
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,8,9), labels = grtxt)
    RegData$Gr <- NA
    RegData$Gr[which(RegData$Undergruppe == 20)] <- 1
    RegData$Gr[which(RegData$Undergruppe == 21)] <- 2
    RegData$Gr[which(RegData$Undergruppe == 4)] <- 3
    RegData <- RegData[!is.na(RegData$Gr), ]
    RegData$Gr <- factor(RegData$Gr, levels = 1:3, labels = c('DM1', 'DM2', 'LGMD'))
    tittel <- c('Psykisk helsetjeneste')
  }

  if (valgtVar == 'Kostveiledning_subgr') { # per pasient
    grtxt <- c('Ja', 'Nei', 'Ikke relevant', 'Ukjent')
    RegData <- RegData[which(RegData$Diagnosegr %in% c(1,2,3)), ]
    RegData$VariabelGr <- RegData$TilbudKostveiledning
    RegData <- RegData[!is.na(RegData$VariabelGr), ]
    # RegData$VariabelGr[is.na(RegData$VariabelGr)] <- 99
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    RegData$VariabelGr <- factor(RegData$VariabelGr, levels = c(1,0,8,9), labels = grtxt)
    RegData$Gr <- factor(RegData$Diagnosegr, levels = 1:3, labels = c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati'))
    tittel <- 'Tilbud om kostveiledning'
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

  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, grtxt2=grtxt2, subtxt=subtxt, flerevar=flerevar,
                     retn=retn, cexgr=cexgr, AntVar=AntVar, NVar=NVar, stabel=stabel, incl_N=incl_N, N_colwise=N_colwise)

  return(invisible(PlotParams))


}



