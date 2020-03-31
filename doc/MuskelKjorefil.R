setwd('C:/GIT/muskel/doc/')
library(muskel)
rm(list=ls())

ForlopsData <- read.table('I:/muskel/ForlopsOversikt2018-08-06 11-02-04.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2018-08-06 11-02-02.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak', 'Hjerteoppfoelging',
                        'KognitivSvikt', 'MedikBehandling', 'Smertestillende', "Antiarytmika", "ACEHemmer", "AnnetHjerteMed",
                        "AnnenMedikBeh", "OppfolgBarnelegeNevrolog", "PsykiskHelsetjeneste", "OppholdRehab", "TilbudKostveiledning",
                        "TilbudGenetiskVeiledning", "AnsvarsgruppeIP", "BPA", "Arbeid", "SympFamilie", "TrygdFraAlder", "Kardiomyopati",
                        "Hjertearytmi", "HjerteAffAnnet", "EKG", "HyppighetEKG")] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2018-08-06 11-01-59.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))

RegData <- MuskelPreprosess(RegData)

valgtVar <- 'HyppighetEKG'
incl_N <- T
datoFra='2000-01-01'
datoTil='2099-12-31'
minald=0
maxald=120
erMann=99
outfile=''
# outfile= paste0(valgtVar, '.pdf')
diagnoseSatt= 99#101719
enhetsUtvalg=1
preprosess=F
hentData=F
reshID <- 101719
diagnosegr <- -1
forlop <- 99
diagnose <- -1 #c('G71.0', 'G60.0', 'G73.6')
undergr <- -1
undergr2 <- -1
egenavd <- 0
avdod <- ''
UtredningsaarFra <- 1900
UtredningsaarTil <- 2100
debutAlderFra=0
debutAlderTil=120
UtredningsaarFra=1970
UtredningsaarTil=2009
inkl_tittel=T

if (outfile=='') {x11()}
TabellData <- MuskelFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr, diagnose=diagnose,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop=forlop, egenavd = egenavd,
                 undergr = undergr, undergr2=undergr2, enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData, UtredningsaarFra=UtredningsaarFra,
                 UtredningsaarTil=UtredningsaarTil)



# varNavnStabel <- c('AndelGenVerifisertSpes', 'HoyesteUtdanning',  'HjerteAff_samlet', 'Kostveiledning_subgr')
if (outfile=='') {x11()}
MuskelFigAndelStabel(RegData=RegData, valgtVar='HjerteAff_samlet_2', datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr,
                     minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop=forlop, egenavd=egenavd, diagnose=diagnose,
                     undergr=undergr, undergr2=undergr2, enhetsUtvalg=2, avdod=avdod, preprosess=preprosess, hentData=hentData)




valgtVar <- 'TrygdFraAlder'
if (outfile=='') {x11()}
MuskelFigCumAndel(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr,
                  minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop=forlop, egenavd=egenavd,
                  enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData, debutAlderFra=debutAlderFra,
                  debutAlderTil=debutAlderTil, UtredningsaarFra=UtredningsaarFra, UtredningsaarTil=UtredningsaarTil)

if (outfile=='') {x11()}
MuskelFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr, diagnose=diagnose,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop=forlop, egenavd = egenavd,
                 undergr = undergr, undergr2=undergr2, enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData, UtredningsaarFra=2010,
                 UtredningsaarTil=2018)

enhetsUtvalg=2
if (outfile=='') {x11()}
MuskelFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr, diagnose=diagnose,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop=forlop, egenavd = egenavd,
                 undergr = undergr, undergr2=undergr2, enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

enhetsUtvalg=1
egenavd <- 2
if (outfile=='') {x11()}
MuskelFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr, diagnose=diagnose,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop=forlop, egenavd = egenavd,
                 undergr = undergr, undergr2=undergr2, enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

enhetsUtvalg=2
# egenavd <- 0
if (outfile=='') {x11()}
MuskelFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr, diagnose=diagnose,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop=forlop, egenavd = egenavd,
                 undergr = undergr, undergr2=undergr2, enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

enhetsUtvalg=0
# egenavd <- 0
if (outfile=='') {x11()}
MuskelFigAndeler(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr, diagnose=diagnose,
                 minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, forlop=forlop, egenavd = egenavd,
                 undergr = undergr, undergr2=undergr2, enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)






valgtVar <- 'TidDebUtred'
if (outfile=='') {x11()}
MuskelFigCumAndel(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr,
                  undergr=undergr, minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, diagnoseSatt=diagnoseSatt, forlop=forlop,
                  enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

if (outfile=='') {x11()}
MuskelFigAndelStabel(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr,
                     minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, diagnoseSatt=diagnoseSatt, forlop=forlop,
                     enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

if (outfile=='') {x11()}
MuskelFigKumsum(RegData=RegData, valgtVar=valgtVar, datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr,
                minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, diagnoseSatt=diagnoseSatt, forlop=forlop,
                enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

if (outfile=='') {x11()}
MuskelFigCumAndel_flereShus(RegData=RegData, valgtVar='TidUtredDiag', datoFra=datoFra, datoTil=datoTil, reshID=reshID, diagnosegr=diagnosegr,
                minald=minald, maxald=maxald, erMann=erMann, outfile=outfile, diagnoseSatt=diagnoseSatt, forlop=forlop,
                enhetsUtvalg=enhetsUtvalg, preprosess=preprosess, hentData=hentData)

###### Tilrettelegg kodebok ####################

Klokebok <- read.table('C:/GIT/muskel/doc/Muskelregisteret_klokeboken.csv_20.02.2017.csv', header=TRUE, sep=';',
                       stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
save(Klokebok, file = 'C:/GIT/muskel/data/Klokebok.RData', enc2utf8())

###### Utforsk koder

# Har pasientene med G12.9 andre diagnoser?
table(RegData$DiagICD10[RegData$PasientID %in% RegData$PasientID[RegData$DiagICD10=='G12.9']], useNA = 'ifany')

# Hva er registrert i Undergruppe 1020 "Annen" under G71.1?
table(as.character(RegData$Undergruppe2Spes[RegData$Undergruppe==1020 & RegData$DiagICD10=='G71.1']), useNA = 'ifany')
# Ingenting

table(RegData$Undergruppe[RegData$DiagICD10=='G71.2'])

Uttrekk <- RegData[which(RegData$Undergruppe2==13 & RegData$Avdod==0), ]

kontroll <- RegData[which(RegData$PasientID %in% unique(Uttrekk$PasientID)), ]

kontroll[kontroll$PasientID== kontroll$PasientID[which(kontroll$Undergruppe2 != 13)], ]

Uttrekk <- Uttrekk[match(unique(Uttrekk$PasientID), Uttrekk$PasientID), c('PasientID', 'ForlopsID', 'HovedDato')]


tmp2 <- RegData[which(RegData$Undergruppe==1030), ]


write.csv2(Uttrekk, 'FKRP.csv', row.names = F)


### Diverse uttrekk

uttrekk <- RegData[which(RegData$Undergruppe==20 | RegData$Undergruppe2 == 13 | RegData$Undergruppe==1), ]
uttrekk <- uttrekk[which(uttrekk$HjerteAffAnnet==1), ]

uttrekk[, c("HovedDato", "Undergruppe_label", "Undergruppe2_label", "HjerteAffAnnetSpes")]






# tmp <- table(RegData$RegInst_label, RegData$DiagICD10)
# x11()
#
#
# # png(file = 'tabell.png', res=72/8, family='sans')
# png(filename = "tabell.png",
#     width = 480, units = "px", pointsize = 10,
#     bg = "white")
#
# pdf(file = 'tabell.pdf', width=3*7, height=7)
#
# png(file = 'tabell.png', width=4000, height=2000, pointsize = 1)
# # par('fig'=c(0,1,0,2))
# gplots::textplot(tmp, halign='left')
# dev.off()
#
#
#
#
# RegData$RegInst[which(RegData$RegInst %in% c(NA, 99))] <- 0
# RegData$RegInstAndre <- RegData$RegInstAndre + 13
# RegData$RegInstAndre[which(RegData$RegInstAndre %in% c(NA, 112))] <- 0
# RegData$RegInst[RegData$RegInst == 0] <- RegData$RegInstAndre[RegData$RegInst == 0]
#
#
#
# RegData <- RegData[RegData$InnDato < as.POSIXlt('2015-01-01'), ]
# table(RegData$SykehusNavn)
#
#
#
# gmldata <- read.table('C:/SVN/jasper/Muskel/data/all_data_muskel2016-02-12 08-43-49.txt', header=TRUE, sep=';')
#
#
# RegData <- MuskelPreprosess(RegData=RegData)
# diagnosetabell <- table(RegData$PasientID, RegData$Diagnosegr, useNA = 'ifany')
# diagnosetabell <- table(RegData$PasientID, RegData$DiagICD10, useNA = 'ifany')
# diagnosetabell[which(diagnosetabell>1)] <- 1
# flerdiagPID <- as.numeric(names(rowSums(diagnosetabell)[rowSums(diagnosetabell)>1]))
#
# TilSynnove <- RegData[RegData$PasientID %in% flerdiagPID, ]  #RegData[RegData$PasientID %in% flerdiagPID, c("PasientID", "GenetiskAarsakPaavist", "Diagnosegr")]
#
# write.csv2(TilSynnove, 'Avvik.csv', row.names = F)
#
#
# ############################################################
# ### Figur over kummulativ antall registreringer i registeret
# ############################################################
#
# setwd('C:/GIT/muskel/doc/')
# rm(list=ls())
#
# ForlopsData <- read.table('C:/SVN/jasper/Muskel/data/ForlopsOversikt2016-07-28 10-41-35.txt', header=TRUE, sep=';')
# ForlopsData <- ForlopsData[ForlopsData$BasisRegStatus==1 & ForlopsData$ForlopsType1Num==1, ]
# utvalgTxt <- paste('Registrert: ', min(as.POSIXlt(ForlopsData$HovedDato), na.rm=T), ' til ',
#                    max(as.POSIXlt(ForlopsData$HovedDato), na.rm=T), sep='' )
# ForlopsData$Aar <- as.POSIXlt(ForlopsData$HovedDato)
# ForlopsData$Aar <- ForlopsData$Aar$year + 1900
# Andeler <- cumsum(table(ForlopsData$Aar, useNA = 'ifany'))
# outfile <- 'AntReg.png'
# cexgr <- 0.8
# par('fig'=c(0, 1, 0, 1-0.02*(length(utvalgTxt)-1)))
#
# if (outfile=='') {x11()}
# FigTypUt <- figtype(outfile=outfile, fargepalett='BlaaRapp', pointsizePDF=12)
# farger <- FigTypUt$farger
# ymax <- max(Andeler, na.rm=T)*1.25
# pos <- barplot(as.numeric(Andeler), beside=TRUE, las=1, ylab="Antall",	#main=tittel,
#                #sub=subtxt,	#names.arg=grtxt, cex.names=cexgr,
#                col=farger[1], border='white', ylim=c(0, ymax))
# text(pos, Andeler+max(Andeler)/50, labels = Andeler, cex=cexgr)
# mtext(at=pos, names(Andeler), side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
# title(main = paste0('Antall registrerte per ', Sys.Date()), line=1, font.main=1, cex.main=1.3*cexgr)
# mtext(utvalgTxt, side=3, las=1, cex=.9*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))
# par('fig'=c(0, 1, 0, 1))
# if ( outfile != '') {dev.off()}
