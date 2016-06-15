#'S' - Vertikale eller hor. søyler
#'L' - Linjer

#Søylediagram som viser fordeling av valgt variabel:

#Inngangsdata:
#	RegData - ei dataramme med alle nødvendige variable fra registeret
#	libkat - sti til bibliotekkatalog
#   outfile - navn på fil figuren skrives ned til
#	egenReshID - avdelingsid for egen avdeling, standard: 0-hele landet
# 	Brukerstyrt i Jasper:
#		valgtVar - Må velges: ...
#		erMann - kjønn, 1-menn, 0-kvinner, standard: '' (alt annet enn 0 og 1), dvs. begge
#		minald - alder, fra og med
#		maxald - alder, til og med
#		datoFra <- '2010-01-01'    # min og max dato i utvalget vises alltid i figuren.
#		datoTil <- '2013-05-25'
#		sml - Sammenligne med resten av landet, standard (1) eller ikke(0). Ikke aktiv når hele landet valgt.
#			(Skipper inputktr for denne)
#		Ikke med: avd - 0: data fra hele landet, 1: data fra egen avdeling (standard)
#		plotType='S' - Velge mellom søyler 'S' og linjer 'L' - ikke aktivert enda



#Trenger funksjonene...:
#	fun_FigFilType.R



FigFordeling <- function(RegData, libkat, outfile='', valgtVar, datoFra='2000-01-01', datoTil='2050-01-01',
		minald=0, maxald=120, erMann='', plotType='S', diagnoseSatt='')
{


#------------Gjøre utvalg-------------------------
#Definerer registerspesifikke variable................
#alderVar <- 'Alder'
datoVar <- 'FILL_DATE'
kjVar <- 'erMann'
RegData$kjVar <- 0
RegData$kjVar[RegData$Kjoenn == 'M'] <- 1
#RegData$AdmitDt <- as.POSIXlt(RegData$AdmitDt, format="%Y-%m-%d")
#RegData$InnDato <- as.POSIXlt(RegData[ ,datoVar], format="%Y-%m-%d") 	#%H:%M:%S" )	#"%d.%m.%Y"	"%Y-%m-%d"
RegData$InnDato <- as.POSIXlt(RegData[ ,datoVar], format="%Y-%m-%d")	#"%d.%m.%Y %H:%M:%S" )	#"%d.%m.%Y"
RegData$Alder <- round(as.numeric(difftime(Sys.Date(),
		strptime(RegData$BIRTH_DATE, format="%Y-%m-%d" ))/365.25),1)


if (valgtVar %in% c('Alder', 'Kjonn'))	{
	RegData$Variabel <- RegData[ ,valgtVar] }

if (valgtVar %in% c('Diagnosegr', 'GenHovedgr', 'GenUndergr'))	{
	RegData$Diagnosegr <- 'Ikke registrert'
	RegData$Diagnosegr[RegData$DIAGNOSIS %in%
		c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G71.8', 'G71.9', 'G72.3', 'G73.6')] <- 'Muskelsykdommer'
	RegData$Diagnosegr[RegData$DIAGNOSIS %in% c('G12.0', 'G12.1', 'G12.8', 'G12.9')] <- 'Spinal muskelatrofi'
	RegData$Diagnosegr[RegData$DIAGNOSIS %in% c('G60.0', 'G60.1', 'G60.8', 'G60.9')] <- 'Polynevropati'
	RegData <- RegData[which(RegData$Diagnosegr != 'Ikke registrert'),]
	RegData$Variabel <- RegData$Diagnosegr
	}


if (valgtVar == 'Fysio') {
	RegData$Variabel <- RegData$PHYSIOTHERAPY
	#Valg: -, 1 - Ja, 0 - Nei, 2 - Ikke behov, 9 - Ukjent
	}

if (valgtVar == 'TidDebDiag') {
RegData$Debut <- as.POSIXlt(RegData$BIRTH_DATE, format="%Y-%m-%d")$year+1900 + RegData$DEBUT_AGE
RegData$Variabel <- RegData$DIAGNOSIS_YEAR - RegData$Debut
	}

if (valgtVar == 'TidDebUtred') {
RegData$Debut <- as.POSIXlt(RegData$BIRTH_DATE, format="%Y-%m-%d")$year+1900 + RegData$DEBUT_AGE
RegData$Variabel <- RegData$ASSESSMENT_START - RegData$Debut
	}

if (valgtVar == 'TidUtredDiag') {
RegData$Variabel <- RegData$DIAGNOSIS_YEAR - RegData$ASSESSMENT_START
	}

if (valgtVar == 'BoHF') {
RegData$Variabel <- RegData$BoHFnavn
	}

#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
Ninn <- dim(RegData)[1]
indMed <- 1:Ninn

indVarMed <- intersect(intersect(intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != 'NaN')), which(RegData$Variabel != '')),
                       which(!is.na(RegData$Variabel)))
indAldUt <- which(RegData$AlderAar < minald | RegData$AlderAar > maxald)
indDatoUt <- which(RegData$InnDato < as.POSIXlt(datoFra) | RegData$InnDato > as.POSIXlt(datoTil))
#indKjUt <- switch(as.character(erMann), '0'=which(RegData$isMale==0), '1'=which(RegData$isMale==0))		#erMann != 0:1, which(RegData$erMann
indKjUt <- if (erMann %in% 0:1) {which(RegData$kjVar != erMann)} else {indKjUt <- NULL}
indDiagSattUt <- if (diagnoseSatt %in% c(1:13, 99)) {which(RegData$PRIMARY_DIAG_INST != diagnoseSatt)} else {indDiagSattUt <- NULL}
indMed <- intersect(setdiff(1:Ninn, c(indAldUt, indDatoUt, indKjUt, indDiagSattUt)),
			indVarMed)
RegData <- RegData[indMed,]
N <- dim(RegData)[1]

# if (valgtVar %in% c('GenUndergr', 'GenHovedgr'))  {
#   RegData$Variabel <- RegData$GENETIC_CAUSE
# }


utvalgTxt <- c(paste('Registreringsperiode: ',
	min(RegData$InnDato), ' til ', max(RegData$InnDato), sep='' ),
	if ((minald>0) | (maxald<120)) {
		paste('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år', sep='')},
	if (erMann %in% 0:1){paste('Kjønn: ', c('kvinner', 'menn')[erMann+1], sep='')},
	if (diagnoseSatt %in% c(1:16, 99)){paste0('Først diagnostisert: ', switch(as.character(diagnoseSatt),
	                                                                          '1' = 'Akershus Universitetssykehus',
	                                                                          '2' = 'Haukeland Universitetssykehus',
	                                                                          '3' = 'Nordlandssykehuset Bodø',
	                                                                          '4' = 'Oslo Universitetssykehus',
	                                                                          '5' = 'Oslo Universitetssykehus, Rikshospitalet',
	                                                                          '6' = 'St. Olavs Hospital',
	                                                                          '7' = 'Stavanger Universitetssykehus',
	                                                                          '8' = 'Sykehuset Innlandet, Lillehammer',
	                                                                          '9' = 'Sykehuset Telemark, Skien',
	                                                                          '10' = 'Sykehuset Vestfold, Tønsberg',
	                                                                          '11' = 'Sykehuset Østfold, Fredrikstad',
	                                                                          '12' = 'Sørlandet sykehus, Kristiansand',
	                                                                          '13' = 'Universitetssykehuset Nord Norge, Tromsø',
	                                                                          '99' = 'Andre')
	)}
  )

#-----------Må ha en del som er registerspesifikk, så må selve plottet være i pakken, dvs. funksjoner.
cexgr <- 0.9
retn <- 'V'
txtretn <- 1
grtxt <- ''
grtxt2 <- ''
undertxt <- ''	#'Tall på søylene angir antall registreringer i gruppa'
topsoyletxt <- ''
type <- 'andel'

if (valgtVar=='Alder') {
	tittel <- 'Aldersfordeling i registeret'	#bør ha med at alder per rapportdata sys.date...
	gr <- c(0, seq(10, 80, 10), 120)	#c(0,16,31,46,61,76,200)
	RegData$VariabelGr <- cut(RegData$Variabel, breaks=gr, include.lowest=TRUE, right=FALSE)
	grtxt <- c(levels(RegData$VariabelGr)[1:(length(gr)-2)], '80+')
	cexgr <- 0.8
	subtxt <- 'Aldersgrupper'
	Ngr <- table(RegData$VariabelGr)
#	topsoyletxt <- as.numeric(Ngr)
	Andeler <- round(Ngr/N*100,2)
}

if (valgtVar %in% c('TidDebDiag', 'TidDebUtred', 'TidUtredDiag')) {
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
	cexgr <- 0.8
	subtxt <- 'Antall år'
	Ngr <- table(RegData$VariabelGr)
#	topsoyletxt <- as.numeric(Ngr)
	Andeler <- round(Ngr/N*100,2)
}

if (valgtVar == 'Diagnosegr') {
	tittel <- 'Fordeling av diagnosegrupper'
	#gr <- (1:10,99) - Kodene som registereres
	grtxt <- levels(as.factor(RegData$Diagnosegr))	#c('Muskelsykdommer', 'Spinal muskelatrofi', 'Polynevropati')
	subtxt <- 'Diagnosegrupper'
	Ngr <- table(RegData$Diagnosegr)
#	topsoyletxt <- as.numeric(Ngr)
	Andeler <- round(Ngr/N*100,2)
}

if (valgtVar == 'GenHovedgr') {
#1.	Andelen med genetisk verifisert diagnose i de 3 gruppene Muskelsykdommer Spinal muskelatrofi
# Polynevropati,
	tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'av de med spesifikk diagnostisk undergruppe')
	subtxt <- 'Diagnosegrupper'
	#Telle opp GENETIC_CAUSE == 1 for hver gruppe. Vil ha andel av alle.
	Ngr <- table(RegData$Diagnosegr)
	Ngen <- table(as.factor(RegData$Diagnosegr)[which(RegData$GENETIC_CAUSE==1)])
	topsoyletxt <- paste('N=',Ngr, sep='')	#paste(Ngen,'/',Ngr, sep='')
	Andeler <- Ngen/Ngr*100
	grtxt <- names(Andeler)
}

if (valgtVar == 'GenUndergr') {
	#... og for følgende undergrupper: G71.0, G71.1, G71.2, G71.3, G12.0, G12.1, G60.0
	tittel <- c('Andel pasienter med genetisk verifisert diagnose', 'av de med spesifikk diagnostisk undergruppe')
	subtxt <- 'Diagnoser'
	grtxt <- sort(c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G12.0', 'G12.1', 'G60.0'))
	indUndergr <- which(as.character(RegData$DIAGNOSIS) %in% grtxt)
	RegData$Undergr <- 'dum'
	RegData$Undergr[indUndergr] <- as.character(RegData$DIAGNOSIS[indUndergr])
	RegData <- RegData[which(RegData$Undergr != 'dum'), ]	#RegData[-which(RegData$Undergr == 'dum'), ]
	RegData$Undergr <- as.factor(RegData$Undergr)
		#RegData <- RegData[grtxt, ]
#Telle opp GENETIC_CAUSE == 1 for hver gruppe. Vil ha andel av alle.
	Ngr <- table(RegData$Undergr)
	Ngen <- table(RegData$Undergr[which(RegData$GENETIC_CAUSE==1)])
	topsoyletxt <- paste('N=',Ngr, sep='')	#paste(Ngen,'/',Ngr, sep='')
	Andeler <- Ngen/Ngr*100
  grtxt <- names(Andeler)
}

if (valgtVar == 'BoHF') {
	retn <- 'H'
	type <- 'ant'
	tittel <- 'Hvilke HF-områder kommer pasientene fra?'
	subtxt <- 'BoHF'
	Ngr <- sort(table(RegData$Variabel))
	Ngr <- Ngr[names(Ngr) != '']
	grtxt <- names(Ngr)
	topsoyletxt <- as.numeric(Ngr)
	Andeler <- Ngr	#round(Ngr/N*100,2)
}



#.....Må få med denne:
if (retn == 100){
#(dim(RegData)[1]==0 | (NSh==0 & egenavd==1)) {
	#-----------Figur---------------------------------------
#figtype(outfile)
	plot.new()
	title(main=tittel)
	text(0.5, 0.7, 'Ingen egne data registrert for dette utvalget',cex=1.3)
	if ( outfile != '') {dev.off()}
} else {

#-----------Figur---------------------------------------
#Inn parametre: subtxt, grtxt, grtxt2, tittel, Andeler

source(paste(libkat, 'LibFigFilType.R', sep=''), encoding="UTF-8")
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger

#Plottspesifikke parametre:
#farger <- hsv(7/12, c(1,0.55,0.35,0.1,0), v=c(0.62,0.75,0.9,0.95,1))
#source(paste(libkat, 'fun_FigFilType.R', sep=''), encoding="UTF-8")
#figtype(outfile)

plotType <- 'S'	#'L'-linje, 'S'-søyle. Søyler er standard.
antGr <- length(grtxt)


if (retn == 'V' ) {
#Vertikale søyler eller linje
	par('fig'=c(0, 1, 0, 0.9))
	ymax <- min(max(Andeler, na.rm=T)*1.25, 110)
	pos <- barplot(as.numeric(Andeler), beside=TRUE, las=txtretn, ylab="Andel (%)",	#main=tittel,
		#sub=subtxt,	#names.arg=grtxt, cex.names=cexgr,
		col=farger[1], border='white', ylim=c(0, ymax))	#farger[c(1,3)]
	mtext(at=pos, grtxt, side=1, las=1, cex=0.8, adj=0.5, line=0.5)
	mtext(at=pos, grtxt2, side=1, las=txtretn, cex=0.8, adj=0.5, line=1.5)
#	text(pos, Andeler+0.015*ymax, topsoyletxt, cex=0.75)
	text(pos, 0, pos=3, topsoyletxt, cex=0.75, col=farger[4])
	mtext(at=min(pos)-0.5, cex=0.8, side=1, line=3, adj=0, undertxt)
	mtext(side=1, line=2, adj=0.5, subtxt)
}

if (retn == 'H') {
#Horisontale søyler
	ymax <- antGr*1.4
	xmax <- switch(type, andel=min(max(Andeler,na.rm=T)*1.25, 100), ant=max(Andeler)*1.1)
	par('fig'=c(0.15, 1, 0, 0.9))
	pos <- barplot(rev(as.numeric(Andeler)), horiz=TRUE, beside=TRUE, las=1, #xlab="Andel(%)", #main=tittel,
		col=farger[1], border='white', font.main=1, xlim=c(0, xmax), ylim=c(0,ymax))	#
#	text(rev(Andeler)+0.015*ymax, pos+0, rev(as.numeric(Ngr)), cex=0.75)
	mtext(at=pos+0.15, rev(grtxt), side=2, las=1, cex=0.8, adj=1, line=0.25)
	mtext(side=1, line=2, adj=0.5, switch(type, andel='Andel (%)', ant='Antall registrerte'))
#	mtext(at=min(pos)-0.5, cex=0.8, side=1, line=3, adj=0, 'Tall ved søylene angir antall registreringer')
}
legend('top', paste('Totalt antall registreringer: ', N, sep=''),
		border=NA, fill='white', bty='n', ncol=1, cex=0.9)

utvpos <- 5.2
avst <- 0.8
krymp <- 0.8
title(tittel, font.main=1)	#line=1,
mtext(utvalgTxt[1], side=3, las=1, cex=krymp, adj=0, line=utvpos, col=farger[1])
mtext(utvalgTxt[2], side=3, las=1, cex=krymp, adj=0, line=utvpos-avst, col=farger[1])
mtext(utvalgTxt[3], side=3, las=1, cex=krymp, adj=0, line=utvpos-2*avst, col=farger[1])
mtext(utvalgTxt[4], side=3, las=1, cex=krymp, adj=0, line=utvpos-3*avst, col=farger[1])
mtext(utvalgTxt[5], side=3, las=1, cex=krymp, adj=0, line=utvpos-4*avst, col=farger[1])

par('fig'=c(0, 1, 0, 1))
if ( outfile != '') {dev.off()}

}
}
