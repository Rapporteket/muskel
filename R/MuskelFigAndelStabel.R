FigAndelStabel<- function(RegData, libkat, outfile='', valgtVar, datoFra='2000-01-01', datoTil='2050-01-01',
		minald=0, maxald=120, erMann='', diagnoseSatt='')
{

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



#Trenger funksjonene...:
#	fun_FigFilType.R

#------------Gjøre utvalg-------------------------
#Definerer registerspesifikke variable................
datoVar <- 'FILL_DATE'
kjVar <- 'erMann'
RegData$kjVar <- 0
RegData$kjVar[RegData$Kjoenn == 'M'] <- 1
#RegData$AdmitDt <- as.POSIXlt(RegData$AdmitDt, format="%Y-%m-%d")
#RegData$InnDato <- as.POSIXlt(RegData[ ,datoVar], format="%Y-%m-%d") 	#%H:%M:%S" )	#"%d.%m.%Y"	"%Y-%m-%d"
RegData$InnDato <- as.POSIXlt(RegData[ ,datoVar], format="%Y-%m-%d" )	#"%d.%m.%Y"	"%Y-%m-%d"
RegData$Alder <- round(as.numeric(difftime(Sys.Date(),
		strptime(RegData$BIRTH_DATE, format="%Y-%m-%d" ))/365.25),1)


if (valgtVar %in% c('Diagnosegr', 'GenHovedgr', 'Fysio', 'Ergo'))	{
	RegData$Diagnosegr <- 'Ikke registrert'
	RegData$Diagnosegr[RegData$DIAGNOSIS %in%
		c('G71.0', 'G71.1', 'G71.2', 'G71.3', 'G71.8', 'G71.9', 'G72.3', 'G73.6')] <- 'Muskelsykdommer'
	RegData$Diagnosegr[RegData$DIAGNOSIS %in% c('G12.0', 'G12.1', 'G12.8', 'G12.9')] <- 'Spinal muskelatrofi'
	RegData$Diagnosegr[RegData$DIAGNOSIS %in% c('G60.0', 'G60.1', 'G60.8', 'G60.9')] <- 'Polynevropati'
	RegData <- RegData[which(RegData$Diagnosegr != 'Ikke registrert'),]
	RegData$Gr <- RegData$Diagnosegr
	}

if (valgtVar == 'Fysio') {
#Defineres her bare for å få bort manglende registreringer
	RegData$Variabel <- RegData$PHYSIOTHERAPY
	#Valg: -, 1 - Ja, 0 - Nei, 2 - Ikke behov, 9 - Ukjent
}

if (valgtVar == 'Ergo') {
#Defineres her bare for å få bort manglende registreringer
	RegData$Variabel <- RegData$ERGOTHERAPY
	#Valg: -, 1 - Ja, 0 - Nei, 2 - Ikke behov, 9 - Ukjent
}

#Tar ut de med manglende registrering av valgt variabel og gjør utvalg
Ninn <- dim(RegData)[1]
indMed <- 1:Ninn

indVarMed <- intersect(intersect(intersect(which(RegData$Variabel != 'NA'), which(RegData$Variabel != 'NaN')),
                                 which(RegData$Variabel != '')), which(!is.na(RegData$Variabel)))
indAldUt <- which(RegData$AlderAar < minald | RegData$AlderAar > maxald)
indDatoUt <- which(RegData$InnDato < as.POSIXlt(datoFra) | RegData$InnDato > as.POSIXlt(datoTil))
indDiagSattUt <- if (diagnoseSatt %in% c(1:13, 99)) {which(RegData$PRIMARY_DIAG_INST != diagnoseSatt)} else {indDiagSattUt <- NULL}
#indKjUt <- switch(as.character(erMann), '0'=which(RegData$isMale==0), '1'=which(RegData$isMale==0))		#erMann != 0:1, which(RegData$erMann
indKjUt <- if (erMann %in% 0:1) {which(RegData$kjVar != erMann)} else {indKjUt <- NULL}
indMed <- intersect(setdiff(1:Ninn, c(indAldUt, indDatoUt, indKjUt, indDiagSattUt)),
			indVarMed)
RegData <- RegData[indMed,]
N <- dim(RegData)[1]

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
Nutv <- N


if (valgtVar=='Nytte') {
	t1 <- 'Hvilken nytte mener du at du har hatt av operasjonen?'
		RegData$Var <- RegData$Nytte
		RegData$Var[which(RegData$Nytte %in% 1:2)] <- 1	#Bedre
		RegData$Var[which(RegData$Nytte %in% 3:5)] <- 2	#Omtrent uendret
		RegData$Var[which(RegData$Nytte %in% 6:7)] <- 3	#Verre
		grtxt <- c('Frisk/mye bedre', 'Omtrent uendret', 'Klart verre')
	}

if (valgtVar == 'Fysio') {
	tittel <- 'Andel som får fysioterapi'
	#Valg: -, 0 - Nei, 1 - Ja, 2 - Ikke behov, 9 - Ukjent
	#Endrer Ja til -1
	RegData$PHYSIOTHERAPY[RegData$PHYSIOTHERAPY==1] <- -1
	RegData$Variabel <- factor(RegData$PHYSIOTHERAPY, labels = c('Ja', 'Nei', 'Ikke behov', 'Ukjent'))
	}

if (valgtVar == 'Ergo') {
	tittel <- 'Andel som får ergoterapi'
	#Valg: -, 0 - Nei, 1 - Ja, 2 - Ikke behov, 9 - Ukjent
	#Endrer Ja til -1
	RegData$ERGOTHERAPY[RegData$ERGOTHERAPY==1] <- -1
	RegData$Variabel <- factor(RegData$ERGOTHERAPY, labels = c('Ja', 'Nei', 'Ikke behov', 'Ukjent'))
	}

#-------------------------Beregninger-----------------------------------------
RegData$Gr <- factor(RegData$Gr)	#, levels = min(as.numeric(RegData$Gr)):max(as.numeric(RegData$Gr)))
grtxt <- levels(factor(RegData$Gr))
stabeltxt <- levels(RegData$Variabel)
#grtxt <- attr(AndelVar, 'row.vars')
#RegData$Var <- factor(RegData$Var)
NVarGr <- ftable(RegData[ , c('Variabel','Gr')])	#ftable(list(RegData$Var, RegData$Gr))
NGr <- colSums(NVarGr)
AndelVar <- prop.table(NVarGr,2)*100


#-----------Figur---------------------------------------

source(paste(libkat, 'LibFigFilType.R', sep=''), encoding="UTF-8")
FigTypUt <- figtype(outfile)
farger <- FigTypUt$farger
#source(paste(libkat, 'fun_FigFilType.R', sep=''), encoding="UTF-8")
#farger <- c(hsv(7/12, c(1,0.55,0.35,0.1), v=c(0.62,0.75,0.9,0.95)))	#rev(
#figtype(outfile)

par('fig'=c(0, 1, 0, 0.9))

koord <- barplot(AndelVar, beside=F, las=1, #names.arg=grtxt, cex.names=0.95,
        col=farger, ylab="Andel (%)", ylim=c(0,132),	 #xlim=c(0, length(grtxt)*1.2),
	cex.main=1, font.main=1, axes=F, cex.axis=.9, cex.lab=.95, space=.25, border=NA)
axis(side=2, at=c(0,20,40,60,80,100))
legend('top', legend=rev(stabeltxt), bty='n', cex=.8, 	#max(koord+0.5)*1.35, y=80,
		xjust=0.5, fill=farger[length(stabeltxt):1], border=farger[length(stabeltxt):1], ncol=1)
mtext(at=koord, cex=0.9, side=1, line=0, adj=0.5, grtxt)	#
text(koord, 102.7, paste('N=',NGr,sep=''), cex=0.75)
#mtext(at=min(koord)-0.5, cex=0.8, side=1, line=2, adj=0, 'Tall over søylene angir antall registreringer')

utvpos <- 5.2
avst <- 0.8
krymp <- 0.8
title(tittel, line=1, font.main=1)
mtext(utvalgTxt[1], side=3, las=1, cex=krymp, adj=0, line=utvpos, col=farger[1])
mtext(utvalgTxt[2], side=3, las=1, cex=krymp, adj=0, line=utvpos-avst, col=farger[1])
mtext(utvalgTxt[3], side=3, las=1, cex=krymp, adj=0, line=utvpos-2*avst, col=farger[1])
mtext(utvalgTxt[4], side=3, las=1, cex=krymp, adj=0, line=utvpos-3*avst, col=farger[1])
mtext(utvalgTxt[5], side=3, las=1, cex=krymp, adj=0, line=utvpos-4*avst, col=farger[1])
par('fig'=c(0, 1, 0, 1))

if ( outfile != '') {dev.off()}


}

