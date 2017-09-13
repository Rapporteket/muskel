#' Lag søylediagram eller som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel skal plottes
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren). Tekststreng skrevet som 'YYYY-mm-dd'
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren). Tekststreng skrevet som 'YYYY-mm-dd'
#' @param minald Alder, fra og med (Default: 0)
#' @param maxald Alder, til og med (Default: 130)
#' @param erMann kjønn
#'                 1: menn
#'                 0: kvinner
#'                 99: begge (Default)
#' @param outfile Navn på fil figuren skrives til. Default: '' (Figur skrives
#'    til systemets default output device (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param diagnoseSatt Hvor ble pasienten opprinnelig utredet
#' @param diagnosegr Diagnosegruppe
#'                 1: Muskelsykdommer
#'                 2: Spinal muskelatrofi
#'                 3: Polynevropati
#' @param forlop Forløpstype
#'                 1: Basisregistrering
#'                 2: Oppfølging
#'                 3: Annet
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Default)
#'                 2: Egen enhet
#' @param preprosess Preprosesser data
#'                 FALSE: Nei (Default)
#'                 TRUE: Ja
#' @param hentData Gjør spørring mot database
#'                 FALSE: Nei, RegData gis som input til funksjonen (Default)
#'                 TRUE: Ja
#'
#' @return En figur med søylediagram av ønsket variabel
#'
#' @export

MuskelFigAndeler <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID, diagnosegr='',
                             minald=0, maxald=120, erMann=99, outfile='', diagnoseSatt=99, forlop = 99,
                             undergr='', undergr2='', enhetsUtvalg=1, preprosess=F, hentData=F)
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

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  MuskelUtvalg <- MuskelUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, forlop = forlop,
                               maxald=maxald, erMann=erMann, diagnosegr=diagnosegr, diagnoseSatt=diagnoseSatt,
                               undergr=undergr, undergr2=undergr2)
  RegData <- MuskelUtvalg$RegData
  utvalgTxt <- MuskelUtvalg$utvalgTxt

  # Initialiserer nødvendige størrelser
  Andeler <- list(Hoved = 0, Rest =0)
  ind <- list(Hoved=which(RegData$AvdRESH == reshID), Rest=which(RegData$AvdRESH != reshID))
  Nrest <- 0

  if (valgtVar %in% c('AndelGenVerifisert', 'DiagByggerPaa', 'DiagGenVerifisert',
                      'KonkrUndGrDuchBeck', 'AndelSteroider')) {
    flerevar <- 1
  } else {
    flerevar <- 0
  }

  if (flerevar == 0 ) {
    ## Forbered variabler for fremstilling i figur
    PlotParams <- MuskelPrepVar(RegData=RegData, valgtVar=valgtVar)
    RegData <- PlotParams$RegData
    PlotParams$RegData <- NA
    if (enhetsUtvalg==1) {
      AntHoved <- table(RegData$VariabelGr[ind$Hoved])
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
      AntRest <- table(RegData$VariabelGr[ind$Rest])
      Nrest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*AntRest/Nrest
    } else {
      AntHoved <- table(RegData$VariabelGr)
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
    }
  }

  if (flerevar == 1){

    if (enhetsUtvalg==1) {
      PlotParams <- MuskelPrepVar(RegData[ind$Hoved, ], valgtVar) # Hovegruppe
      AntHoved <- PlotParams$AntVar
      NHoved <- max(PlotParams$NVar, na.rm=T)
      Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar
      PlotParams2 <- MuskelPrepVar(RegData[ind$Rest, ], valgtVar) # Sammenligningsgruppe
      AntRest <- PlotParams2$AntVar
      NRest <- max(PlotParams2$NVar,na.rm=T)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*PlotParams2$AntVar/PlotParams2$NVar
      rm(PlotParams2)
    } else {
      PlotParams <- MuskelPrepVar(RegData, valgtVar)
      AntHoved <- PlotParams$AntVar
      NHoved <- max(PlotParams$NVar, na.rm=T)
      if (valgtVar=='AndelSteroider') {
        NHoved <- sum(PlotParams$NVar, na.rm=T)
      }
      Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar
    }
  }   #end sjekk om figuren inneholder flere variable







  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; #grtxt2 <- PlotParams$grtxt2;
  subtxt <- PlotParams$subtxt; retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;
  FigTypUt <- figtype(outfile=outfile, fargepalett=MuskelUtvalg$fargepalett, pointsizePDF=12)

  #Hvis for få observasjoner..
  if (NHoved < 5 | (Nrest<5 & enhetsUtvalg==1)) {
    farger <- FigTypUt$farger
    plot.new()
    # title(tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
    #Plottspesifikke parametre:

    farger <- FigTypUt$farger
    NutvTxt <- length(utvalgTxt)
    grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f', Andeler$Hoved)), '%)', sep='')
    grtxt2 <- paste(sprintf('%.1f',Andeler$Hoved), '%', sep='')
    # if (incl_pst) {grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f', Andeler$Hoved)), '%)', sep='')}
    if (PlotParams$incl_N) {
      grtxtpst <- paste(rev(grtxt), ' (n=', rev(sprintf('%.0f', AntHoved)), ')', sep='')  #################  AD-HOC, farlig
      grtxt2 <- paste('n=', sprintf('%.0f',Andeler$Hoved*NHoved/100), sep='')             #################
      }
    vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med

    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	# Størrelse på legendmarkør?

    if (retn == 'V' ) {
      #Vertikale søyler
      ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
      ylabel <- "Andel pasienter"
      pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab=ylabel,  #main=tittel,
                     sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, #names.arg=grtxt, cex.names=cexgr,
                     col=fargeHoved, border='white', ylim=c(0, ymax))	#farger[c(1,3)]
      mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
      if (enhetsUtvalg == 1) {
        points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
               lwd=lwdRest, ncol=1, cex=cexgr)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
      }
    }


    if (retn == 'H') {
      #Horisontale søyler
      ymax <- antGr*1.4
      xmax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
      xlabel <- "Andel pasienter (%)"

      pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab=xlabel, #main=tittel,
                     col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
      mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)
      if (PlotParams$N_colwise) {
        if (flerevar == 1) {
          text(x=rev(as.numeric(Andeler$Hoved)), y=pos+0.05, labels = rev(PlotParams$NVar), pos=4)
        }
        else {
          text(x=rev(as.numeric(Andeler$Hoved)), y=pos+0.05, labels = paste0('n=', rev(sprintf('%.0f', Andeler$Hoved*NHoved/100))), pos=4)
        }
      }

      if (enhetsUtvalg == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexgr)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
      }
    }


    krymp <- .9
    title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
    mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

    par('fig'=c(0, 1, 0, 1))

    if ( outfile != '') {dev.off()}

  }

}


#
# #.....Må få med denne:
# if (retn == 100){
# #(dim(RegData)[1]==0 | (NSh==0 & egenavd==1)) {
# 	#-----------Figur---------------------------------------
# #figtype(outfile)
# 	plot.new()
# 	title(main=tittel)
# 	text(0.5, 0.7, 'Ingen egne data registrert for dette utvalget',cex=1.3)
# 	if ( outfile != '') {dev.off()}
# } else {
#
# #-----------Figur---------------------------------------
# #Inn parametre: subtxt, grtxt, grtxt2, tittel, Andeler
#
# # source(paste(libkat, 'LibFigFilType.R', sep=''), encoding="UTF-8")
# FigTypUt <- figtype(outfile)
# farger <- FigTypUt$farger
#
# #Plottspesifikke parametre:
# #farger <- hsv(7/12, c(1,0.55,0.35,0.1,0), v=c(0.62,0.75,0.9,0.95,1))
# #source(paste(libkat, 'fun_FigFilType.R', sep=''), encoding="UTF-8")
# #figtype(outfile)
#
# # plotType <- 'S'	#'L'-linje, 'S'-søyle. Søyler er standard.
# antGr <- length(grtxt)
#
#
# if (retn == 'V' ) {
# #Vertikale søyler eller linje
# 	par('fig'=c(0, 1, 0, 0.9))
# 	ymax <- min(max(Andeler, na.rm=T)*1.25, 110)
# 	pos <- barplot(as.numeric(Andeler), beside=TRUE, las=txtretn, ylab="Andel (%)",	#main=tittel,
# 		#sub=subtxt,	#names.arg=grtxt, cex.names=cexgr,
# 		col=farger[1], border='white', ylim=c(0, ymax))	#farger[c(1,3)]
# 	mtext(at=pos, grtxt, side=1, las=1, cex=0.8, adj=0.5, line=0.5)
# 	mtext(at=pos, grtxt2, side=1, las=txtretn, cex=0.8, adj=0.5, line=1.5)
# #	text(pos, Andeler+0.015*ymax, topsoyletxt, cex=0.75)
# 	text(pos, 0, pos=3, topsoyletxt, cex=0.75, col=farger[4])
# 	mtext(at=min(pos)-0.5, cex=0.8, side=1, line=3, adj=0, undertxt)
# 	mtext(side=1, line=2, adj=0.5, subtxt)
# }
#
# if (retn == 'H') {
# #Horisontale søyler
# 	ymax <- antGr*1.4
# 	xmax <- switch(type, andel=min(max(Andeler,na.rm=T)*1.25, 100), ant=max(Andeler)*1.1)
# 	par('fig'=c(0.15, 1, 0, 0.9))
# 	pos <- barplot(rev(as.numeric(Andeler)), horiz=TRUE, beside=TRUE, las=1, #xlab="Andel(%)", #main=tittel,
# 		col=farger[1], border='white', font.main=1, xlim=c(0, xmax), ylim=c(0,ymax))	#
# #	text(rev(Andeler)+0.015*ymax, pos+0, rev(as.numeric(Ngr)), cex=0.75)
# 	mtext(at=pos+0.15, rev(grtxt), side=2, las=1, cex=0.8, adj=1, line=0.25)
# 	mtext(side=1, line=2, adj=0.5, switch(type, andel='Andel (%)', ant='Antall registrerte'))
# #	mtext(at=min(pos)-0.5, cex=0.8, side=1, line=3, adj=0, 'Tall ved søylene angir antall registreringer')
# }
# legend('top', paste('Totalt antall registreringer: ', N, sep=''),
# 		border=NA, fill='white', bty='n', ncol=1, cex=0.9)
#
# utvpos <- 5.2
# avst <- 0.8
# krymp <- 0.8
# title(tittel, font.main=1)	#line=1,
# mtext(utvalgTxt[1], side=3, las=1, cex=krymp, adj=0, line=utvpos, col=farger[1])
# mtext(utvalgTxt[2], side=3, las=1, cex=krymp, adj=0, line=utvpos-avst, col=farger[1])
# mtext(utvalgTxt[3], side=3, las=1, cex=krymp, adj=0, line=utvpos-2*avst, col=farger[1])
# mtext(utvalgTxt[4], side=3, las=1, cex=krymp, adj=0, line=utvpos-3*avst, col=farger[1])
# mtext(utvalgTxt[5], side=3, las=1, cex=krymp, adj=0, line=utvpos-4*avst, col=farger[1])
#
# par('fig'=c(0, 1, 0, 1))
# if ( outfile != '') {dev.off()}
#
# }
# }
# For variabler som går på person, ikke per operasjon
# if (valgtVar %in% c()) {
#     RegData <- RegData[order(RegData$InnDato, decreasing = T), ]   # Sorter slik at man velger nyeste operasjon når flere
#     RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
# }

