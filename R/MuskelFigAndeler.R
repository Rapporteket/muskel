#' Lag søylediagram som viser andeler av ulike variabler
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
#' @param diagnosegr Diagnosegruppe
#'                 1: Muskelsykdommer
#'                 2: Spinal muskelatrofi
#'                 3: Polynevropati
#' @param forlop Forløpstype
#'                 1: Basisregistrering
#'                 2: Oppfølging
#'                 3: Annet
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet (Default)
#'                 1: Egen enhet mot resten av landet
#'                 2: Egen enhet
#' @param egenavd  0: Registrert ved HF
#'                 1: Følges opp ved HF
#'                 2: Diagnostisert ved HF
#'                 3: Bostatt i fylke
#' @param diagnose diagnosegrupper
#' @param undergr undergruppe av diagnosegruppen
#' @param undergr2 undergruppe to
#' @param avdod avdød.
#' @param UtredningsaarFra Utredningsår f.o.m
#' @param UtredningsaarTil Utredningsår t.o.m
#' @param inkl_tittel inkluder titel. kan være TRUE eller FALSE
#' @param debutAlderFra debutalder f.o.m
#' @param debutAlderTil debutalder t.o.m
#'
#'
#' @return En figur med søylediagram av ønsket variabel
#'
#' @export
#'
MuskelFigAndeler <- function(
    RegData, valgtVar='Alder', datoFra='2000-01-01',
    datoTil='2050-01-01', reshID=0, diagnosegr=-1,
    minald=0, maxald=120, erMann=99, outfile='',
    forlop = 99, egenavd = 0, diagnose=-1,
    undergr=-1, undergr2=-1, enhetsUtvalg=0,
    avdod='', gen_aarsak_paavist=-1,
    UtredningsaarFra=1950,
    UtredningsaarTil=as.numeric(format(Sys.Date(),"%Y")),
    inkl_tittel=T, debutAlderFra=0, debutAlderTil=90)
{


  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  MuskelUtvalg <- MuskelUtvalg(
    RegData=RegData, datoFra=datoFra, datoTil=datoTil,
    minald=minald, forlop = forlop, maxald=maxald, erMann=erMann,
    diagnosegr=diagnosegr, enhetsUtvalg, diagnose=diagnose,
    undergr=undergr, undergr2=undergr2, avdod=avdod, egenavd=egenavd,
    reshID=reshID, UtredningsaarFra=UtredningsaarFra,
    UtredningsaarTil=UtredningsaarTil, debutAlderFra=debutAlderFra,
    debutAlderTil=debutAlderTil, gen_aarsak_paavist = gen_aarsak_paavist)
  RegData <- MuskelUtvalg$RegData
  utvalgTxt <- MuskelUtvalg$utvalgTxt
  shtxt <- MuskelUtvalg$shtxt

  # print(dim(RegData))

  # Initialiserer nødvendige størrelser
  Andeler <- list(Hoved = 0, Rest =0)
  ind <- MuskelUtvalg$ind
  Nrest <- 0

  if (valgtVar %in% c('DiagByggerPaa', 'DiagGenVerifisert',
                      'TypeMedikBehandling', 'KonkrUndGrDuchBeck',
                      'AndelSteroider', 'TypeHjerteaffeksjon')) {
    flerevar <- 1
  } else {
    flerevar <- 0
  }

  if (flerevar == 0 ) {
    ## Forbered variabler for fremstilling i figur
    PlotParams <- MuskelPrepVar(RegData=RegData, valgtVar=valgtVar,
                                inkl_tittel=inkl_tittel)
    RegData <- PlotParams$RegData
    PlotParams$RegData <- NA

    if (enhetsUtvalg==1) {
      if (egenavd ==3) {
        ind <- list(Hoved=which(RegData$Fylke == MuskelUtvalg$fylke), Rest=which(RegData$Fylke != MuskelUtvalg$fylke))
      } else {
        ind <- list(Hoved=which(RegData$AvdRESH == reshID), Rest=which(RegData$AvdRESH != reshID))
      }
      AntHoved <- table(RegData$VariabelGr[ind$Hoved])
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
      AntRest <- table(RegData$VariabelGr[ind$Rest])
      Nrest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*AntRest/Nrest
      # Antall <- merge(as.data.frame(AntHoved), as.data.frame(AntRest), by = 'Var1', all = T)
      Antall <- cbind(AntHoved, AntRest)
      N_ut <- cbind(NHoved=rep(NHoved, dim(Antall)[1]),
                    Nrest=rep(Nrest, dim(Antall)[1]))
      Antall <- as.data.frame(cbind(Antall, N_ut))
    } else {
      AntHoved <- table(RegData$VariabelGr)
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
      # Antall <- as.data.frame(AntHoved)
      N_ut <- rep(NHoved, dim(AntHoved)[1])
      Antall <- as.data.frame(cbind(AntHoved, NHoved=N_ut))
    }
  }

  if (flerevar == 1){

    if (enhetsUtvalg==1) {
      PlotParams <- MuskelPrepVar(RegData[ind$Hoved, ], valgtVar, inkl_tittel=inkl_tittel) # Hovegruppe
      AntHoved <- PlotParams$AntVar
      NHoved <- max(PlotParams$NVar, na.rm=T)
      Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar
      PlotParams2 <- MuskelPrepVar(RegData[ind$Rest, ], valgtVar, inkl_tittel=inkl_tittel) # Sammenligningsgruppe
      AntRest <- PlotParams2$AntVar
      Nrest <- max(PlotParams2$NVar, na.rm=T)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*PlotParams2$AntVar/PlotParams2$NVar
      Antall <- data.frame(AntHoved, AntRest, NHoved=PlotParams$NVar, Nrest = PlotParams2$NVar)
      rm(PlotParams2)
    } else {
      PlotParams <- MuskelPrepVar(RegData, valgtVar, inkl_tittel=inkl_tittel)
      AntHoved <- PlotParams$AntVar
      NHoved <- max(PlotParams$NVar, na.rm=T)
      if (valgtVar=='AndelSteroider') {
        NHoved <- sum(PlotParams$NVar, na.rm=T)
      }
      Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar
      Antall <- data.frame(AntHoved, NHoved=PlotParams$NVar)
    }
  }   #end sjekk om figuren inneholder flere variable


  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt; #grtxt2 <- PlotParams$grtxt2;
  subtxt <- PlotParams$subtxt; retn <- PlotParams$retn;
  cexgr <- PlotParams$cexgr;
  FigTypUt <- rapFigurer::figtype(
    outfile=outfile, fargepalett=MuskelUtvalg$fargepalett,
    pointsizePDF=12)

  #Hvis for få observasjoner..
  if (NHoved < 5 | (Nrest<5 & enhetsUtvalg==1)) {
    farger <- FigTypUt$farger
    plot.new()
    # title(tittel)	#, line=-6)
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'F\u00E6rre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
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
    # vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8)) # Gammel versjon beholdt for referanse
    vmarg <- switch(retn, V=0, min(H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8),
                                   strwidth('passe lang eksempeltekst', units='figure', cex=cexgr)*0.8))
    grtxtpst <- wrap.it(grtxtpst, len = 25)
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
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet for\u00F8vrig (N=', Nrest,')', sep='')),
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
          text(x=rev(as.numeric(Andeler$Hoved)), y=pos+0.05, labels = paste0('n=', rev(PlotParams$NVar)), pos=4)
        }
        else {
          text(x=rev(as.numeric(Andeler$Hoved)), y=pos+0.05, labels = paste0('n=', rev(sprintf('%.0f', Andeler$Hoved*NHoved/100))), pos=4)
        }
      }

      if (enhetsUtvalg == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet for\u00F8vrig (N=', Nrest,')', sep='')),
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

    utData <- list(tittel = tittel, utvalgTxt = utvalgTxt,
                   Andeler = Andeler, Antall = Antall,
                   PlotParams=PlotParams,
                   MuskelUtvalg=MuskelUtvalg,
                   NHoved=NHoved,
                   Nrest=Nrest,
                   enhetsUtvalg=enhetsUtvalg,
                   AntHoved = AntHoved)
    return(invisible(utData))

  }

}

#' Beregn andeler til bruk i MuskelPlotAndeler
#'
#' Beskrevet over
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return Beregn fordeling av valgt variabel
#'
#' @export
#'
MuskelBeregnAndeler <- function(
    RegData, valgtVar='Alder', datoFra='2000-01-01',
    datoTil='2050-01-01', reshID=0, diagnosegr=-1,
    minald=0, maxald=120, erMann=99, outfile='',
    forlop = 99, egenavd = 0, diagnose=-1,
    undergr=-1, undergr2=-1, enhetsUtvalg=0,
    avdod='', gen_aarsak_paavist=-1,
    UtredningsaarFra=1950,
    UtredningsaarTil=as.numeric(format(Sys.Date(),"%Y")),
    inkl_tittel=T, debutAlderFra=0, debutAlderTil=90)
{
  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  MuskelUtvalg <- MuskelUtvalg(
    RegData=RegData, datoFra=datoFra, datoTil=datoTil,
    minald=minald, forlop = forlop, maxald=maxald, erMann=erMann,
    diagnosegr=diagnosegr, enhetsUtvalg, diagnose=diagnose,
    undergr=undergr, undergr2=undergr2, avdod=avdod, egenavd=egenavd,
    reshID=reshID, UtredningsaarFra=UtredningsaarFra,
    UtredningsaarTil=UtredningsaarTil, debutAlderFra=debutAlderFra,
    debutAlderTil=debutAlderTil, gen_aarsak_paavist = gen_aarsak_paavist)
  RegData <- MuskelUtvalg$RegData
  utvalgTxt <- MuskelUtvalg$utvalgTxt
  shtxt <- MuskelUtvalg$shtxt

  # Initialiserer nødvendige størrelser
  Andeler <- list(Hoved = 0, Rest =0)
  ind <- MuskelUtvalg$ind
  Nrest <- 0

  if (valgtVar %in% c('DiagByggerPaa', 'DiagGenVerifisert',
                      'TypeMedikBehandling', 'KonkrUndGrDuchBeck',
                      'AndelSteroider', 'TypeHjerteaffeksjon')) {
    flerevar <- 1
  } else {
    flerevar <- 0
  }

  if (flerevar == 0 ) {
    ## Forbered variabler for fremstilling i figur
    PlotParams <- MuskelPrepVar(RegData=RegData, valgtVar=valgtVar,
                                inkl_tittel=inkl_tittel)
    RegData <- PlotParams$RegData
    PlotParams$RegData <- NA

    if (enhetsUtvalg==1) {
      if (egenavd ==3) {
        ind <- list(Hoved=which(RegData$Fylke == MuskelUtvalg$fylke),
                    Rest=which(RegData$Fylke != MuskelUtvalg$fylke))
      } else {
        ind <- list(Hoved=which(RegData$AvdRESH == reshID),
                    Rest=which(RegData$AvdRESH != reshID))
      }
      AntHoved <- table(RegData$VariabelGr[ind$Hoved])
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
      AntRest <- table(RegData$VariabelGr[ind$Rest])
      Nrest <- sum(AntRest)
      Andeler$Rest <- 100*AntRest/Nrest
      Antall <- cbind(AntHoved, AntRest)
      N_ut <- cbind(NHoved=rep(NHoved, dim(Antall)[1]),
                    Nrest=rep(Nrest, dim(Antall)[1]))
      Antall <- as.data.frame(cbind(Antall, N_ut))
    } else {
      AntHoved <- table(RegData$VariabelGr)
      NHoved <- sum(AntHoved)
      Andeler$Hoved <- 100*AntHoved/NHoved
      N_ut <- rep(NHoved, dim(AntHoved)[1])
      Antall <- as.data.frame(cbind(AntHoved, NHoved=N_ut))
    }
  }

  if (flerevar == 1){

    if (enhetsUtvalg==1) {
      PlotParams <- MuskelPrepVar(RegData[ind$Hoved, ],
                                  valgtVar, inkl_tittel=inkl_tittel) # Hovegruppe
      AntHoved <- PlotParams$AntVar
      NHoved <- max(PlotParams$NVar, na.rm=T)
      Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar
      PlotParams2 <- MuskelPrepVar(RegData[ind$Rest, ], valgtVar,
                                   inkl_tittel=inkl_tittel) # Sammenligningsgruppe
      AntRest <- PlotParams2$AntVar
      Nrest <- max(PlotParams2$NVar, na.rm=T)	#length(indRest)- Kan inneholde NA
      Andeler$Rest <- 100*PlotParams2$AntVar/PlotParams2$NVar
      Antall <- data.frame(AntHoved, AntRest,
                           NHoved=PlotParams$NVar, Nrest = PlotParams2$NVar)
      rm(PlotParams2)
    } else {
      PlotParams <- MuskelPrepVar(RegData, valgtVar,
                                  inkl_tittel=inkl_tittel)
      AntHoved <- PlotParams$AntVar
      NHoved <- max(PlotParams$NVar, na.rm=T)
      if (valgtVar=='AndelSteroider') {
        NHoved <- sum(PlotParams$NVar, na.rm=T)
      }
      Andeler$Hoved <- 100*PlotParams$AntVar/PlotParams$NVar
      Antall <- data.frame(AntHoved, NHoved=PlotParams$NVar)
    }
  }   #end sjekk om figuren inneholder flere variable

  tittel <- PlotParams$tittel;

  utData <- list(tittel = tittel, utvalgTxt = utvalgTxt,
                 Andeler = Andeler, Antall = Antall,
                 PlotParams=PlotParams,
                 MuskelUtvalg=MuskelUtvalg,
                 NHoved=NHoved,
                 Nrest=Nrest,
                 enhetsUtvalg=enhetsUtvalg,
                 AntHoved = AntHoved)
  return(invisible(utData))

}

#' Plotdelen av MuskelFigAndeler tatt ut som egen funksjon
#'
#' Beskrevet over
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return Fordelingsfigur over valgt variabel
#'
#' @export
#'
MuskelPlotAndeler <- function(plotdata,
                              outfile="",
                              fargepalett="BlaaRapp") {

  tittel <- plotdata$PlotParams$tittel
  grtxt <- plotdata$PlotParams$grtxt
  subtxt <- plotdata$PlotParams$subtxt
  retn <- plotdata$PlotParams$retn
  cexgr <- plotdata$PlotParams$cexgr
  NHoved <- plotdata$NHoved
  Nrest <- plotdata$Nrest
  enhetsUtvalg <- plotdata$enhetsUtvalg
  utvalgTxt <- plotdata$utvalgTxt
  Andeler <- plotdata$Andeler
  AntHoved <- plotdata$AntHoved
  shtxt <- plotdata$MuskelUtvalg$shtxt
  PlotParams <- plotdata$PlotParams

  FigTypUt <- rapFigurer::figtype(
    outfile=outfile, fargepalett=fargepalett,
    pointsizePDF=12)

  #Hvis for få observasjoner..
  if (NHoved < 5 | (Nrest<5 & enhetsUtvalg==1)) {
    farger <- FigTypUt$farger
    plot.new()
    legend('topleft',utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    text(0.5, 0.6, 'F\u00E6rre enn 5 registreringer i egen- eller
         sammenlikningsgruppa', cex=1.2)
    if ( outfile != '') {dev.off()}
  } else {
    #Plottspesifikke parametre:

    farger <- FigTypUt$farger
    NutvTxt <- length(utvalgTxt)
    grtxtpst <- paste(rev(grtxt), ' (',
                      rev(sprintf('%.1f', Andeler$Hoved)), '%)', sep='')
    grtxt2 <- paste(sprintf('%.1f',Andeler$Hoved), '%', sep='')

    if (PlotParams$incl_N) {
      grtxtpst <- paste(rev(grtxt), ' (n=',
                        rev(sprintf('%.0f', AntHoved)), ')', sep='')  ####  AD-HOC, farlig
      grtxt2 <- paste('n=', sprintf('%.0f',Andeler$Hoved*NHoved/100), sep='') ####
    }
    vmarg <- switch(
      retn,
      V=0,
      H=min(max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8),
            strwidth('passe lang eksempeltekst', units='figure', cex=cexgr)*0.8))
    grtxtpst <- wrap.it(grtxtpst, len = 25)
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med

    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    antGr <- length(grtxt)
    lwdRest <- 3	# Størrelse på legendmarkør?

    if (retn == 'V' ) {
      #Vertikale søyler
      ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
      ylabel <- "Andel pasienter"
      pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE,
                     las=1, ylab=ylabel,
                     sub=subtxt, cex.axis=cexgr,
                     cex.sub=cexgr,	cex.lab=cexgr,
                     col=fargeHoved, border='white',
                     ylim=c(0, ymax))
      mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
      if (enhetsUtvalg == 1) {
        points(pos, as.numeric(Andeler$Rest), col=fargeRest,
               cex=2, pch=18)
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''),
                        paste('Landet for\u00F8vrig (N=', Nrest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest),
               bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
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

      pos <- barplot(
        rev(as.numeric(Andeler$Hoved)), horiz=TRUE,
        beside=TRUE, las=1, xlab=xlabel,
        col=fargeHoved, border='white',
        font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)
      mtext(at=pos+0.05, text=grtxtpst, side=2, las=1,
            cex=cexgr, adj=1, line=0.25)
      if (PlotParams$N_colwise) {
        if (flerevar == 1) {
          text(x=rev(as.numeric(Andeler$Hoved)), y=pos+0.05,
               labels = paste0('n=', rev(PlotParams$NVar)), pos=4)
        }
        else {
          text(
            x=rev(as.numeric(Andeler$Hoved)), y=pos+0.05,
            labels=paste0('n=', rev(sprintf('%.0f', Andeler$Hoved*NHoved/100))),
            pos=4)
        }
      }

      if (enhetsUtvalg == 1) {
        points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,
               cex=2, pch=18) #c("p","b","o"),
        legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''),
                        paste('Landet for\u00F8vrig (N=', Nrest,')', sep='')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest),
               bty='n', pch=c(15,18), pt.cex=2,
               lwd=lwdRest,	lty=NA, ncol=1, cex=cexgr)
      } else {
        legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
      }
    }


    krymp <- .9
    title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
    mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0,
          col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

    par('fig'=c(0, 1, 0, 1))

    if ( outfile != '') {dev.off()}

  }

}

