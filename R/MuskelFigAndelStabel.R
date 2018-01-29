#' Lag grupperte stabelplot for valgte variabler
#'
#' Denne funksjonen lager grupperte stabelplot for valgte variabler
#'
#' Her kan detaljer skrives
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return PrepData En figur med stabelplot av ønsket variabel
#'
#' @export
#'
MuskelFigAndelStabel<- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID, diagnosegr='',
                          minald=0, maxald=120, erMann=99, outfile='', diagnoseSatt=99, forlop = 99,
                          enhetsUtvalg=1, preprosess=F, hentData=F, incl_N=F)
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
  # if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}

#   # Sykehustekst avhengig av bruker og brukervalg
#   if (enhetsUtvalg==0) {
#     shtxt <- 'Hele landet'
#   } else {
#     shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
#   }

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  MuskelUtvalg <- MuskelUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, forlop = forlop,
                               maxald=maxald, erMann=erMann, diagnosegr=diagnosegr, reshID=reshID, enhetsUtvalg=enhetsUtvalg)
  RegData <- MuskelUtvalg$RegData
  utvalgTxt <- MuskelUtvalg$utvalgTxt

  PlotParams <- MuskelPrepVar(RegData=RegData, valgtVar=valgtVar)
  RegData <- PlotParams$RegData
  PlotParams$RegData <- NA

  #-------------------------Beregninger-----------------------------------------
  # RegData$Gr <- factor(RegData$Diagnosegr_label)
  # grtxt <- levels(factor(RegData$Gr))
  if (PlotParams$flerevar == 0) {
    grtxt <- levels(RegData$Gr)
    stabeltxt <- levels(RegData$VariabelGr)
    NVarGr <- ftable(RegData[ , c('VariabelGr','Gr')])	#ftable(list(RegData$Var, RegData$Gr))
    NGr <- colSums(NVarGr)
    AndelVar <- prop.table(NVarGr,2)*100
  }
  if (PlotParams$flerevar == 1) {
    # grtxt <- levels(RegData$Gr)
    # AntVar <- PlotParams$AntVar
    # stabeltxt <- as.character(AntVar$Group.1)
    grtxt <- levels(RegData$Gr)
    tmp <- tidyr::gather(PlotParams$AntVar, VariabelGr, Antall, -Group.1)
    AndelVar <- tidyr::spread(tmp, Group.1, Antall)
    stabeltxt <- AndelVar$VariabelGr
    AndelVar <- AndelVar[,-1]/t(matrix(PlotParams$NVar, nrow = length(grtxt), ncol = length(stabeltxt)))*100
    NGr <- PlotParams$NVar
    # rep(PlotParams$NVar, 2,2)
  }

  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; stabel <- PlotParams$stabel;
  subtxt <- PlotParams$subtxt; cexgr <- PlotParams$cexgr;
  FigTypUt <- figtype(outfile=outfile, fargepalett=MuskelUtvalg$fargepalett, pointsizePDF=12)

  farger <- FigTypUt$farger
  NutvTxt <- length(utvalgTxt)



  par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med

  if (length(stabeltxt) == 2 & !stabel){
    ymax <- min(1.1*max(AndelVar[1,]), 100)
    ylabel <- "Andel pasienter"
    pos <- barplot(AndelVar[1,], beside=TRUE, las=1, ylab=ylabel,  #main=tittel,
                   sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, #names.arg=grtxt, cex.names=cexgr,
                   col=farger[1], border='white', ylim=c(0, ymax))	#farger[c(1,3)]
    mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
    text(pos, AndelVar[1,]+1, paste('N=',NGr,sep=''), cex=0.75)
  } else {
    if (stabel==1){
      koord <- barplot(AndelVar, beside=F, las=1, #names.arg=grtxt, cex.names=0.95,
                       col=farger, ylab="Andel (%)", ylim=c(0,132),	 #xlim=c(0, length(grtxt)*1.2),
                       cex.main=1, font.main=1, axes=F, cex.axis=.9, cex.lab=.95, space=.25, border=NA) #
      axis(side=2, at=c(0,20,40,60,80,100))
      legend('top', legend=rev(stabeltxt), bty='n', cex=.8, 	#max(koord+0.5)*1.35, y=80,
             xjust=0.5, fill=farger[length(stabeltxt):1], border=farger[length(stabeltxt):1], ncol=1)
      mtext(at=koord, cex=0.9, side=1, line=0, adj=0.5, grtxt)	#
      text(koord, 102.7, paste('N=',NGr,sep=''), cex=0.75)
    } else {
      koord <- barplot(as.matrix(AndelVar), beside=T, las=2, #names.arg=grtxt, cex.names=0.95,
                       col=farger[1:length(stabeltxt)], ylab="Andel (%)", ylim=c(0,max(AndelVar)*1.2),	 #xlim=c(0, length(grtxt)*1.2),
                       cex.main=1, font.main=1, axes=F, cex.axis=.9, cex.lab=.95, border=NA, xaxt='n', ann=FALSE) #
      axis(side=2, at=c(0,20,40,60,80,100))
      legend('top', legend=rev(stabeltxt), bty='n', cex=.8, 	#max(koord+0.5)*1.35, y=80,
             xjust=0.5, fill=farger[length(stabeltxt):1], border=farger[length(stabeltxt):1], ncol=1)
      mtext(at=colMeans(koord), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=colMeans(koord), paste('N=',NGr,sep=''), side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
    }
  }

  krymp <- .9
  title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}



}

