#' Lag søylediagram med kummulativ vekst over år
#'
#' Denne funksjonen lager et søylediagram som viser kummulativ vekst over år
#' filtrert på de utvalg som er gjort.
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return En figur med kummulativt vekst i registeret
#'
#' @export

MuskelFigKumsum <- function(RegData, valgtVar='AntReg', datoFra='2000-01-01', datoTil='2050-01-01', reshID, diagnosegr='',
                             minald=0, maxald=120, erMann=99, outfile='', forlop = 99, avdod='',
                             enhetsUtvalg=0, preprosess=F, hentData=F)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- MuskelHentRegData()
  }

  # Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- MuskelPreprosess(RegData=RegData)
  }

  # # Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  # if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$AvdRESH == reshID), ]}
  #
  # # Sykehustekst avhengig av bruker og brukervalg
  # if (enhetsUtvalg==0) {
  #   shtxt <- 'Hele landet'
  # } else {
  #   shtxt <- as.character(RegData$SykehusNavn[match(reshID, RegData$AvdRESH)])
  # }
  #
  # # RegData <- RegData[RegData$ForlopsType1Num == 1, ]    # Velger basisregistreringene

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  MuskelUtvalg <- MuskelUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, forlop = forlop,
                               maxald=maxald, erMann=erMann, diagnosegr=diagnosegr, reshID=reshID, avdod=avdod, enhetsUtvalg = enhetsUtvalg)
  RegData <- MuskelUtvalg$RegData
  utvalgTxt <- MuskelUtvalg$utvalgTxt



  Andeler <- cumsum(table(RegData$Aar, useNA = 'ifany'))
  cexgr <- 1
  par('fig'=c(0, 1, 0, 1-0.02*(length(utvalgTxt)-1)))

  FigTypUt <- figtype(outfile=outfile, fargepalett='BlaaRapp', pointsizePDF=12)
  farger <- FigTypUt$farger
  ymax <- max(Andeler, na.rm=T)*1.25
  pos <- barplot(as.numeric(Andeler), beside=TRUE, las=1, ylab="Antall",
                 col=farger[1], border='white', ylim=c(0, ymax))
  text(pos, Andeler+max(Andeler)/50, labels = Andeler, cex=cexgr)
  mtext(at=pos, names(Andeler), side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
  # title(main = paste0('Antall registrerte per ', Sys.Date()), line=1, font.main=1, cex.main=1.3*cexgr)
  title(main = 'Antall registrerte', line=0.5, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=.9*cexgr, adj=0, col=FigTypUt$farger[1], line=c(1.5+0.8*((length(utvalgTxt) -1):0)))
  par('fig'=c(0, 1, 0, 1))
  if ( outfile != '') {dev.off()}

}
