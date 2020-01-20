#' Lag plot med kummulativ andel av valgt variabel
#'
#' Denne funksjonen lager et plot som viser kummulativ andel av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @inheritParams MuskelFigAndeler
#' @param diagnoseSatt daignosesatt
#'
#' @return Et plot som viser kummulativ andel av valgt variabel
#'
#' @export
#'
MuskelFigCumAndel_flereShus <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID, diagnosegr='',
                             minald=0, maxald=120, erMann=99, outfile='', diagnoseSatt=99, forlop = 99,
                             enhetsUtvalg=1, preprosess=F, hentData=F)
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
                               maxald=maxald, erMann=erMann, diagnosegr=diagnosegr, diagnoseSatt=diagnoseSatt)
  RegData <- MuskelUtvalg$RegData
  utvalgTxt <- MuskelUtvalg$utvalgTxt

  RegData <- RegData[which(RegData$HFdiag %in% c('OUS', 'UNN', 'Helse Bergen')), ]
  RegData$HFdiag <- as.character(RegData$HFdiag)

  if (valgtVar %in% c('TidDebDiag', 'TidDebUtred', 'TidUtredDiag')) {
    RegData$Variabel <- RegData[, valgtVar]
    if (valgtVar %in% c('TidDebDiag', 'TidUtredDiag')) {
      RegData <- RegData[which(!(RegData$DiagICD10 %in% c('G71.9', 'G12.9', 'G60.9'))), ]}
    RegData <- RegData[!is.na(RegData$Variabel),]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    N <- dim(RegData)[1]
    tittel <- switch(valgtVar,
                     TidDebDiag = 'Tid fra symptomdebut til spesifikk diagnose',
                     TidDebUtred = 'Tid fra symptomdebut til utredningsstart',
                     TidUtredDiag = c('Tid fra utredningsstart til diagnose', '(for de som f\u00E5r en spesifikk diagnose )'))
    cexgr <- 0.8
    subtxt <- 'Antall \u00E5r'
    aux <- apply(table(RegData$HFdiag, RegData$Variabel, useNA = 'ifany'), 1, cumsum)
    Ngr <- tapply(RegData$Variabel, RegData$HFdiag, length)
    CumAndel <- 100 * aux / t(matrix(Ngr, nrow = dim(aux)[2], ncol = dim(aux)[1]))
    grtxt <- as.numeric(rownames(aux))
  }

  FigTypUt <- figtype(outfile)
  farger <- FigTypUt$farger

  NutvTxt <- length(utvalgTxt)
  par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))

  plot(grtxt, CumAndel[ , 1], type='l', ylim= c(0,110), lwd=2, col=farger[3],
       xlab = 'Antall \u00E5r', ylab='Kumulativ andel (%)', frame.plot=FALSE)
  lines(grtxt, CumAndel[ , 2], type='l', lwd=2, lty=2, col='red')
  lines(grtxt, CumAndel[ , 3], type='l', lwd=2, lty=3, col=farger[1])
  # title(tittel, line=1, font.main=1)
  abline(h=c(20,40,60,80,100), col=farger[3], lwd=1)
#   text(x=(0+max(grtxt))/2, y=105, paste('Totalt antall registreringer: ', N, ' (=100%)', sep=''),
#        adj=0.5, cex=0.85)
  legend(x="top", legend = c(paste0('HUS, N = ', Ngr[1]), paste0('OUS, N = ', Ngr[2]), paste0('UNN, N = ', Ngr[3])), lty = 1:3, col = c(farger[3], 'red', farger[1]), lwd=2, cex=.8, bty='n')

  krymp <- .9
  title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}


}
