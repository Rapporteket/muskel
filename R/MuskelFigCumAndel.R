#' Lag plot med kummulativ andel av valgt variabel
#'
#' Denne funksjonen lager et plot som viser kummulativ andel av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return Et plot som viser kummulativ andel av valgt variabel
#'
#' @export
#'
MuskelFigCumAndel <- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', reshID, diagnosegr='',
                             diagnose='', undergr='', undergr2='', minald=0, maxald=120, erMann=99, outfile='', forlop = 99,
                             enhetsUtvalg=0, egenavd =0, avdod='', preprosess=F, hentData=F, debutAlderFra=0, debutAlderTil=120,
                             UtredningsaarFra=1900, UtredningsaarTil=2100)
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

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  MuskelUtvalg <- MuskelUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, forlop = forlop, egenavd = egenavd, enhetsUtvalg=enhetsUtvalg,
                               diagnose=diagnose, undergr=undergr, undergr2=undergr2, maxald=maxald, erMann=erMann, diagnosegr=diagnosegr,
                               avdod=avdod, debutAlderFra=debutAlderFra, debutAlderTil=debutAlderTil, reshID = reshID,
                               UtredningsaarFra=UtredningsaarFra, UtredningsaarTil=UtredningsaarTil)
  RegData <- MuskelUtvalg$RegData
  RegData <- RegData[MuskelUtvalg$ind$Hoved, ]
  utvalgTxt <- MuskelUtvalg$utvalgTxt
  shtxt <- MuskelUtvalg$shtxt

  if (valgtVar %in% c('TidDebDiag', 'TidDebUtred', 'TidUtredDiag', 'AlderTapGang', 'AlderRespStotte', 'TrygdFraAlder')) {
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
                     TidUtredDiag = c('Tid fra utredningsstart til diagnose', '(for de som får en spesifikk diagnose )'),
                     AlderTapGang = 'Alder ved tap av gangfunksjon',
                     AlderRespStotte = 'Alder for respirasjonsstøtte',
                     TrygdFraAlder = 'Alder for mottak av trygd')
    cexgr <- 0.8
    subtxt <- 'Antall år'
    if (valgtVar %in% c('AlderTapGang', 'AlderRespStotte', 'TrygdFraAlder')) {subtxt <- 'Alder'}
    CumAndel <- cumsum(table(RegData$Variabel))/N*100
    grtxt <- as.numeric(names(CumAndel))
  }

  if (valgtVar == 'AlderHjAff_cumsum') {
    RegData$Variabel <- RegData$HjerteAffAlder
    N_tot <- length(unique(RegData$PasientID))
    aux <- RegData[which(RegData$HjerteAff %in% c(2,9) | is.na(RegData$HjerteAff)), ]
    RegData <- RegData[which(RegData$HjerteAff == 1), ]
    # RegData <- RegData[!is.na(RegData$Variabel),]
    RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
    RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
    N_ikkekjent <- length(setdiff(aux$PasientID, RegData$PasientID))
    N_ukjent <- sum(is.na(RegData$HjerteAffAlder))
    RegData <- RegData[!is.na(RegData$HjerteAffAlder), ]
    N <- dim(RegData)[1] + N_ukjent
    # N_ukjent <- sum(is.na(RegData$HjerteAffAlder))
    tittel <- c('Alder ved hjerteaffeksjon') #, paste0(N_ukjent, ' ukjente'))

    # tittel <- c('Andel med hjerteaffeksjon', paste0('Totalen inkuderer ', N_ukjent, ' ukjente'))
    maksAld <- max(RegData$Variabel, na.rm = T)
    CumAndel <- cumsum(table(factor(RegData$Variabel, levels = 0:maksAld)))/N*100
    cexgr <- 0.8
    subtxt <- 'Alder'
    grtxt <- as.numeric(names(CumAndel))
  }

  # x11()
  FigTypUt <- figtype(outfile)
  farger <- FigTypUt$farger

  NutvTxt <- length(utvalgTxt)
  par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))
  xmax<-ceiling(max(grtxt)/10)*10
  ymax<-ceiling(max(CumAndel)*1.2)
  # if (valgtVar %in% c('TidDebDiag', 'TidDebUtred', 'TidUtredDiag')) {
    ymax <- 110
    # }

  plot(grtxt, CumAndel, type='l', lwd=2, col=farger[1], xlim = c(0,xmax), ylim= c(0,ymax),
       xlab = subtxt, ylab='Kumulativ andel (%)', frame.plot=FALSE)
  # axis(side=1, at = xskala, labels = Tidtxt, cex.axis=0.9)
  grid(NA, 6, lwd = 1)
  legend('bottomright', shtxt, lty=1, col = farger[1], bty='n', cex = 0.8)
  # title(tittel, line=1, font.main=1)
  # abline(h=c(20,40,60,80,100), col=farger[3], lwd=1)
  if (valgtVar == 'AlderHjAff_cumsum'){
    text(x=(0+max(grtxt))/2, y=ymax, paste0('Antall med hjerteaffeksjon: ', N, ' (=100 %), hvorav ', N_ukjent, ' mangler alder for hjerteaffeksjon'),
         adj=0.5, cex=0.85)
    text(x=(0+max(grtxt))/2, y=ymax-7, paste0('Totalt antall med gitt diagnose: ', N_tot, ', der ', N_ikkekjent, ' har ukjent status for hjerteaffeksjon'),
         adj=0.5, cex=0.85)
  } else {
    text(x=(0+max(grtxt))/2, y=ymax, paste0('Antall pasienter: ', N, ' (=100 %)'),
         adj=0.5, cex=0.85)
  }




  krymp <- .9
  title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}


}
