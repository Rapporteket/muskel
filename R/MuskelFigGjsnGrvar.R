#' Presenter andeler gruppert etter valgt grupperingsvariabel
#'
#' Denne funksjonen lager grupperte andeler
#'
#' Her kan detaljer skrives
#'
#' @inheritParams MuskelFigAndeler
#'
#' @return PrepData En figur med ønsket plot
#'
#' @export
#'
MuskelFigGjsnGrvar<- function(RegData, valgtVar, datoFra='2000-01-01', datoTil='2050-01-01', diagnosegr=-1, grvar="SykehusNavn",
                                minald=0, maxald=120, erMann=99, outfile='', forlop = 99, diagnose=-1, undergr=-1, skriftstr = 1.0,
                                undergr2=-1, avdod='', inkl_tittel=T, gen_aarsak_paavist=-1, xtekst='', graa = '', tittel='',
                               terskel=10, decreasing=T, UtredningsaarFra=1950, UtredningsaarTil=as.numeric(format(Sys.Date(),"%Y")))
{
  # RegData=subdata; valgtVar= "TidUtredDiag"; datoFra='2015-01-01'; datoTil='2019-12-31';
  # diagnosegr=-1; grvar="rhf"; minald=0; maxald=120; erMann=99; outfile=''; forlop = 99;
  # diagnose=-1; undergr=-1; skriftstr = 1.0; undergr2=-1; avdod=''; inkl_tittel=T; decreasing=T
  # gen_aarsak_paavist=-1; xtekst='Andel'; graa = ''; tittel='Andel genetisk verifisert diagnose'; terskel=30

  ## Fjerner registreringer som mangler grupperingsvariabel
  RegData$grvar <- RegData[, grvar]
  RegData <- RegData[!is.na(RegData$grvar), ]

  ## Fjerner registreringer som mangler valgt variabel
  RegData$Variabel <- RegData[, valgtVar]
  RegData <- RegData[!is.na(RegData$Variabel), ]


  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  MuskelUtvalg <- MuskelUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, minald=minald, forlop = forlop,
                               diagnose=diagnose, undergr=undergr, undergr2=undergr2, maxald=maxald, erMann=erMann, gen_aarsak_paavist=gen_aarsak_paavist,
                               diagnosegr=diagnosegr, reshID=0, enhetsUtvalg=0, egenavd = 0, avdod=avdod, UtredningsaarFra=UtredningsaarFra,
                               UtredningsaarTil = UtredningsaarTil)
  RegData <- MuskelUtvalg$RegData
  utvalgTxt <- MuskelUtvalg$utvalgTxt

  Tabell <- RegData %>% dplyr::group_by(grvar) %>%
    dplyr::summarise(antall = sum(Variabel),
                     N = dplyr::n(),
                     andel = antall/N) %>% dplyr::ungroup()

  Tabell <- dplyr::bind_rows(Tabell, dplyr::tibble(grvar='Nasjonalt', antall=sum(Tabell$antall),
                                     N=sum(Tabell$N), andel = sum(Tabell$antall)/sum(Tabell$N)))

  Tabell$grvar_ren <- Tabell$grvar
  Tabell$grvar <- paste0(Tabell$grvar, ' (', Tabell$N, ')')

  Tabell$andel[Tabell$N < terskel] <- NA

  if (decreasing){
    rekkefolge <- order(Tabell$andel, decreasing = !decreasing, na.last = F)
  } else {
    rekkefolge <- order(Tabell$andel, decreasing = !decreasing)
  }
  Tabell <- Tabell[rekkefolge, ]
  Tabell[Tabell$N<terskel, -1] <- NA

  col_txt <- sprintf('%.1f', Tabell$andel)
  col_txt[is.na(Tabell$andel)] <- paste0('N<', terskel)
  col_txt <- c(col_txt, NA)

  Tabell <- dplyr::bind_rows(Tabell, dplyr::tibble(grvar='(N)', antall=NA, N=NA, andel=NA, grvar_ren=NA))

  FigTypUt <- rapFigurer::figtype(outfile=outfile, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(Tabell)[1])
  soyleFarger[which(substr(Tabell$grvar, 1, 6)=='Nasjon')] <- farger[4]
  soyleFarger[which(Tabell$grvar_ren %in% graa)] <- 'gray88'

  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig


  cexgr <- skriftstr
  xmax <- max(Tabell$andel, na.rm = T)*1.1

  vmarg <- max(0, strwidth(Tabell$andel, units='figure', cex=cexgr)*0.8)
  # par('fig'=c(vmarg, 1, 0, 1))
  NutvTxt <- length(MuskelUtvalg$utvalgTxt)
  par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))

  ypos <- barplot( t(Tabell$andel), beside=T, las=1,
                   # main = tittel,
                   font.main=1, #cex.main=1.3,
                   # xlim=c(0,max(andeler, na.rm = T)*1.1),
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(Tabell)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = xtekst)#,
  # ylim = c(0,dim(Tabell)[1]*1.4)) # '#96BBE7'
  ypos <- as.vector(ypos)

  axis(1,cex.axis=0.9)
  mtext(Tabell$grvar, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  # mtext( 'Sykehus/HF', side=2, line=9.5, las=0, col=1, cex=cexgr)
  text(x=0, y=ypos, labels = col_txt, cex=0.8*cexgr,pos=4)


  title(tittel, line=1, font.main=1, cex.main=1.2*cexgr)
  mtext(MuskelUtvalg$utvalgTxt, side=3, las=1, cex=0.9*cexgr, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

  par('fig'= oldpar_fig)
  # if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
  if (outfile != '') {dev.off()}



}
