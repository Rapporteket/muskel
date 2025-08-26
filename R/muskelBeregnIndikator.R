
#' Calculate Indicator for Muskel Data
#'
#' This function calculates an indicator based on the provided data and indicator ID.
#'
#' @param RegData A data frame containing the registry data to be used for the calculation.
#' @param ind_id An identifier for the indicator to be calculated.
#'
#' @return A data frame or other object containing the calculated indicator results.
#'
#' @examples
#' # Example usage:
#' # result <- muskelBeregnIndikator(RegData, ind_id = 1)
#'
#' @export
muskelBeregnIndikator <- function(RegData, ind_id) {

  kobl_resh_orgnr <- tibble::tribble(
    ~ReshID, ~Sykehus, ~orgnr,
    100065,          "Helgelandssykehuset", 983974929,
    100082,                 "Helse Bergen", 983974724,
    100083,              "Helse Stavanger", 983974678,
    100084,                  "Helse Fonna", 983974694,
    100085,                  "Helse Førde", 983974732,
    100089, "Ahus", 983971636,
    100091,          "Sykehuset Innlandet", 983971709,
    100092,            "Sykehuset Østfold", 983971768,
    100093,              "Sunnaas sykehus", 883971752,
    100100,         "Sykehuset i Vestfold", 983975259,
    100132,           "Sykehuset Telemark", 983975267,
    100133,            "Sørlandet sykehus", 983975240,
    100317,         "Helse Nord-Trøndelag", 983974791,
    100320,           "St. Olavs Hospital", 883974832,
    101051,           "Nordlandssykehuset", 983974910,
    101719,                          "UNN", 983974899,
    101971,           "Finnmarkssykehuset", 983974880,
    4001031,     "OUS", 993467049,
    4201115,        "Helse Møre og Romsdal", 997005562,
    700272,                 "Vestre Viken", 894166762,
    960001,         "Privat spesialistsenter", 888888,
    960002,                      "Legekontor", 999999,
    960003,           "Rehabiliteringssenter", 333333333
  )

  terskel <- 5
  minstekrav <- NA
  maal <- NA
  skriftStr <- 1.0
  pktStr <- 1
  legPlass <- "top"
  minstekravTxt <- "Akseptabelt"
  maalTxt <- "Mål"
  maalretn <- 'hoy'
  decreasing <- F
  width <- 800
  height <- 700
  tittel <- ""

  # avdod <- RegData |> dplyr::filter(!is.na(AvdodDato)) |>
  #   dplyr::filter(ForlopsID == min(ForlopsID),
  #          .by = PasientID) |>
  #   dplyr::select(PasientID, AvdodDato)
  # inkludert <- RegData |> dplyr::filter(ForlopsType1Num == 1) |>
  #   dplyr::select(PasientID, HovedDato, AvdodDato)



  if (ind_id == "muskel_utredningstid") {
    indikator <- RegData |>
      dplyr::filter(!is.na(TidUtredDiag)) |>
      dplyr::filter(TidUtredDiag == min(TidUtredDiag),
                    DiagnoseAar == min(DiagnoseAar),
                    .by = PasientID) |>
      dplyr::filter(ForlopsID == min(ForlopsID),
                    .by = PasientID) |>
      dplyr::select(PasientID, TidUtredDiag, AvdRESH, DiagnoseAar) |>
      dplyr::mutate(var = as.numeric(TidUtredDiag < 2),
                    denominator = 1,
                    year = DiagnoseAar,
                    orgnr = kobl_resh_orgnr$orgnr[
                      match(AvdRESH, kobl_resh_orgnr$ReshID)],
                    Sykehus = kobl_resh_orgnr$Sykehus[
                      match(AvdRESH, kobl_resh_orgnr$ReshID)],
                    context = "caregiver") |>
      dplyr::select(orgnr, year, var, denominator, context, Sykehus)
    tittel <- "Andel pasienter med tilfredsstillende utredningstid"
    minstekrav <- 50
    maal <- 80
  }

  if (ind_id == "muskel_genetisk_bekreftet_muskelsykdommer") {
    indikator <- RegData |>
      # dplyr::mutate(GenetiskAarsakPaavist =
      #                 ifelse(is.na(GenetiskAarsakPaavist) |
      #                          GenetiskAarsakPaavist == 9, 0,
      #                        GenetiskAarsakPaavist)
      # ) |>
      dplyr::filter(Diagnosegr == 1,
                    GenetiskAarsakPaavist %in% 0:1) |>
      dplyr::filter(GenetiskAarsakPaavist == max(GenetiskAarsakPaavist),
                    .by = PasientID) |>
      dplyr::filter(Aar == min(Aar), .by = PasientID) |>
      dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
      dplyr::rename(var = GenetiskAarsakPaavist,
                    year = Aar) |>
      dplyr::mutate(denominator = 1,
                    orgnr = kobl_resh_orgnr$orgnr[
                      match(AvdRESH, kobl_resh_orgnr$ReshID)],
                    Sykehus = kobl_resh_orgnr$Sykehus[
                      match(AvdRESH, kobl_resh_orgnr$ReshID)],
                    context = "caregiver") |>
      dplyr::select(orgnr, year, var, denominator, context, Sykehus)
    tittel <- c("Andel pasienter med molekylærgenetisk bekreftet",
                "diagnose blant pasienter med muskelsykdom")
    minstekrav <- 50
    maal <- 70
  }

  if (ind_id == "muskel_genetisk_bekreftet_sma") {
    indikator <- RegData |>
      # dplyr::mutate(GenetiskAarsakPaavist =
      #                 ifelse(is.na(GenetiskAarsakPaavist) |
      #                          GenetiskAarsakPaavist == 9, 0,
      #                        GenetiskAarsakPaavist)
      # ) |>
      dplyr::filter(Diagnosegr == 2) |>
      dplyr::filter(GenetiskAarsakPaavist == max(GenetiskAarsakPaavist),
                    .by = PasientID) |>
      dplyr::filter(Aar == min(Aar), .by = PasientID) |>
      dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
      dplyr::rename(var = GenetiskAarsakPaavist,
                    year = Aar) |>
      dplyr::mutate(denominator = 1,
                    orgnr = kobl_resh_orgnr$orgnr[
                      match(AvdRESH, kobl_resh_orgnr$ReshID)],
                    Sykehus = kobl_resh_orgnr$Sykehus[
                      match(AvdRESH, kobl_resh_orgnr$ReshID)],
                    context = "caregiver") |>
      dplyr::select(orgnr, year, var, denominator, context, Sykehus)
    tittel <- c("Andel pasienter med molekylærgenetisk bekreftet",
                "diagnose blant pasienter med spinal muskelatrofi")
    minstekrav <- 65
    maal <- 90
  }

  if (ind_id == "muskel_genetisk_bekreftet_polynevropati") {
    indikator <- RegData |>
      dplyr::mutate(GenetiskAarsakPaavist =
                      ifelse(is.na(GenetiskAarsakPaavist) |
                               GenetiskAarsakPaavist == 9, 0,
                             GenetiskAarsakPaavist)
      ) |>
      dplyr::filter(Diagnosegr == 3) |>
      dplyr::filter(GenetiskAarsakPaavist == max(GenetiskAarsakPaavist),
                    .by = PasientID) |>
      dplyr::filter(Aar == min(Aar), .by = PasientID) |>
      dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
      dplyr::rename(var = GenetiskAarsakPaavist,
                    year = Aar) |>
      dplyr::mutate(denominator = 1,
                    orgnr = kobl_resh_orgnr$orgnr[
                      match(AvdRESH, kobl_resh_orgnr$ReshID)],
                    Sykehus = kobl_resh_orgnr$Sykehus[
                      match(AvdRESH, kobl_resh_orgnr$ReshID)],
                    context = "caregiver") |>
      dplyr::select(orgnr, year, var, denominator, context, Sykehus)
    tittel <- c("Andel pasienter med molekylærgenetisk bekreftet",
                "diagnose blant pasienter med polynevropati")
    minstekrav <- 20
    maal <- 40
  }

  if (ind_id == "muskel_genetisk_veiledning") {
    indikator <- RegData |>
      dplyr::filter(OppfolgBarnelegeNevrolog %in% 0:1) |>
      dplyr::filter(OppfolgBarnelegeNevrolog == max(OppfolgBarnelegeNevrolog),
                    .by = PasientID) |>
      dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
      dplyr::mutate(var = OppfolgBarnelegeNevrolog,
                    denominator = 1,
                    year = Aar,
                    FoelgesOppAvIns = ifelse(FoelgesOppAvIns %in% c("97", "98"),
                                             AvdRESH, FoelgesOppAvIns),
                    orgnr = kobl_resh_orgnr$orgnr[
                      match(FoelgesOppAvIns, kobl_resh_orgnr$ReshID)],
                    Sykehus = kobl_resh_orgnr$Sykehus[
                      match(FoelgesOppAvIns, kobl_resh_orgnr$ReshID)],
                    context = "caregiver") |>
      dplyr::select(orgnr, year, var, denominator, context, Sykehus)
      tittel <- "Andel pasienter med genetisk veiledning"
      minstekrav <- 65
      maal <- 90
  }

  if (ind_id == "muskel_oppf_fysio") {
    indikator <- RegData |>
      dplyr::filter(Fysioterapi %in% 0:1) |>
      dplyr::filter(Fysioterapi == max(Fysioterapi), .by = PasientID) |>
      dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
      dplyr::mutate(var = Fysioterapi,
                    denominator = 1,
                    year = Aar,
                    FoelgesOppAvIns = ifelse(FoelgesOppAvIns %in% c("97", "98"),
                                             AvdRESH, FoelgesOppAvIns),
                    orgnr = kobl_resh_orgnr$orgnr[
                      match(FoelgesOppAvIns, kobl_resh_orgnr$ReshID)],
                    Sykehus = kobl_resh_orgnr$Sykehus[
                      match(FoelgesOppAvIns, kobl_resh_orgnr$ReshID)],
                    context = "caregiver") |>
      dplyr::select(orgnr, year, var, denominator, context, Sykehus)
    tittel <- "Andel pasienter med fysioterapi"
    minstekrav <- 65
    maal <- 90
  }

  if (ind_id == "muskel_oppf_hjerteoppf") {
    indikator <- RegData |>
      dplyr::filter(Hjerteoppfoelging %in% 0:1) |>
      dplyr::filter(Hjerteoppfoelging == max(Hjerteoppfoelging),
                    .by = PasientID) |>
      dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
      dplyr::mutate(var = Hjerteoppfoelging,
                    denominator = 1,
                    year = Aar,
                    FoelgesOppAvIns = ifelse(FoelgesOppAvIns %in% c("97", "98"),
                                             AvdRESH, FoelgesOppAvIns),
                    orgnr = kobl_resh_orgnr$orgnr[
                      match(FoelgesOppAvIns, kobl_resh_orgnr$ReshID)],
                    Sykehus = kobl_resh_orgnr$Sykehus[
                      match(FoelgesOppAvIns, kobl_resh_orgnr$ReshID)],
                    context = "caregiver") |>
      dplyr::select(orgnr, year, var, denominator, context, Sykehus)
    tittel <- "Andel pasienter med hjerteoppfølging"
    minstekrav <- 65
    maal <- 90
  }

  if (ind_id == "muskel_oppf_barnelege_nevrolog") {
    indikator <- RegData |>
      dplyr::filter(OppfolgBarnelegeNevrolog %in% 0:1) |>
      dplyr::filter(OppfolgBarnelegeNevrolog == max(OppfolgBarnelegeNevrolog),
                    .by = PasientID) |>
      dplyr::filter(ForlopsID == min(ForlopsID), .by = PasientID) |>
      dplyr::mutate(var = OppfolgBarnelegeNevrolog,
                    denominator = 1,
                    year = Aar,
                    FoelgesOppAvIns = ifelse(FoelgesOppAvIns %in% c("97", "98"),
                                             AvdRESH, FoelgesOppAvIns),
                    orgnr = kobl_resh_orgnr$orgnr[
                      match(FoelgesOppAvIns, kobl_resh_orgnr$ReshID)],
                    Sykehus = kobl_resh_orgnr$Sykehus[
                      match(FoelgesOppAvIns, kobl_resh_orgnr$ReshID)],
                    context = "caregiver") |>
      dplyr::select(orgnr, year, var, denominator, context, Sykehus)
    tittel <- c("Andel pasienter med oppfølging", "hos barnelege/nevrolog")
    minstekrav <- 65
    maal <- 90
  }


  indikatordata <- list(
    indikator=indikator, tittel=tittel, terskel=terskel,
    minstekrav=minstekrav, maal=maal, skriftStr=skriftStr,
    pktStr=pktStr, legPlass=legPlass, minstekravTxt=minstekravTxt,
    maalTxt=maalTxt, decreasing=decreasing,
    width=width, height=height, maalretn=maalretn)

}


#' @title Plot Indicator for Muskel Data
#'
#' @description
#' This function generates a plot for the given indicator data related to muskel analysis.
#'
#' @param indikatordata A data frame or tibble containing the indicator data to be plotted.
#'
#' @details
#' The function is designed to visualize the indicator data in a meaningful way.
#' Ensure that the input data is preprocessed and formatted correctly before passing it to this function.
#'
#' @return
#' A plot object representing the visualized indicator data.
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' muskelPlotIndikator(indikatordata = my_data)
#' }
#'
#' @export
muskelPlotIndikator <- function(indikatordata,
                                graaUt=NA,
                                lavDG=NA,
                                outfile = '') {

  indikator=indikatordata$indikator
  tittel=indikatordata$tittel
  terskel=indikatordata$terskel
  minstekrav=indikatordata$minstekrav
  maal=indikatordata$maal
  skriftStr=indikatordata$skriftStr
  pktStr=indikatordata$pktStr
  legPlass=indikatordata$legPlass
  minstekravTxt=indikatordata$minstekravTxt
  maalTxt=indikatordata$maalTxt
  decreasing=indikatordata$decreasing
  width=indikatordata$width
  height=indikatordata$height
  maalretn=indikatordata$maalretn

  indikator <- indikator |>
    dplyr::filter(year > max(year)-3) # behold bare siste 3 år

  Tabell <- indikator |>
    dplyr::summarise(Antall = sum(var),
                     N = dplyr::n(),
                     .by = c(Sykehus, year)) |>
    dplyr::group_by(year) |>
    dplyr::group_modify(~ .x |> janitor::adorn_totals(name = "Nasjonalt")) |>
    dplyr::mutate(Andel = Antall/N*100)

  N <- Tabell |>
    tidyr::pivot_wider(id_cols = Sykehus,
                       id_expand = TRUE,
                       names_from = year,
                       values_from = N,
                       values_fill = 0)
  andeler <- Tabell |>
    tidyr::pivot_wider(id_cols = Sykehus,
                       id_expand = TRUE,
                       names_from = year,
                       values_from = Andel)

  # Fjern år med færre registreringer enn terskelverdi og sykehus med
  # for lav dekningsgrad
  andeler[N < terskel] <- NA
  andeler[andeler$Sykehus %in% lavDG, -1] <- NA

  # Ordne rekkefølge, stigende eller synkende
  if (decreasing){
    rekkefolge <- order(andeler[[dim(andeler)[2]]],
                        decreasing = decreasing, na.last = F)
  } else {
    rekkefolge <- order(andeler[[dim(andeler)[2]]],
                        decreasing = decreasing, na.last = F)
  }

  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]

  # Skjul også tidligere år hvis siste år er sensurert pga. for få reg.
  # andeler[as.vector(N[, dim(andeler)[2]]<terskel), 2:3] <- NA
  andeler[as.vector(N[, dim(andeler)[2]]<terskel),
          2:(dim(andeler)[2]-1)] <- NA

  pst_txt <- paste0(sprintf('%.0f', purrr::as_vector(andeler[, dim(andeler)[2]])), ' %')
  pst_txt[N[, dim(andeler)[2]]<terskel] <- paste0('N<', terskel)
  pst_txt[andeler$Sykehus %in% lavDG] <- 'Dekningsgrad < 60 %'
  pst_txt <- c(NA, pst_txt, NA, NA)

  FigTypUt <- rapFigurer::figtype(outfile=outfile,
                                  width=width,
                                  height=height,
                                  pointsizePDF=11,
                                  fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(andeler)[1])
  soyleFarger[which(andeler$Sykehus=='Nasjonalt')] <- farger[4]
  if (!is.na(graaUt[1])) {
    soyleFarger[which(andeler$SykehusNavn %in% graaUt)] <- 'gray88'
  }
  soyleFarger <- c(NA, soyleFarger)

  # Lagre parameterverdier
  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr

  andeler <- rbind(andeler, c(NA,NA,NA,NA))
  andeler$Sykehus[dim(andeler)[1]] <- ''
  andeler <- rbind(c(NA,NA,NA,NA), andeler, c(NA,NA,NA,NA))
  andeler$Sykehus[dim(andeler)[1]] <- ''
  andeler$Sykehus[1] <- ' '

  vmarg <- max(0, strwidth(andeler$Sykehus,
                           units='figure', cex=cexgr)*0.75)
  # par('fig'=c(vmarg, 1, 0, 1))
  par('mar'=c(5.1, 9.1, 5.1, 9.1))

  xmax <- min(100, 1.15*max(andeler[,-1], na.rm = T))

  ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)')

  fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')

  if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=minstekrav, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=maal, ybottom=1, xright=minstekrav, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
    # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
    # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}


  barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
           names.arg=rep('',dim(andeler)[1]),
           horiz=T, axes=F, space=c(0,0.3),
           col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)

  title(main = tittel)
  ypos <- as.numeric(ypos) #as.vector(ypos)
  yposOver <- max(ypos)-2 + 0.5*diff(ypos)[1]
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, yposOver), col=fargerMaalNiva[2], lwd=2)
    par(xpd=TRUE)
    text(x=minstekrav, y=yposOver, labels = minstekravTxt,
         pos = 4, cex=cexgr*0.65, srt = 90)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, yposOver), col=fargerMaalNiva[1], lwd=2)
    barplot( t(andeler[, dim(andeler)[2]]), beside=T, las=1,
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=yposOver, labels = maalTxt, pos = 4, cex=cexgr*0.65, srt = 90) #paste0(maalTxt,maal,'%')
    par(xpd=FALSE)
  }

  axis(1,cex.axis=0.9)
  mtext( andeler$Sykehus, side=2, line=0.2,
         las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(andeler)[2]-1

  mtext( c(NA, purrr::as_vector(N[,2]), names(N)[2], NA, NA), side=4,
         line=2.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
  mtext( c(NA, purrr::as_vector(N[,3]), names(N)[3], NA, NA), side=4,
         line=5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
  mtext( c(NA, purrr::as_vector(N[,4]), names(N)[4], NA, NA), side=4,
         line=7.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
  mtext( 'N', side=4, line=5.0, las=1, at=max(ypos), col=1, cex=cexgr*.7, adj = 1)

  par(xpd=TRUE)
  points(y=ypos, x=purrr::as_vector(andeler[,2]),cex=pktStr) #'#4D4D4D'
  points(y=ypos, x=purrr::as_vector(andeler[,3]),cex=pktStr,pch= 19)
  par(xpd=FALSE)
  if (legPlass=='nede'){
    legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n', #bg='white', box.col='white',
           lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
           legend=names(N) )}
  if (legPlass=='top'){
    legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
           lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
           legend=names(N[,-1]), ncol = dim(andeler)[2]-1)
  }

  text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#
  if ( outfile != '') {dev.off()}



}









