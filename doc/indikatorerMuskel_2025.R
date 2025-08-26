
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


RegData <- muskel::MuskelHentRegData() |>
  muskel::MuskelPreprosess() |>
  dplyr::filter(Aar < 2025,
                Aar > 2007)

ind_id <- c("muskel_utredningstid",
            "muskel_genetisk_bekreftet_muskelsykdommer",
            "muskel_genetisk_bekreftet_sma",
            "muskel_genetisk_bekreftet_polynevropati",
            "muskel_genetisk_veiledning",
            "muskel_oppf_fysio",
            "muskel_oppf_hjerteoppf",
            "muskel_oppf_barnelege_nevrolog")


for (k in ind_id) {
  indikatordata <- muskelBeregnIndikator(RegData, k)
  outfile <- paste0(k, ".pdf")
  muskelPlotIndikator(indikatordata, outfile = outfile)
}

RegData$kjonn <- factor(RegData$erMann, levels = c(0,1),
                        labels = c("Kvinne", "Mann"))
gr <- c(0, seq(10, 80, 10), 120)	#c(0,16,31,46,61,76,200)
RegData$AlderGr <- cut(RegData$AlderVreg, breaks=gr,
                          include.lowest=TRUE, right=FALSE)

regdata_filtrert <- RegData |>
  dplyr::filter(Avdod == "Nei",
                ForlopsType1Num == 1,
                HovedDato < "2025-01-01")


MuskelUtvalg <- muskel::MuskelUtvalg(RegData=regdata_filtrert, avdod=F,
                                     reshID = 0, enhetsUtvalg = 0)
RegData <- MuskelUtvalg$RegData
AntHoved <- table(RegData[, c("kjonn", "AlderGr")])
NHoved <- rowSums(AntHoved)
utvalgTxt <- MuskelUtvalg$utvalgTxt
tittel <- "Alders- og kjønnsfordeling"
outfile <- "alder_kjonn.pdf"
FigTypUt <- rapFigurer::figtype(outfile=outfile, pointsizePDF=12)
pos <- barplot(AntHoved, beside=TRUE, las=1, ylab="Antall pasienter",
               sub="Aldersgrupper", cex.axis=1, cex.sub=1,	cex.lab=1, # ,	names.arg=grtxt, cex.names=cexgr,
               border='white', xaxt='n', col = FigTypUt$farger[1:2])
mtext(at=colMeans(pos), levels(RegData$AlderGr), side=1, las=1, cex=.8,
      adj=0.5, line=0.5)
legend('topright', c(paste0('Menn, N=', NHoved[1]),
                     paste0('Kvinner, N=', NHoved[2])), bty='n',
       fill=FigTypUt$farger[2:1], border=NA, ncol=1, cex=1)
title(main = tittel, line=1, font.main=1, cex.main=1.3)
if (outfile != "") {dev.off()}

## lag alder-kjønn også for diagnosegruppene.


####### SMA ######################################

SMAoversikt <- rapbase::loadRegData(
  registryName = "data",
  dbType = "mysql",
  query = "SELECT sma.*, m.PATIENT_ID
             FROM smafollowup sma LEFT JOIN mce m ON sma.MCEID = m.MCEID"
) |> dplyr::relocate(PATIENT_ID)

tabell_sma <- SMAoversikt %>%
  dplyr::mutate(ASSESSMENT_DATE = as.Date(ASSESSMENT_DATE)) %>%
  dplyr::filter(
    # ASSESSMENT_DATE >= req(input$datoFra2),
    #             ASSESSMENT_DATE <= req(input$datoTil2),
    #             STATUS %in% as.numeric(req(input$regstatus)),
                BEHANDLNG_SPINRAZA ==1) %>%
  dplyr::arrange(ASSESSMENT_DATE) %>%
  dplyr::summarise(
    ASSESSMENT_DATE_baseline = dplyr::first(ASSESSMENT_DATE),
    HFMSE_baseline = dplyr::first(KLINISK_HFMSE, order_by = ASSESSMENT_DATE),
    RULM_baseline = dplyr::first(KLINISK_RULM, order_by = ASSESSMENT_DATE),
    x6MWT_baseline = dplyr::first(KLINISK_6MWT, order_by = ASSESSMENT_DATE),
    ATEND_baseline = dplyr::first(KLINISK_ATEND, order_by = ASSESSMENT_DATE),
    BIPAP_baseline = dplyr::first(KLINISK_BIPAP, order_by = ASSESSMENT_DATE),
    FUNKSJONSSTATUS_baseline = dplyr::first(KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE),
    ASSESSMENT_DATE_latest = dplyr::last(ASSESSMENT_DATE),
    CENTREID = dplyr::last(CENTREID),
    HFMSE_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                          NA, dplyr::last(KLINISK_HFMSE, order_by = ASSESSMENT_DATE)),
    RULM_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                         NA, dplyr::last(KLINISK_RULM, order_by = ASSESSMENT_DATE)),
    x6MWT_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                          NA, dplyr::last(KLINISK_6MWT, order_by = ASSESSMENT_DATE)),
    ATEND_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                          NA, dplyr::last(KLINISK_ATEND, order_by = ASSESSMENT_DATE)),
    BIPAP_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                          NA, dplyr::last(KLINISK_BIPAP, order_by = ASSESSMENT_DATE)),
    FUNKSJONSSTATUS_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                                    NA, dplyr::last(KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE)),
    Tidsdiff_dager = difftime(ASSESSMENT_DATE_latest, ASSESSMENT_DATE_baseline, units = "days"),
    FUNKSJONSSTATUS_all = paste0(BEHANDLNG_FUNKSJONSSTATUS, collapse = ","),
    BEHANDLING_all = paste0(BEHANDLNG_BEHANDLING, collapse = ","),
    .by = PATIENT_ID) %>%
  dplyr::mutate(year = lubridate::year(ASSESSMENT_DATE_baseline)) |>
  dplyr::filter(Tidsdiff_dager != 0)


tabell_sma_antall <- SMAoversikt %>%
  dplyr::mutate(ASSESSMENT_DATE = as.Date(ASSESSMENT_DATE),
                year = lubridate::year(ASSESSMENT_DATE)) %>%
  # dplyr::filter(
  #   BEHANDLNG_SPINRAZA ==1) %>%
  dplyr::arrange(ASSESSMENT_DATE) %>%
  dplyr::summarise(N = dplyr::n(),
                   .by = c(year, CENTREID)) |>
  tidyr::pivot_wider(id_cols = CENTREID, values_from = N, names_from = year) |>
  dplyr::mutate(CENTREID = kobl_resh_orgnr$Sykehus[match(CENTREID, kobl_resh_orgnr$ReshID)])

## Lag med unike pasienter





