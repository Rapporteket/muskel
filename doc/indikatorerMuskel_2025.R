
kobl_resh_orgnr <- tibble::tribble(
  ~ReshID, ~Sykehus, ~orgnr, ~RHF,
  100065,          "Helgelandssykehuset", 983974929, "Helse Nord",
  100082,                 "Helse Bergen", 983974724, "Helse Vest",
  100083,              "Helse Stavanger", 983974678, "Helse Vest",
  100084,                  "Helse Fonna", 983974694, "Helse Vest",
  100085,                  "Helse Førde", 983974732, "Helse Vest",
  100089,                         "Ahus", 983971636, "Helse Sør-Øst",
  100091,          "Sykehuset Innlandet", 983971709, "Helse Sør-Øst",
  100092,            "Sykehuset Østfold", 983971768, "Helse Sør-Øst",
  100093,              "Sunnaas sykehus", 883971752, "Helse Sør-Øst",
  100100,         "Sykehuset i Vestfold", 983975259, "Helse Sør-Øst",
  100132,           "Sykehuset Telemark", 983975267, "Helse Sør-Øst",
  100133,            "Sørlandet sykehus", 983975240, "Helse Sør-Øst",
  100317,         "Helse Nord-Trøndelag", 983974791, "Helse Midt",
  100320,           "St. Olavs Hospital", 883974832, "Helse Midt",
  101051,           "Nordlandssykehuset", 983974910, "Helse Nord",
  101719,                          "UNN", 983974899, "Helse Nord",
  101971,           "Finnmarkssykehuset", 983974880, "Helse Nord",
  4001031,                         "OUS", 993467049, "Helse Sør-Øst",
  4201115,       "Helse Møre og Romsdal", 997005562, "Helse Midt",
  700272,                 "Vestre Viken", 894166762, "Helse Sør-Øst",
  960001,      "Privat spesialistsenter", 888888, "Annet/privat",
  960002,                   "Legekontor", 999999, "Annet/privat",
  960003,        "Rehabiliteringssenter", 333333333, "Annet/privat"
)



RegData <- muskel::MuskelHentRegData() |>
  muskel::MuskelPreprosess() |>
  dplyr::filter(Aar < 2025,
                Aar > 2007)
RegDataAll <- RegData

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
  outfile <- paste0(k, ".svg")
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


MuskelUtvalg <- muskel::MuskelUtvalg(
  RegData=regdata_filtrert, avdod=F,
  reshID = 0, enhetsUtvalg = 0)
RegData <- MuskelUtvalg$RegData
AntHoved <- table(RegData[, c("kjonn", "AlderGr")])
NHoved <- rowSums(AntHoved)
utvalgTxt <- MuskelUtvalg$utvalgTxt
tittel <- "Alders- og kjønnsfordeling"
outfile <- "alder_kjonn.svg"
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


MuskelUtvalg <- muskel::MuskelUtvalg(
  RegData=regdata_filtrert, avdod=F,
  reshID = 0, enhetsUtvalg = 0, diagnosegr = 1)
RegData <- MuskelUtvalg$RegData
AntHoved <- table(RegData[, c("kjonn", "AlderGr")])
NHoved <- rowSums(AntHoved)
utvalgTxt <- MuskelUtvalg$utvalgTxt
tittel <- "Alders- og kjønnsfordeling"
outfile <- "alder_kjonn_muskelsyke.svg"
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

MuskelUtvalg <- muskel::MuskelUtvalg(
  RegData=regdata_filtrert, avdod=F,
  reshID = 0, enhetsUtvalg = 0, diagnosegr = 2)
RegData <- MuskelUtvalg$RegData
AntHoved <- table(RegData[, c("kjonn", "AlderGr")])
NHoved <- rowSums(AntHoved)
utvalgTxt <- MuskelUtvalg$utvalgTxt
tittel <- "Alders- og kjønnsfordeling"
outfile <- "alder_kjonn_sma.svg"
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


MuskelUtvalg <- muskel::MuskelUtvalg(
  RegData=regdata_filtrert, avdod=F,
  reshID = 0, enhetsUtvalg = 0, diagnosegr = 3)
RegData <- MuskelUtvalg$RegData
AntHoved <- table(RegData[, c("kjonn", "AlderGr")])
NHoved <- rowSums(AntHoved)
utvalgTxt <- MuskelUtvalg$utvalgTxt
tittel <- "Alders- og kjønnsfordeling"
outfile <- "alder_kjonn_polynevro.svg"
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


tabell_sma_antall_reg <- SMAoversikt  |>
  dplyr::mutate(FILL_DATE = as.Date(FILL_DATE),
                year = lubridate::year(FILL_DATE)) |>
  dplyr::filter(
    year < 2025) |>
  dplyr::arrange(FILL_DATE) |>
  dplyr::summarise(N = dplyr::n(),
                   .by = c(year, CENTREID)) |>
  tidyr::pivot_wider(id_cols = CENTREID, values_from = N, names_from = year) |>
  dplyr::mutate(CENTREID = kobl_resh_orgnr$Sykehus[
    match(CENTREID, kobl_resh_orgnr$ReshID)])

## Lag med unike pasienter

tabell_sma_antall_pasienter <- SMAoversikt |>
  dplyr::mutate(ASSESSMENT_DATE = as.Date(FILL_DATE),
                year = lubridate::year(ASSESSMENT_DATE)) |>
  dplyr::filter(
    year < 2025) |>
  dplyr::arrange(FILL_DATE) |>
  dplyr::summarise(N = length(unique(PATIENT_ID)),
                   .by = c(year, CENTREID)) |>
  tidyr::pivot_wider(id_cols = CENTREID, values_from = N, names_from = year) |>
  dplyr::mutate(CENTREID = kobl_resh_orgnr$Sykehus[
    match(CENTREID, kobl_resh_orgnr$ReshID)])


plot_long_sma_reg <- tabell_sma_antall_reg |>
  pivot_longer(cols = names(tabell_sma_antall_reg)[-1], names_to = "År",
               values_to = "antall_reg")
plot_long_sma_pas <- tabell_sma_antall_pasienter |>
  pivot_longer(cols = names(tabell_sma_antall_pasienter)[-1], names_to = "År",
               values_to = "antall_pas")
plot_long_sma <- merge(plot_long_sma_reg, plot_long_sma_pas,
                       by = c("CENTREID", "År")) |>
  pivot_longer(cols = c("antall_reg", "antall_pas"),
               names_to = "Enhet", values_to = "Antall") |>
  mutate(Antall = ifelse(is.na(Antall), 0, Antall))

plotobjekt_sma <- plot_long_sma |>
  ggplot(aes(År, Antall,
             colour = CENTREID,
             linetype = Enhet,
             group = interaction(CENTREID,Enhet))) +
  geom_line() +
  theme_classic() +
  labs(title = "Antall pasienter/registreringer SMA")

ggsave(filename = "sma_reg.svg",
       plot = plotobjekt_sma)

write.csv2(tabell_sma_antall_reg, "tabell_sma_antall_reg.csv", row.names = F,
           fileEncoding = "Latin1", na = "")
write.csv2(tabell_sma_antall_pasienter, "tabell_sma_antall_pas.csv", row.names = F,
           fileEncoding = "Latin1", na = "")



########## Andel missing kval ind

# c("TidUtredDiag", "GenetiskAarsakPaavist", "OppfolgBarnelegeNevrolog",
#   "Fysioterapi", "Hjerteoppfoelging", "TilbudGenetiskVeiledning")
#
# table(RegData$TilbudGenetiskVeiledning, useNA = 'ifany')

missing <- RegData |> dplyr::summarise(
  N = dplyr::n(),
  TidUtredDiag_missing = sum(is.na(TidUtredDiag))/N*100,
  GenetiskAarsakPaavist_missing = sum(is.na(GenetiskAarsakPaavist))/N*100,
  OppfolgBarnelegeNevrolog_missing = sum(is.na(OppfolgBarnelegeNevrolog) |
                                           OppfolgBarnelegeNevrolog == 9)/N*100,
  Fysioterapi_missing = sum(is.na(Fysioterapi) | Fysioterapi == 9)/N*100,
  Hjerteoppfoelging_missing = sum(is.na(Hjerteoppfoelging) |
                                    Hjerteoppfoelging == 9)/N*100,
  TilbudGenetiskVeiledning_missing = sum(is.na(TilbudGenetiskVeiledning) |
                                           TilbudGenetiskVeiledning == 9)/N*100,
  .by = Aar
) |> dplyr::arrange(Aar)



plot_long <- missing |> select(-N) |>
  pivot_longer(cols = names(missing)[-c(1,2)], names_to = "variabel")

plotobjekt <- plot_long |>
  filter(Aar > 2016) |>
  ggplot(aes(Aar, value, colour = variabel, group = variabel)) +
  geom_line() +
  theme_classic() +
  scale_x_continuous(breaks = min(plot_long$Aar):max(plot_long$Aar)) +
  labs(title = "Andel missing sentrale variabler",
       x = "Registreringsår",
       y = "Andel %")

ggsave(filename = "missing_v2.svg",
       plot = plotobjekt)

demografi <- RegDataAll |> mutate(RHF = kobl_resh_orgnr$RHF[
  match(AvdRESH, kobl_resh_orgnr$ReshID)]) |>
  summarise(Antall = n(),
            gjsn.alder.v.reg. = mean(AlderVreg),
            .by = c(Aar, RHF, Diagnosegr_label))

write.csv2(demografi, "demografi.csv", row.names = F,
           fileEncoding = "Latin1", na = "")

levende <- RegDataAll |>
  mutate(RHF = kobl_resh_orgnr$RHF[match(AvdRESH, kobl_resh_orgnr$ReshID)],
         alder2024utgang = age(Foedselsdato, "2024-12-31")) |>
  filter(Avdod == "Nei" | (Avdod == "Ja" & AvdodDato >= "2025-01-01")) |>
  filter(ForlopsID == max(ForlopsID), .by = PasientID) |>
  summarise(Antall = n(),
            gjsn.alder.utgang2024 = mean(alder2024utgang),
            .by = c(RHF, Diagnosegr_label))

write.csv2(levende, "alder_levende_2024.csv", row.names = F,
           fileEncoding = "Latin1", na = "")






