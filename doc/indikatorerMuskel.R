library(muskel)
library(tidyverse)
rm(list = ls())

ForlopsData <- read.table('I:/muskel/ForlopsOversikt2019-05-22 12-50-07.txt', header=TRUE, sep=';', encoding = 'UTF-8')
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2019-05-22 12-44-01.txt', header=TRUE, sep=';', encoding = 'UTF-8')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak', 'Hjerteoppfoelging',
                        'KognitivSvikt', 'MedikBehandling', 'Smertestillende', "Antiarytmika", "ACEHemmer", "AnnetHjerteMed",
                        "AnnenMedikBeh", "OppfolgBarnelegeNevrolog", "PsykiskHelsetjeneste", "OppholdRehab", "TilbudKostveiledning",
                        "TilbudGenetiskVeiledning", "AnsvarsgruppeIP", "BPA", "Arbeid", "SympFamilie", "TrygdFraAlder", "Kardiomyopati",
                        "Hjertearytmi", "HjerteAffAnnet", "EKG", "HyppighetEKG", "HyppighetRytmereg", "Ultralyd", "HyppighetUltralyd", "AarstallGenAarsak")]
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2019-05-22 12-43-59.txt', header=TRUE, sep=';', encoding = 'UTF-8')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
RegData <- MuskelPreprosess(RegData=RegData)


### 1.	Andel pasienter med fastsatt diagnose under ett år etter utredningsstart


tmp <- RegData %>%
  group_by(PasientID) %>%
  summarise(max_TidUtredDiag = max(TidUtredDiag, na.rm = T),
            min_TidUtredDiag = min(TidUtredDiag, na.rm = T),
            Utredningsstart_max = Utredningsstart[which(TidUtredDiag == max(TidUtredDiag, na.rm = T))[1]],
            Utredningsstart_min = Utredningsstart[which(TidUtredDiag == min(TidUtredDiag, na.rm = T))[1]],
            DiagnoseAar_max = DiagnoseAar[which(TidUtredDiag == max(TidUtredDiag, na.rm = T))[1]],
            DiagnoseAar_min = DiagnoseAar[which(TidUtredDiag == min(TidUtredDiag, na.rm = T))[1]],
            # Utredningsstart_min = min(Utredningsstart, na.rm = T),
            # Utredningsstart_max = max(Utredningsstart, na.rm = T),
            ForlopsID_Utredningsstart_max = ForlopsID[which(TidUtredDiag == max(TidUtredDiag, na.rm = T))[1]],
            ForlopsID_Utredningsstart_min = ForlopsID[which(TidUtredDiag == min(TidUtredDiag, na.rm = T))[1]]) %>% ungroup()

length(unique(tmp$PasientID))


tmp$diff <- abs(tmp$max_TidUtredDiag - tmp$min_TidUtredDiag)
tmp$diff[tmp$diff == Inf] <- NA

sort(tmp$PasientID[which(tmp$Utredningsstart_min != tmp$Utredningsstart_max)])

# max(tmp$diff, na.rm = T)
#
tmp[which(tmp$diff !=0), ]
RegData[RegData$PasientID == as.numeric(tmp[which(tmp$diff !=0)[2], "PasientID"]),
        c("TidUtredDiag", "Debut", "Utredningsstart", "DiagnoseAar", "DiagICD10", "DiagnoseStiltAv", "PasientAlder", "DiagnoseAlder",
          "HovedDato", "PasientID", "ForlopsType1")]
#













