setwd('C:/GIT/muskel/doc/')
library(muskel)
rm(list=ls())

########### Antall SMA og DMD fylkesvis 24.04.2020 #####################
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2020-03-06 14-22-57.txt', header=TRUE, sep=';', stringsAsFactors = F,
                          fileEncoding = 'UTF-8')
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2020-03-06 14-22-57.txt', header=TRUE, sep=';', stringsAsFactors = F, fileEncoding = 'UTF-8')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "Undergruppe", "Utredningsstart",
                        "Undergruppe2", "Undergruppe2Spes", "DebutAlder", "DiagnoseAar")] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2020-03-06 14-22-57.txt', header=TRUE, sep=';', stringsAsFactors = F,encoding = 'UTF-8')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "Undergruppe", "Undergruppe2")]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)

aux <- RegData[which(RegData$Diagnosegr_label == "Spinal muskelatrofi", RegData$Avdod == 'Nei'), ]

aux <- aux[order(aux$HovedDato, decreasing = TRUE), ]
aux <- aux[match(unique(aux$PasientID), aux$PasientID), ]

aux$under18 <- NA
aux$under18[which(aux$Foedselsdato > '2002-03-01')] <- 1
aux$under18[which(aux$Foedselsdato <= '2002-03-01')] <- 0

icd10_18pluss <- addmargins(table(aux[aux$under18==0, c("Fylke", "DiagICD10")], useNA = 'ifany'))
undergruppe_18pluss <- addmargins(table(aux[aux$under18==0, c("Fylke", "Undergruppe_label")], useNA = 'ifany'))

write.csv2(icd10_18pluss, paste0('muskel_icd_', Sys.Date(), '.csv'), row.names = T)
write.csv2(undergruppe_18pluss, paste0('muskel_undergruppe_', Sys.Date(), '.csv'), row.names = T)



########### 21.04.2020 Kompletthet av utvalgte variabler ####################################################################
library(tidyverse)
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2020-03-06 14-22-57.txt', header=TRUE, sep=';', encoding = 'UTF-8')
RegData <- read.table('I:/muskel/AlleVarNum2020-03-06 14-22-57.txt', header=TRUE, sep=';', encoding = 'UTF-8')
RegDataLabel <- read.table('I:/muskel/AlleVar2020-03-06 14-22-57.txt', header=TRUE, sep=';', encoding = 'UTF-8')

komplettvar <- RegData[ , c("ForlopsID", "PasientID", "DebutAlder", "HjerteAff", "Hjerteoppfoelging",
                            "AnsvarsgruppeIP", "TilbudGenetiskVeiledning", "GenetiskAarsakPaavist",
                            "Fysioterapi", "OppholdRehab", "Ergoterapi", "PsykiskHelsetjeneste")]
ForlopsData <- ForlopsData[, c("ForlopsID", "HovedDato")]
komplettvar <- merge(komplettvar, ForlopsData, by = "ForlopsID")
komplettvar$HovedDato <- as.Date(komplettvar$HovedDato)
komplettvar <- komplettvar[komplettvar$HovedDato >= '2018-01-01' & komplettvar$HovedDato <= '2019-12-31', ]
komplettvar$Aar <- format(komplettvar$HovedDato, "%Y")
komplettvar <- komplettvar[, -13]

tmp <- komplettvar[komplettvar$Aar==2018,-c(1,2, 13)] %>%
  summarise_all(list(ukjent = function(x) sum(x==9, na.rm = T)/n()*100,
                     missing =function(x) sum(is.na(x))/n()*100)) %>%
  gather() %>%
  separate(col="key", into=c("Variabel", "Status"), sep="_") %>%
  spread(key = Status, value = value)

tmp$N <- dim(komplettvar[komplettvar$Aar==2018,])[1]
tall2018 <- tmp

tmp <- komplettvar[komplettvar$Aar==2019,-c(1,2, 13)] %>%
  summarise_all(list(ukjent = function(x) sum(x==9, na.rm = T)/n()*100,
                     missing =function(x) sum(is.na(x))/n()*100)) %>%
  gather() %>%
  separate(col="key", into=c("Variabel", "Status"), sep="_") %>%
  spread(key = Status, value = value)

tmp$N <- dim(komplettvar[komplettvar$Aar==2019,])[1]
tall2019 <- tmp

tall2018$missing <- round(tall2018$missing, 1)
tall2018$ukjent <- round(tall2018$ukjent, 1)
tall2019$missing <- round(tall2019$missing, 1)
tall2019$ukjent <- round(tall2019$ukjent, 1)

# length(unique(komplettvar$PasientID))
# komplettvar %>% summarise(missing_debut = sum(is.na(DebutAlder)),
#                           missing_hjerteaff = sum(is.na(HjerteAff)),
#                           ukjent_hjerteaff = sum(HjerteAff==9),
#                           missing_hjerteoppf = sum(is.na(Hjerteoppfoelging)),
#                           ukjent_hjerteoppf = sum(Hjerteoppfoelging==9),
#                           missing_AnsvarsgruppeIP = sum(is.na(AnsvarsgruppeIP)),
#                           ukjent_AnsvarsgruppeIP = sum(AnsvarsgruppeIP==9),
#                           missing_TilbudGenetiskVeiledning = sum(is.na(TilbudGenetiskVeiledning)),
#                           ukjent_TilbudGenetiskVeiledning = sum(TilbudGenetiskVeiledning==9),
#                           missing_GenetiskAarsakPaavist = sum(is.na(GenetiskAarsakPaavist)),
#                           ukjent_GenetiskAarsakPaavist = sum(GenetiskAarsakPaavist==9),
#                           missing_Fysioterapi = sum(is.na(Fysioterapi)),
#                           ukjent_Fysioterapi = sum(Fysioterapi==9),
#                           missing_OppholdRehab = sum(is.na(OppholdRehab)),
#                           ukjent_OppholdRehab = sum(OppholdRehab==9),
#                           missing_Ergoterapi = sum(is.na(Ergoterapi)),
#                           ukjent_Ergoterapi = sum(Ergoterapi==9),
#                           missing_PsykiskHelsetjeneste = sum(is.na(PsykiskHelsetjeneste)),
#                           ukjent_PsykiskHelsetjeneste = sum(PsykiskHelsetjeneste==9),
#                           antall_reg = n())

####### Utlevering 08.11.2019 Fødselnummer, pid og adresse pas under 16 år ###################################################
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2019-11-08 10-10-17.txt', header=TRUE, sep=';', encoding = 'UTF-8')
RegData <- read.table('I:/muskel/AlleVarNum2019-11-08 10-10-14.txt', header=TRUE, sep=';', encoding = 'UTF-8')
RegDataLabel <- read.table('I:/muskel/AlleVar2019-11-08 10-10-11.txt', header=TRUE, sep=';', encoding = 'UTF-8')

ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
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

RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
RegData <- MuskelPreprosess(RegData=RegData)


utlevering <- RegData[RegData$Alder>=16 & RegData$Avdod=='Nei', c("PasientID", "Alder")]
utlevering <- utlevering[match(unique(utlevering$PasientID), utlevering$PasientID), ]

persondata <- read.table('I:/muskel/PersjonRapport_Muskel_08112019.csv', header=TRUE, sep=';', encoding = 'UTF-8',
                         colClasses = "character")
persondata <- persondata[match(unique(persondata$PasientID), persondata$PasientID), ]

utlevering <- merge(utlevering, persondata, by = 'PasientID')

write.csv2(utlevering, 'I:/muskel/muskelpas16pluss_08112019.csv', row.names = F)

####### Utlevering Kai 12.09.2019 Fødselnummer og diagnoser Nord Norge ###################################################
RegData <- read.table('I:/muskel/AlleVarNum2019-08-19 13-29-40.txt', header=TRUE, sep=';', stringsAsFactors = F)
admdata <- read.table('I:/muskel/ForlopsOversikt2019-08-19 13-30-02.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- merge(RegData, admdata[, c("ForlopsID", "HovedDato", "Fylke", "PasientKjonn", "PasientAlder", "Sivilstatus", "Utdanning")], by = 'ForlopsID')
persondata <- read.table('I:/muskel/PersjonRapport_Muskel_20082019.csv', header=TRUE, sep=';', encoding = 'UTF-8',
                         colClasses = "character")
persondata <- persondata[match(unique(persondata$PasientID), persondata$PasientID), ]

utvalg <- RegData[which(RegData$Avdoed == 0 & RegData$RegStatus==1 & RegData$DiagICD10!=''), ]
utvalg <- utvalg[order(as.Date(utvalg$HovedDato), decreasing = T), ]
utvalg <- utvalg[match(unique(utvalg$PasientID), utvalg$PasientID), ]
utvalg <- utvalg[utvalg$Fylke %in% c('NORDLAND', 'TROMS', 'FINNMARK'), ]

utvalg <- merge(utvalg[, c("PasientID", "DiagICD10", "HovedDato")], persondata[, c("PasientID", "Personnummer")], by = 'PasientID')

write.csv2(utvalg, 'I:/muskel/kai_12092019.csv', row.names = F)


###### Liste over pasienter med DM1 20.08.2019 ###########################################################################
persondata <- read.table('I:/muskel/PersjonRapport_Muskel_20082019.csv', header=TRUE, sep=';', encoding = 'UTF-8',
                         colClasses = "character")
RegData <- read.table('I:/muskel/AlleVarNum2019-08-19 13-29-40.txt', header=TRUE, sep=';', stringsAsFactors = F)
utvalg <- RegData[which(RegData$Undergruppe == 20 & RegData$Avdoed == 0 & RegData$RegStatus==1), ]

persondata <- persondata[which(persondata$PasientID %in% unique(utvalg$PasientID)), ]
persondata <- persondata[match(unique(persondata$PasientID), persondata$PasientID), ]

write.csv2(persondata, 'I:/muskel/DM1.csv', row.names = F)

###### Lister til dekningsgradsanalyser 03.04.2019 - redigert 24.04.2019 #################################################
persondata <- read.table('I:/muskel/PersjonRapport_Muskel05042019.csv', header=TRUE, sep=';', encoding = 'UTF-8',
                         colClasses = "character")
persondata <- persondata[, c("Personnummer", "PasientID")]
persondata$PasientID <- as.numeric(persondata$PasientID)
persondata <- persondata[match(unique(persondata$PasientID), persondata$PasientID), ]

ForlopsData <- read.table('I:/muskel/ForlopsOversikt2019-04-05 13-14-23.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "Fylke", "Fylkenr", "Avdod", "ForlopsType1",
                               "Kommune", "Kommunenr")]
RegData <- read.table('I:/muskel/AlleVarNum2019-04-05 13-14-20.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("ForlopsID", "PasientID", "DiagICD10")]
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegData$HovedDato <- as.Date(RegData$HovedDato)
RegData <- RegData[RegData$HovedDato < '2019-01-01', ]
rm(list=c('ForlopsData'))
# Fjerner avdøde
RegData <- RegData[RegData$Avdod == 'Nei', ]

# RegData <- RegData[RegData$DiagICD10 != '', ]

# Velger nyeste registrering per pasient
RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]
RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')
RegData$Kommune <- iconv(RegData$Kommune, from = 'UTF-8', to = '')

RegData <- RegData[, c("PasientID", "DiagICD10", "Fylke", "Kommune", "Kommunenr", "HovedDato")]
persondata <- persondata[persondata$PasientID %in% RegData$PasientID, ]

write.csv2(RegData, 'I:/muskel/aktivitet_muskel_v2.csv', row.names = F)
write.csv2(persondata, 'I:/muskel/kobling.csv', row.names = F)

##### Liste over hvem som registrerer 03.04.2019  ##################################################

skjemaoversikt <- read.table('I:/muskel/SkjemaOversikt2019-03-26 14-57-31.txt', header=TRUE, sep=';', stringsAsFactors = F)
skjemaoversikt$Sykehusnavn <- iconv(skjemaoversikt$Sykehusnavn, from = 'UTF-8', to = '')
skjemaoversikt$Skjemanavn <- iconv(skjemaoversikt$Skjemanavn, from = 'UTF-8', to = '')
skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
skjemaoversikt$Aar <- format(skjemaoversikt$HovedDato, '%Y')

aux <- skjemaoversikt[skjemaoversikt$SkjemaStatus==1, ]
tmp <- addmargins(table(aux$OpprettetAv, aux$Aar, useNA = 'ifany'), 2)
tmp <- as_tibble(as.data.frame.matrix(tmp), rownames='bruker')
tmp <- tmp[order(tmp$Sum, decreasing = T), ]
write.csv2(tmp, 'muskelreg.csv', row.names = F)


########### Sjekk tall tidligere levert Kristin Ørstavik 31.10.2018 #####################
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2018-10-31 09-56-09.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2018-10-31 09-56-06.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "Undergruppe", "Utredningsstart",
                        "Undergruppe2", "Undergruppe2Spes", "DebutAlder", "DiagnoseAar")] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2018-10-31 09-56-04.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "Undergruppe", "Undergruppe2")]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')

aux <- RegData[which(RegData$Undergruppe %in% c(1, 81, 20), RegData$Avdod == 'Nei'), ]

aux <- aux[order(aux$HovedDato, decreasing = TRUE), ]
aux <- aux[match(unique(aux$PasientID), aux$PasientID), ]

aux$under18 <- NA
aux$under18[which(aux$Foedselsdato > '2000-10-31')] <- 1
aux$under18[which(aux$Foedselsdato <= '2000-10-31')] <- 0

under18 <- table(aux[aux$under18==1, c("Fylke", "Undergruppe_label")], useNA = 'ifany')
over18 <- table(aux[aux$under18==0, c("Fylke", "Undergruppe_label")], useNA = 'ifany')

under18_v2 <- table(aux[aux$Alder<=18, c("Fylke", "Undergruppe_label")], useNA = 'ifany')
over18 <- table(aux[aux$under18==0, c("Fylke", "Undergruppe_label")], useNA = 'ifany')



########### Kristin Ørstavik: Antall SMA og DMD fylkesvis 17.10.2018 #####################
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2018-10-08 09-46-18.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2018-10-08 09-46-16.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "Undergruppe", "Utredningsstart",
                        "Undergruppe2", "Undergruppe2Spes", "DebutAlder", "DiagnoseAar")] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2018-10-08 09-46-13.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "Undergruppe", "Undergruppe2")]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')

aux <- RegData[which(RegData$Undergruppe %in% c(1, 81, 20), RegData$Avdod == 'Nei'), ]

aux <- aux[order(aux$HovedDato, decreasing = TRUE), ]
aux <- aux[match(unique(aux$PasientID), aux$PasientID), ]

aux$under18 <- NA
aux$under18[which(aux$Foedselsdato > '2000-10-08')] <- 1
aux$under18[which(aux$Foedselsdato <= '2000-10-08')] <- 0

under18 <- table(aux[aux$under18==1, c("Fylke", "Undergruppe_label")], useNA = 'ifany')
over18 <- table(aux[aux$under18==0, c("Fylke", "Undergruppe_label")], useNA = 'ifany')

write.csv2(under18, 'under18.csv', row.names = T)
write.csv2(over18, 'over18.csv', row.names = T)



########### Kristin Ørstavik: Antall SMA og DMD fylkesvis 16.10.2018 #####################
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2018-10-08 09-46-18.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2018-10-08 09-46-16.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "Undergruppe", "Utredningsstart",
                        "Undergruppe2", "Undergruppe2Spes", "DebutAlder", "DiagnoseAar")] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2018-10-08 09-46-13.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "Undergruppe", "Undergruppe2")]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')

aux <- RegData[which(RegData$Undergruppe== c(1,81), RegData$Avdod == 'Nei'), ]  #### FEIL!!!!!!!! skal være RegData$Undergruppe %in% c(1,81)

aux <- aux[order(aux$HovedDato, decreasing = TRUE), ]
aux <- aux[match(unique(aux$PasientID), aux$PasientID), ]

aux$under18 <- NA
aux$under18[which(aux$Foedselsdato > '2000-10-08')] <- 1
aux$under18[which(aux$Foedselsdato <= '2000-10-08')] <- 0

under18 <- table(aux[aux$under18==1, c("Fylke", "Undergruppe_label")], useNA = 'ifany')
over18 <- table(aux[aux$under18==0, c("Fylke", "Undergruppe_label")], useNA = 'ifany')

write.csv2(under18, 'under18.csv', row.names = T)
write.csv2(over18, 'over18.csv', row.names = T)

########### Kristin Ørstavik: Antall SMA og DMD fylkesvis 08.10.2018 #####################
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2018-10-08 09-46-18.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2018-10-08 09-46-16.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "Undergruppe", "Utredningsstart",
                        "Undergruppe2", "Undergruppe2Spes", "DebutAlder", "DiagnoseAar")] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2018-10-08 09-46-13.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "Undergruppe", "Undergruppe2")]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')

aux <- RegData[which(RegData$Undergruppe==1 | RegData$DiagICD10 == 'G12.1', RegData$Avdod == 'Nei'), ]

aux <- aux[order(aux$HovedDato, decreasing = TRUE), ]
aux <- aux[match(unique(aux$PasientID), aux$PasientID), ]

aux$under18 <- NA
aux$under18[which(aux$Foedselsdato > '2000-10-08')] <- 1
aux$under18[which(aux$Foedselsdato <= '2000-10-08')] <- 0

under18 <- table(aux[aux$under18==1, c("Fylke", "DiagICD10")], useNA = 'ifany')
over18 <- table(aux[aux$under18==0, c("Fylke", "DiagICD10")], useNA = 'ifany')

write.csv2(under18, 'under18.csv', row.names = T)
write.csv2(over18, 'over18.csv', row.names = T)

########### Dekningsgradtall fylkesvis, Robert Wiik 30.05.2018 #####################
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2018-05-30 10-26-04.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2018-05-30 10-26-02.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "Undergruppe", "Utredningsstart",
                        "Undergruppe2", "Undergruppe2Spes", "DebutAlder", "DiagnoseAar")] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2018-05-30 10-25-59.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "Undergruppe", "Undergruppe2")]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)

# Fjerner avdøde og registreringer etter 2017
RegData <- RegData[RegData$Avdod == 'Nei' & RegData$Aar < 2018, ]

# Velger nyeste registrering per pasient
RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]

RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')

pas.pr.fylke <- aggregate(RegData$PasientID, by=list(Fylke = RegData$Fylke), length)

write.csv2(pas.pr.fylke, 'muskel_fylkesvis_2018.csv', row.names = F)



########### Global FKRP Registry - nye pasienter over 16 år ######################
ForlopsData <- read.table('I:/muskel/ForlopsOversikt2018-05-11 09-03-38.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2018-05-11 09-03-35.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("GenetiskAarsakPaavist", "ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar",
                        "Utredningsstart", "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('I:/muskel/AlleVar2018-05-11 09-03-33.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv", "GenetiskAarsakPaavist",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
RegData <- RegData[RegData$Avdod == 'Nei', ]

LGMD2i <- RegData[which(RegData$DiagICD10 == 'G71.0' & RegData$Undergruppe==4 & RegData$Undergruppe2==13), ]

LGMD2i <- LGMD2i[order(LGMD2i$HovedDato, decreasing = TRUE), ]
LGMD2i <- LGMD2i[match(unique(LGMD2i$PasientID), LGMD2i$PasientID), ]

library(lubridate)
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}

LGMD2i$Alder <- age(LGMD2i$Foedselsdato)
LGMD2i <- LGMD2i[order(LGMD2i$Foedselsdato, decreasing = T), ]

write.csv2(LGMD2i[, c("PasientID", "Foedselsdato", "Alder")], 'fkrp11052018.csv', row.names = F)

########### Duchenne-tall 16.02.2018 ##############################################
library(muskel)
rm(list=ls())

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2018-01-04 12-37-31.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2018-01-04 12-37-28.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("GenetiskAarsakPaavist", "ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar",
                        "Utredningsstart", "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-10-23 11-04-36.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv", "GenetiskAarsakPaavist",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
RegData <- RegData[RegData$Avdod == 'Nei', ] # Kun nålevende

RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering
# RegData <- RegData[which(RegData$Alder) >= 18, ]

RegData$Undergruppe_label <- iconv(RegData$Undergruppe_label, from = 'UTF-8', to = '')
RegData$Undergruppe2_label <- iconv(RegData$Undergruppe2_label, from = 'UTF-8', to = '')
RegData$AngiGenetiskAarsak <- iconv(RegData$AngiGenetiskAarsak, from = 'UTF-8', to = '')

RegData$Undergruppe[which(RegData$Undergruppe %in% 1:2)]


RegData <- RegData[which(RegData$Undergruppe %in% c(70, 81, 82, 83)), ]
gr <- c(70, 81, 82, 83)
grtxt <- RegData$Undergruppe_label[match(gr, RegData$Undergruppe)]
RegData$VariabelGr <- factor(RegData$Undergruppe, levels = gr, labels = grtxt)
RegData$GenetiskAarsakPaavist <- factor(RegData$GenetiskAarsakPaavist, levels = c(0,1,9), labels = c('Nei', 'Ja', 'Ikke utfylt'))

RegData <- RegData[which(RegData$Alder >=18), ]

table(RegData$VariabelGr, useNA = 'ifany')




########### SMA-tall 13.02.2018 ##############################################
library(muskel)
rm(list=ls())

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2018-01-04 12-37-31.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2018-01-04 12-37-28.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("GenetiskAarsakPaavist", "ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar",
                        "Utredningsstart", "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-10-23 11-04-36.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv", "GenetiskAarsakPaavist",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
RegData <- RegData[RegData$Avdod == 'Nei', ] # Kun nålevende

RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering
# RegData <- RegData[which(RegData$Alder) >= 18, ]

RegData$Undergruppe_label <- iconv(RegData$Undergruppe_label, from = 'UTF-8', to = '')
RegData$Undergruppe2_label <- iconv(RegData$Undergruppe2_label, from = 'UTF-8', to = '')
RegData$AngiGenetiskAarsak <- iconv(RegData$AngiGenetiskAarsak, from = 'UTF-8', to = '')

RegData <- RegData[which(RegData$Undergruppe %in% c(70, 81, 82, 83)), ]
gr <- c(70, 81, 82, 83)
grtxt <- RegData$Undergruppe_label[match(gr, RegData$Undergruppe)]
RegData$VariabelGr <- factor(RegData$Undergruppe, levels = gr, labels = grtxt)
RegData$GenetiskAarsakPaavist <- factor(RegData$GenetiskAarsakPaavist, levels = c(0,1,9), labels = c('Nei', 'Ja', 'Ikke utfylt'))

RegData <- RegData[which(RegData$Alder >=18), ]

table(RegData$VariabelGr, useNA = 'ifany')

# addmargins(table(RegData$GenetiskAarsakPaavist, RegData$VariabelGr, useNA = 'ifany'))


########### Pasienter ved HUS per ICD10-kode, over og under 18 ##########################

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2018-01-04 12-37-31.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2018-01-04 12-37-28.txt', header=TRUE, sep=';')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2018-01-04 12-37-26.txt', header=TRUE, sep=';')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))

RegData <- MuskelPreprosess(RegData)

RegData <- RegData[RegData$SykehusNavn == 'Helse Bergen HF', ]

RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering

table(as.character(RegData$DiagICD10[RegData$Alder<18]), useNA = 'ifany')

table(as.character(RegData$DiagICD10[RegData$Alder>=18]), useNA = 'ifany')



########### Pasienter med sepn1-mutasjon ##########################

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2017-11-30 10-45-11.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2017-11-30 10-45-09.txt', header=TRUE, sep=';')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-11-30 10-45-06.txt', header=TRUE, sep=';')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))

RegData <- MuskelPreprosess(RegData)

RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')
RegData$AngiGenetiskAarsak <- iconv(RegData$AngiGenetiskAarsak, from = 'UTF-8', to = '')

# RegData <- RegData[RegData$DiagICD10 %in% c('G71.0', 'G71.2', 'G71.8'), ]
streng <- RegData$AngiGenetiskAarsak[RegData$AngiGenetiskAarsak!='']
RegData$Alder[grep('sepn', RegData$AngiGenetiskAarsak, ignore.case = T)]
RegData$PasientAlder[grep('sepn', RegData$AngiGenetiskAarsak, ignore.case = T)]
# grep('Selenon', RegData$AngiGenetiskAarsak, ignore.case = T, value = T)
# grep('selenoprotein ', RegData$AngiGenetiskAarsak, ignore.case = T, value = T)

########### Utlevering, Fagråd 30.11.2017 - diagnoser på bostedsfylkenivå ##########################

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2017-11-30 10-45-11.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2017-11-30 10-45-09.txt', header=TRUE, sep=';')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-11-30 10-45-06.txt', header=TRUE, sep=';')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))

RegData <- MuskelPreprosess(RegData)

RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')

RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering

Utvalgte <- RegData[RegData$Undergruppe %in% c(70, 81:83, 1, 2, 20, 21, 4), ]
Utvalgte <- Utvalgte[Utvalgte$Avdod == 'Nei', ]

# utlevr <- table(as.character(Utvalgte$Undergruppe_label), as.character(Utvalgte$Fylke), useNA = 'ifany')
utlevr <- table(as.character(Utvalgte$Fylke), as.character(Utvalgte$Undergruppe_label), useNA = 'ifany')

write.csv2(utlevr, 'UtvalgteDiagnoserFylkesvis.csv', row.names = T)

# Utvalgte1000 <- Utvalgte[Utvalgte$Undergruppe==1000, ]
# Utvalgte1000$AngiGenetiskAarsak


########### Utlevering, Kristin Ørstavik 30.11.2017 ##########################

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2017-11-30 10-45-11.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2017-11-30 10-45-09.txt', header=TRUE, sep=';')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-11-30 10-45-06.txt', header=TRUE, sep=';')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))

RegData <- MuskelPreprosess(RegData)

RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')

RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering

LGMD2i <- RegData[which(RegData$DiagICD10 == 'G71.0' & RegData$Undergruppe==4 & RegData$Undergruppe2==13), ]
LGMD2i <- LGMD2i[LGMD2i$Alder>=18 & LGMD2i$Alder <=66, ]
LGMD2i <- LGMD2i[LGMD2i$Fylke %in% c('AKERSHUS', 'OSLO', 'BUSKERUD'), ]
LGMD2i <- LGMD2i[LGMD2i$Avdod=="Nei"]

dim(LGMD2i)[1]

Welander <- RegData[which(RegData$DiagICD10 == 'G71.0' & RegData$Undergruppe==8 & RegData$Undergruppe2==20), ]
Welander <- Welander[Welander$Avdod=="Nei"]

FinnWelander <- RegData[which(RegData$DiagICD10 == 'G71.8'), ]


FinnWelander[, c("Fylke", "UndergruppeSpes")]


########### Utlevering, navn og adresse 07.11.2017 ##########################

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2017-10-06 12-53-15.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2017-10-06 12-53-13.txt', header=TRUE, sep=';')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-10-06 12-53-11.txt', header=TRUE, sep=';')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))

RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')


persondata <- read.table('P:/MinData/muskel/PersjonRapport_Muskel_okt3.csv', header=TRUE, sep=';', encoding = 'UTF-8',
                         colClasses = "character")
persondata$PasientID <- as.numeric(persondata$PasientID)
persondata <- persondata[match(unique(persondata$PasientID), persondata$PasientID), ]

rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)

RegData$over18okt2017 <- 0
RegData$over18okt2017[which(RegData$Foedselsdato <= '1999-10-01')] <- 1
RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]

Utlevering <- RegData[which(RegData$DiagICD10 %in% c('G60.0', 'G60.8 ', 'G60.9')), ] # hvilke diagnoser
Utlevering <- Utlevering[Utlevering$over18okt2017 == 1, ]           # over 18?
Utlevering <- Utlevering[Utlevering$Fylke %in% c('VESTFOLD', 'BUSKERUD', 'ØSTFOLD', 'OSLO', 'AKERSHUS'), ]

Utlevering <- merge(Utlevering, persondata, by = 'PasientID')
Utlevering <- Utlevering[, c("Fornavn", "Etternavn", "Adresse", "Postnummer", "Poststed")]

write.csv2(Utlevering, 'Kvalstudie07112017.csv', row.names = F)



########### Antall som følges opp ved UNN 27.10.2017 ##############################################
ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2017-10-23 11-04-40.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2017-10-23 11-04-38.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("GenetiskAarsakPaavist", "ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar",
                        "Utredningsstart", "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-10-23 11-04-36.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv", "GenetiskAarsakPaavist",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
# RegData <- RegData[RegData$Avdod == 'Nei', ]

RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering

RegData$Undergruppe_label <- iconv(RegData$Undergruppe_label, from = 'UTF-8', to = '')
RegData$Undergruppe2_label <- iconv(RegData$Undergruppe2_label, from = 'UTF-8', to = '')
RegData$AngiGenetiskAarsak <- iconv(RegData$AngiGenetiskAarsak, from = 'UTF-8', to = '')

table(RegData[RegData$FoelgesOppAvIns==101719, c("Fylke", "FoelgesOppAvIns")], useNA = 'ifany')

table(RegData[which(RegData$SykehusNavn=='Universitetssykehuset Nord-Norge HF' & RegData$DiagICD10 %in% c('G71.9', 'G12.9', 'G60.9')),
              c("DiagICD10", "SykehusNavn")], useNA = 'ifany')




########### SMA-tall 27.10.2017 ##############################################
library(muskel)
rm(list=ls())

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2017-10-23 11-04-40.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2017-10-23 11-04-38.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegData <- RegData[ , c("GenetiskAarsakPaavist", "ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar",
                        "Utredningsstart", "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-10-23 11-04-36.txt', header=TRUE, sep=';', stringsAsFactors = F)
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv", "GenetiskAarsakPaavist",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)
RegData <- RegData[RegData$Avdod == 'Nei', ]

RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ] # Tar ut nyeste registrering

RegData$Undergruppe_label <- iconv(RegData$Undergruppe_label, from = 'UTF-8', to = '')
RegData$Undergruppe2_label <- iconv(RegData$Undergruppe2_label, from = 'UTF-8', to = '')
RegData$AngiGenetiskAarsak <- iconv(RegData$AngiGenetiskAarsak, from = 'UTF-8', to = '')

RegData <- RegData[which(RegData$Undergruppe %in% c(81, 82, 83)), ]
gr <- c(81, 82, 83)
grtxt <- RegData$Undergruppe_label[match(gr, RegData$Undergruppe)]
RegData$VariabelGr <- factor(RegData$Undergruppe, levels = gr, labels = grtxt)
RegData$GenetiskAarsakPaavist <- factor(RegData$GenetiskAarsakPaavist, levels = c(0,1,9), labels = c('Nei', 'Ja', 'Ikke utfylt'))

RegData <- RegData[which(RegData$Alder >=18), ]

addmargins(table(RegData$GenetiskAarsakPaavist, RegData$VariabelGr, useNA = 'ifany'))


########### Utlevering, navn og adresse 06.10.2017 ##########################

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2017-10-06 12-53-15.txt', header=TRUE, sep=';', stringsAsFactors = F)
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2017-10-06 12-53-13.txt', header=TRUE, sep=';')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-10-06 12-53-11.txt', header=TRUE, sep=';')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))

RegData$Fylke <- iconv(RegData$Fylke, from = 'UTF-8', to = '')


persondata <- read.table('P:/MinData/muskel/PersjonRapport_Muskel_okt3.csv', header=TRUE, sep=';', encoding = 'UTF-8',
                         colClasses = "character")
persondata$PasientID <- as.numeric(persondata$PasientID)
persondata <- persondata[match(unique(persondata$PasientID), persondata$PasientID), ]

rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)

RegData$over18okt2017 <- 0
RegData$over18okt2017[which(RegData$Foedselsdato <= '1999-10-01')] <- 1
RegData <- RegData[order(RegData$HovedDato, decreasing = TRUE), ]
RegData <- RegData[match(unique(RegData$PasientID), RegData$PasientID), ]

Utlevering <- RegData[which(RegData$DiagICD10 %in% c('G60.0', 'G60.8 ', 'G60.9')), ] # hvilke diagnoser
Utlevering <- Utlevering[Utlevering$over18okt2017 == 1, ]           # over 18?
Utlevering <- Utlevering[Utlevering$Fylke %in% c('VESTFOLD', 'BUSKERUD', 'ØSTFOLD', 'OSLO', 'AKERSHUS'), ]

Utlevering <- merge(Utlevering, persondata, by = 'PasientID')
Utlevering <- Utlevering[, c("Fornavn", "Etternavn", "Adresse", "Postnummer", "Poststed")]

write.csv2(Utlevering, 'Kvalstudie20102017.csv', row.names = F)


############# Utlevering Jan Frisch

ForlopsData <- read.table('P:/MinData/muskel/ForlopsOversikt2017-08-21 08-49-54.txt', header=TRUE, sep=';')
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('P:/MinData/muskel/AlleVarNum2017-08-21 08-49-52.txt', header=TRUE, sep=';')
RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
                        "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
                        "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
                        "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
                        "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
                        'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
                        'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
                        "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider')] # , "ORG_RESH"
RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
RegDataLabel <- read.table('P:/MinData/muskel/AlleVar2017-08-21 08-49-50.txt', header=TRUE, sep=';')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))

persondata <- read.table('P:/MinData/muskel/PersjonRapport_Muskel.csv', header=TRUE, sep=';', encoding = 'UTF-8',
                         colClasses = "character")
persondata$PasientID <- as.numeric(persondata$PasientID)
persondata <- persondata[match(unique(persondata$PasientID), persondata$PasientID), ]

rm(list=c('ForlopsData', 'RegDataLabel'))

RegData <- MuskelPreprosess(RegData=RegData)

RegData$alder_juli_2017 <- round(as.numeric(difftime('2017-07-01', RegData$Foedselsdato)/365.25),0)


Utlevering <- RegData[which(RegData$Undergruppe == 'Limb-girdle muskeldystrofi' | RegData$DiagICD10 == 'G60.0'), ]
Utlevering <- Utlevering[which(Utlevering$alder_juli_2017 >= 18 & Utlevering$alder_juli_2017 <= 65), ]
Utlevering <- Utlevering[which(Utlevering$ForlopsType1Num == 1), ]
Utlevering <- Utlevering[which(Utlevering$Avdod == 'Nei'), ]


Utlevering <- Utlevering[, c("PasientID", "alder_juli_2017", "erMann", "DiagICD10", "Undergruppe", "DiagnoseAar")]

Utlevering <- merge(Utlevering, persondata, by = 'PasientID')
Utlevering <- Utlevering[, c(-1, -7)]

write.csv2(Utlevering, 'Frich_v2.csv', row.names = F)


