library(muskel)
library(tidyverse)
rm(list = ls())

ForlopsData <- read.table('I:/muskel/ForlopsOversikt2020-03-06 14-22-57.txt', header=TRUE, sep=';', encoding = 'UTF-8')
ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                               "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
RegData <- read.table('I:/muskel/AlleVarNum2020-03-06 14-22-57.txt', header=TRUE, sep=';', encoding = 'UTF-8')
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
RegDataLabel <- read.table('I:/muskel/AlleVar2020-03-06 14-22-57.txt', header=TRUE, sep=';', encoding = 'UTF-8')
RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                 "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                 'Arvegang', 'Gangfunksjon')]
RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
RegData <- MuskelPreprosess(RegData=RegData)

kobl_resh_shus_muskel <- data.frame(ReshID=sort(unique(RegData$AvdRESH)), Sykehus=RegData$SykehusNavn[match(sort(unique(RegData$AvdRESH)), RegData$AvdRESH)])

write.csv2(kobl_resh_shus_muskel, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/4. Muskel/kobl_resh_shus_muskel.csv', row.names = F)

### 1.	Andel pasienter med fastsatt diagnose under ett (to) år etter utredningsstart
# Det finnes pasienter med flere og avvikende verdier på Tid fra utredning til diagnose, og det finnes pasienter som ikke har registrert verdi.
# Skal høyeste eller laveste verdi brukes, eller skal pasientene utelukkes fra utvalget?. Pasienter uten verdi, skal de inkluderes i nevneren?
# I tillegg er tid for utredningsstart og diagnose oppgitt som årstall og det er dermed ikke mulig å si om pasienter med ett års differanse mellom
# utredningsstart og diagnose har fastsatt diagnose under ett år fra utredningsstart.
# Denne versjonen bruker laveste verdi og inkluderer alle unike pasienter som har registrert diagnoseår i nevneren. Telleren utgjøres av de som har
# ett år eller mindre differanse mellom år for utredningsstart og diagnose, så i praksis kan det være opp til 2 år fra utredningsstart til diagnose.
# Aar er år diagnosen er satt.


tmp <- RegData %>%
  group_by(PasientID) %>%
  summarise(max_TidUtredDiag = max(TidUtredDiag, na.rm = T),
            min_TidUtredDiag = min(TidUtredDiag, na.rm = T),
            Utredningsstart_max = Utredningsstart[which(TidUtredDiag == max(TidUtredDiag, na.rm = T))[1]],
            Utredningsstart_min = Utredningsstart[which(TidUtredDiag == min(TidUtredDiag, na.rm = T))[1]],
            DiagnoseAar_max = DiagnoseAar[which(TidUtredDiag == max(TidUtredDiag, na.rm = T))[1]],
            DiagnoseAar_min = DiagnoseAar[which(TidUtredDiag == min(TidUtredDiag, na.rm = T))[1]],
            # ForlopsID_Utredningsstart_max = ForlopsID[which(TidUtredDiag == max(TidUtredDiag, na.rm = T))[1]],
            # ForlopsID_Utredningsstart_min = ForlopsID[which(TidUtredDiag == min(TidUtredDiag, na.rm = T))[1]],
            ReshId_max = AvdRESH[which(TidUtredDiag == max(TidUtredDiag, na.rm = T))[1]],
            ReshId_min = AvdRESH[which(TidUtredDiag == min(TidUtredDiag, na.rm = T))[1]],
            Resh = AvdRESH[1],
            diagresh = DiagnoseStiltAv[which(TidUtredDiag == min(TidUtredDiag, na.rm = T))[1]],
            N = n()) %>% ungroup()

# tmp$max_TidUtredDiag[tmp$max_TidUtredDiag %in% c(-Inf, Inf)] <- NA
tmp$min_TidUtredDiag[tmp$min_TidUtredDiag %in% c(-Inf, Inf)] <- NA

Ind1_diagnoseinneetaar_Muskel <- tmp[, c("PasientID", "diagresh", "DiagnoseAar_min", "min_TidUtredDiag")]
Ind1_diagnoseinneetaar_Muskel$Teller <- as.numeric(Ind1_diagnoseinneetaar_Muskel$min_TidUtredDiag < 2)
Ind1_diagnoseinneetaar_Muskel$Nevner <- 1
Ind1_diagnoseinneetaar_Muskel <- Ind1_diagnoseinneetaar_Muskel[!is.na(Ind1_diagnoseinneetaar_Muskel$DiagnoseAar_min), ]

Ind1_diagnoseinneetaar_Muskel <- Ind1_diagnoseinneetaar_Muskel[ , c(2,3,5,6)]
names(Ind1_diagnoseinneetaar_Muskel) <- c('ReshId', 'Aar', 'Teller Ind1', 'Nevner Ind1')
Ind1_diagnoseinneetaar_Muskel$Indikator <- 'Ind1'
Ind1_diagnoseinneetaar_Muskel$AarID <- paste0(Ind1_diagnoseinneetaar_Muskel$Aar, Ind1_diagnoseinneetaar_Muskel$ReshId)

write.csv2(Ind1_diagnoseinneetaar_Muskel,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/4. Muskel/Indikator/Ind1_diagnoseinneetaar_Muskel.csv',
           row.names = F)


# 2.	Andel pasienter som får genetisk bekreftet diagnose for arvelige myopatier
# Hvordan behandle ukjente og tomme verdier? I denne versjonen settes tomme og ukjente til 0, altså ikke genetisk verifisert. Som Aar brukes år for
# registrering siden aar for påvisning av genetisk årsak har svært mange manglende verdier. ReshId henviser til resh der diagnose ble satt, er dette rett?

##se på resh
##Se på mulighet for kumulativ andel

RegData$GenetiskAarsakPaavist[is.na(RegData$GenetiskAarsakPaavist)] <- 0
RegData$GenetiskAarsakPaavist[RegData$GenetiskAarsakPaavist == 9] <- 0

Ind2_genetisk_myopatier_Muskel <- RegData[which(RegData$Undergruppe == 4), ] %>% group_by(PasientID) %>% summarise(Teller = max(GenetiskAarsakPaavist),
                                                                                Aar = Aar[GenetiskAarsakPaavist==max(GenetiskAarsakPaavist)][1],
                                                                                # Aar = min(AarstallGenAarsak[GenetiskAarsakPaavist==max(GenetiskAarsakPaavist)], na.rm = T),
                                                                                ReshId = min(DiagnoseStiltAv[GenetiskAarsakPaavist==max(GenetiskAarsakPaavist)], na.rm = T))

Ind2_genetisk_myopatier_Muskel$Nevner <- 1
Ind2_genetisk_myopatier_Muskel <- Ind2_genetisk_myopatier_Muskel[, c(4,3,2,5)]
names(Ind2_genetisk_myopatier_Muskel) <- c('ReshId', 'Aar', 'Teller Ind2', 'Nevner Ind2')
Ind2_genetisk_myopatier_Muskel$Indikator <- 'Ind2'
Ind2_genetisk_myopatier_Muskel$AarID <- paste0(Ind2_genetisk_myopatier_Muskel$Aar, Ind2_genetisk_myopatier_Muskel$ReshId)

write.csv2(Ind2_genetisk_myopatier_Muskel,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/4. Muskel/Indikator/Ind2_genetisk_myopatier_Muskel.csv',
           row.names = F)

# 3.	Andel pasienter som får genetisk bekreftet diagnose for CMT

Ind3_genetisk_CMT_Muskel <- RegData[which(RegData$DiagICD10 == 'G60.0'), ] %>% group_by(PasientID) %>% summarise(Teller = max(GenetiskAarsakPaavist),
                                                                                                                   Aar = Aar[GenetiskAarsakPaavist==max(GenetiskAarsakPaavist)][1],
                                                                                                                   ReshId = min(DiagnoseStiltAv[GenetiskAarsakPaavist==max(GenetiskAarsakPaavist)], na.rm = T))

Ind3_genetisk_CMT_Muskel$Nevner <- 1
Ind3_genetisk_CMT_Muskel <- Ind3_genetisk_CMT_Muskel[, c(4,3,2,5)]
names(Ind3_genetisk_CMT_Muskel) <- c('ReshId', 'Aar', 'Teller Ind3', 'Nevner Ind3')
Ind3_genetisk_CMT_Muskel$Indikator <- 'Ind3'
Ind3_genetisk_CMT_Muskel$AarID <- paste0(Ind3_genetisk_CMT_Muskel$Aar, Ind3_genetisk_CMT_Muskel$ReshId)

write.csv2(Ind3_genetisk_CMT_Muskel,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/4. Muskel/Indikator/Ind3_genetisk_CMT_Muskel.csv',
           row.names = F)



# 4.	Andel pasienter som får genetisk bekreftet diagnose for SMA type 1-4

Ind4_genetisk_SMA_type1_4_Muskel <- RegData[which(RegData$Undergruppe %in% c(70,81:83)), ] %>% group_by(PasientID) %>% summarise(Teller = max(GenetiskAarsakPaavist),
                                                                                                                 Aar = Aar[GenetiskAarsakPaavist==max(GenetiskAarsakPaavist)][1],
                                                                                                                 ReshId = min(DiagnoseStiltAv[GenetiskAarsakPaavist==max(GenetiskAarsakPaavist)], na.rm = T))

Ind4_genetisk_SMA_type1_4_Muskel$Nevner <- 1
Ind4_genetisk_SMA_type1_4_Muskel <- Ind4_genetisk_SMA_type1_4_Muskel[, c(4,3,2,5)]
names(Ind4_genetisk_SMA_type1_4_Muskel) <- c('ReshId', 'Aar', 'Teller Ind4', 'Nevner Ind4')
Ind4_genetisk_SMA_type1_4_Muskel$Indikator <- 'Ind4'
Ind4_genetisk_SMA_type1_4_Muskel$AarID <- paste0(Ind4_genetisk_SMA_type1_4_Muskel$Aar, Ind4_genetisk_SMA_type1_4_Muskel$ReshId)

write.csv2(Ind4_genetisk_SMA_type1_4_Muskel,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/4. Muskel/Indikator/Ind4_genetisk_SMA_type1_4_Muskel.csv',
           row.names = F)



## 5.	Andel pasienter som får oppfølging hos barnelege eller nevrolog
# "Hvordan behandle "Ikke relevant" ekskluderes fra nevneren, men hva med "Ukjent" samt tomme registreringer? Hvilket Aar skal brukes?
# Denne versjonen beholder bare de med ja eller nei som svaralternativer, og registreringsår benyttes som Aar.

Ind5_oppfolging_Muskel <- RegData[which(RegData$OppfolgBarnelegeNevrolog %in% 0:1), ] %>%
  group_by(PasientID) %>% summarise(Teller = max(OppfolgBarnelegeNevrolog),
                                    Aar = min(Aar[OppfolgBarnelegeNevrolog==max(OppfolgBarnelegeNevrolog)]),
                                    ReshId = min(FoelgesOppAvIns[OppfolgBarnelegeNevrolog==max(OppfolgBarnelegeNevrolog)], na.rm = T))

Ind5_oppfolging_Muskel$Nevner <- 1
Ind5_oppfolging_Muskel <- Ind5_oppfolging_Muskel[, c(4,3,2,5)]
names(Ind5_oppfolging_Muskel) <- c('ReshId', 'Aar', 'Teller Ind5', 'Nevner Ind5')
Ind5_oppfolging_Muskel$Indikator <- 'Ind5'
Ind5_oppfolging_Muskel$AarID <- paste0(Ind5_oppfolging_Muskel$Aar, Ind5_oppfolging_Muskel$ReshId)

write.csv2(Ind5_oppfolging_Muskel,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/4. Muskel/Indikator/Ind5_oppfolging_Muskel.csv',
           row.names = F)


## 6.	Andel pasienter som får fysioterapi
# "Hvordan behandle "Ikke relevant" ekskluderes fra nevneren, men hva med "Ukjent" samt tomme registreringer? Hvilket Aar skal brukes?
# Denne versjonen beholder bare de med ja eller nei som svaralternativer, og registreringsår benyttes som Aar.

### FEIL!!!!! MÅ FIKSES, LAG TO VERSJONER: JA TELLER, NEVNER ALLE. V2: JA TELLER, NEVNER JA + NEI, MEN BEHOV

Ind6_fysioterapi_muskel <- RegData[which(RegData$Fysioterapi %in% 0:1), ] %>%
  group_by(PasientID) %>% summarise(Teller = max(Fysioterapi),
                                    Aar = min(Aar[Fysioterapi==max(Fysioterapi)]),
                                    ReshId = min(FoelgesOppAvIns[Fysioterapi==max(Fysioterapi)], na.rm = T))

Ind6_fysioterapi_muskel$Nevner <- 1
Ind6_fysioterapi_muskel <- Ind6_fysioterapi_muskel[, c(4,3,2,5)]
names(Ind6_fysioterapi_muskel) <- c('ReshId', 'Aar', 'Teller Ind6', 'Nevner Ind6')
Ind6_fysioterapi_muskel$Indikator <- 'Ind6'
Ind6_fysioterapi_muskel$AarID <- paste0(Ind6_fysioterapi_muskel$Aar, Ind6_fysioterapi_muskel$ReshId)

write.csv2(Ind6_fysioterapi_muskel,
           'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/4. Muskel/Indikator/Ind6_fysioterapi_muskel.csv',
           row.names = F)


#####################  Nøkkeltall  ############################

nokkeltall_muskel <- RegData %>% group_by(Aar) %>% summarise("Antall nyregistrerte" = sum(ForlopsType1Num==1),
                                        "Antall 5-års oppfølginger" = sum(ForlopsType1Num==2),
                                        "Antall ad-hoc oppfølginger" = sum(ForlopsType1Num==3),
                                        "Gj.sn. alder nyreg." = mean(AlderVreg[ForlopsType1Num==1]),
                                        "Andel kvinner blant nyreg." = mean(erMann[ForlopsType1Num==1]==0)*100,
                                        "Antall registrerende enheter" = length(unique(AvdRESH)))

write.csv2(nokkeltall_muskel, 'Q:/SKDE/Nasjonalt servicemiljø/Resultattjenester/Resultatportalen/4. Muskel/nokkeltall_muskel.csv', row.names = F)


# RegData[RegData$PasientID == 12,
#         c("TidUtredDiag", "Debut", "Utredningsstart", "DiagnoseAar", "DiagICD10", "DiagnoseStiltAv", "PasientAlder", "DiagnoseAlder",
#           "HovedDato", "PasientID", "ForlopsType1")]



 # length(unique(tmp$PasientID))
#
# tmp$diff <- abs(tmp$max_TidUtredDiag - tmp$min_TidUtredDiag)
# tmp$diff[tmp$diff == Inf] <- NA
#
# sort(tmp$PasientID[which(tmp$Utredningsstart_min != tmp$Utredningsstart_max)])
#
# tmp[which(tmp$diff !=0), ]
# RegData[RegData$PasientID == as.numeric(tmp[which(tmp$diff !=0)[2], "PasientID"]),
#         c("TidUtredDiag", "Debut", "Utredningsstart", "DiagnoseAar", "DiagICD10", "DiagnoseStiltAv", "PasientAlder", "DiagnoseAlder",
#           "HovedDato", "PasientID", "ForlopsType1")]
#
