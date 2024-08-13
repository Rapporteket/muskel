context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
onServer <- context == "TEST" | context == "QA" | context == "PRODUCTION" | context =="DEV"
if (onServer) {
  RegData <- muskel::MuskelHentRegData()
  SkjemaOversikt <- rapbase::loadRegData(
    registryName = "muskel",
    dbType = "mysql",
    query = "SELECT *
             FROM SkjemaOversikt"
  )
  AlleVarNum <- rapbase::loadRegData(
    registryName = "muskel",
    dbType = "mysql",
    query = "SELECT *
             FROM AlleVarNum"
  )
  AlleVar <- rapbase::loadRegData(
    registryName = "muskel",
    dbType = "mysql",
    query = "SELECT *
             FROM AlleVar"
  )
  ForlopsOversikt <- rapbase::loadRegData(
    registryName = "muskel",
    dbType = "mysql",
    query = "SELECT *
             FROM ForlopsOversikt"
  )
  SMAoversikt <- rapbase::loadRegData(
    registryName = "muskel",
    dbType = "mysql",
    query = "SELECT *
             FROM SMAoversikt"
  )

} #else if (context = "") {
#   # rm(list = ls())
#   ForlopsData <- read.table('v:/muskel/ForlopsOversikt2019-08-19 13-30-02.txt', header=TRUE, sep=';', encoding = 'UTF-8')
#
#   RegData <- read.table('v:/muskel/AlleVarNum2019-08-19 13-29-40.txt', header=TRUE, sep=';', encoding = 'UTF-8')
#
#   skjemaoversikt <- read.table('v:/muskel/SkjemaOversikt2019-08-19 13-30-03.txt', header=TRUE, sep=';', stringsAsFactors = F, encoding = 'UTF-8')
#
#   RegDataLabel <- read.table('v:/muskel/AlleVar2019-08-19 13-29-38.txt', header=TRUE, sep=';', encoding = 'UTF-8')
#
#   #ForlopsData <- read.table('I:/muskel/ForlopsOversikt2019-03-26 14-57-31.txt', header=TRUE, sep=';')
#   ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
#                                  "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato", 'HovedDato')]
#   #RegData <- read.table('I:/muskel/AlleVarNum2019-03-26 14-57-28.txt', header=TRUE, sep=';')
#   RegData <- RegData[ , c("ForlopsID", "Foedselsdato", "DiagICD10", "DebutAlder", "DiagnoseAar", "Utredningsstart",
#                           "Utfyllingsdato", "DiagnoseStiltAv", "Undergruppe", "Undergruppe2",
#                           "GenetiskAarsakPaavist", "DiagEndret", "FoelgesOppAvIns", "HjerteAffAlder",
#                           "Fysioterapi", "Ergoterapi", "UndergruppeSpes", "Undergruppe2Spes", "HjerteAff","HjerteAffAnnet",
#                           "HjerteAffAnnetSpes", "DiagAnamese", 'DiagEMG', 'DiagCK', 'DiagDNA',
#                           'DiagBiopsi', 'Gangfunksjon', 'AlderTapGang', 'RespStotte', 'AlderRespStotte', "TrygdFraAlder",
#                           'Uforetrygd', 'FysioManglerAarsak', "KognitivSvikt", "Utdanning", "Sivilstatus", "Delesjon",
#                           "PunktMutasjon", "Duplikasjon", 'Arvegang', 'Steroider', 'AngiGenetiskAarsak', 'Hjerteoppfoelging',
#                           'KognitivSvikt', 'MedikBehandling', 'Smertestillende', "Antiarytmika", "ACEHemmer", "AnnetHjerteMed",
#                           "AnnenMedikBeh", "OppfolgBarnelegeNevrolog", "PsykiskHelsetjeneste", "OppholdRehab", "TilbudKostveiledning",
#                           "TilbudGenetiskVeiledning", "AnsvarsgruppeIP", "BPA", "Arbeid", "SympFamilie", "TrygdFraAlder", "Kardiomyopati",
#                           "Hjertearytmi", "HjerteAffAnnet", "EKG", "HyppighetEKG", "HyppighetRytmereg", "Ultralyd", "HyppighetUltralyd", "AarstallGenAarsak")]
#   RegData <- merge(RegData, ForlopsData, by.x = 'ForlopsID', by.y = 'ForlopsID')
#   #RegDataLabel <- read.table('I:/muskel/AlleVar2019-03-26 14-57-25.txt', header=TRUE, sep=';')
#   RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
#                                    "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
#                                    'Arvegang', 'Gangfunksjon')]
#   RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
#   # RegData$Undergruppe_label <- iconv(RegData$Undergruppe_label, from = 'UTF-8', to = '')
#   # RegData$Undergruppe2_label <- iconv(RegData$Undergruppe2_label, from = 'UTF-8', to = '')
#   # RegData$ForlopsType1 <- iconv(RegData$ForlopsType1, from = 'UTF-8', to = '')
#
#   #skjemaoversikt <- read.table('I:/muskel/SkjemaOversikt2019-03-26 14-57-31.txt', header=TRUE, sep=';', stringsAsFactors = F)
#   # skjemaoversikt$Sykehusnavn <- iconv(skjemaoversikt$Sykehusnavn, from = 'UTF-8', to = '')
#   # skjemaoversikt$Skjemanavn <- iconv(skjemaoversikt$Skjemanavn, from = 'UTF-8', to = '')
#   skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
#
#   rm(list=c('ForlopsData', 'RegDataLabel'))
# } else { NULL}

RegData <- muskel::MuskelPreprosess(RegData=RegData)

diagnosegrvalg <- sort(unique(RegData$Diagnosegr))
names(diagnosegrvalg) <- RegData$Diagnosegr_label[match(diagnosegrvalg, RegData$Diagnosegr)]
aux <- c('Alder ved førstegangsregistrering', 'Alder', 'Alder i dag', 'AlderDagens',
         'Debutalder', 'DebutAlder', 'Alder ved diagnose', 'DiagnoseAlder', 'Andel med fysioterapi',
         'Fysioterapi', 'Høyeste utdanning', 'Utdanning', 'Diagnosegrupper', 'Diagnosegr',
         'Hoveddiagnoser (ICD-10)', 'DiagICD10', 'Undergrupper av muskeldystrofier', 'Muskeldystrofier',
         'Undergrupper av spinal muskelatrofi', 'SMA', 'Undergrupper av myotonier/periodiske paralyser',
         'PeriodiskeParalyser', 'Andel med steroidbehandling', 'AndelSteroider', 'Hjerteaffeksjon',
         'HjerteAff', 'Hjerteoppfølging', 'Hjerteoppf', 'Diagnose basert på', 'DiagByggerPaa',
         'DMD/BMD-diagnose basert på', 'DiagByggerPaa_v2', 'Gangfunksjon', 'Gangfunksjon', 'Arvegang',
         'Arvegang', 'Andel genetisk verifisert', 'AndelGenVerifisert', 'Type hjerteaffeksjon',
         'TypeHjerteaffeksjon', 'Tilsvarende sykdom/symptomer i familien', 'SympFamilie',
         'Respirasjonsstøtte', 'RespStotte', 'Kognitiv svikt', 'KognitivSvikt',
         'Type medikamentell behandling', 'TypeMedikBehandling', 'Fysioterapi', 'Fysioterapi',
         'Årsak til manglende fysioterapi', 'FysioManglerAarsak', 'Ergoterapi', 'Ergoterapi',
         'Oppfølging hos nevrolog/barnelege', 'OppfolgBarnelegeNevrolog', 'Oppfølging av psykisk helsetjeneste',
         'PsykiskHelsetjeneste', 'Rehabiliteringsopphold', 'OppholdRehab', 'Tilbud om kostveiledning',
         'TilbudKostveiledning', 'Tilbud om genetisk veiledning', 'TilbudGenetiskVeiledning',
         'Ansvarsgruppe/Individuell plan', 'AnsvarsgruppeIP', 'Brukerstyrt personlig assistent', 'BPA',
         'Arbeidsstatus', 'Arbeid', 'Uføretrygdet', 'Uforetrygd', 'Sivilstatus', 'Sivilstatus')

varvalg <- aux[seq(2,length(aux), by = 2)]
names(varvalg) <- aux[-seq(2,length(aux), by = 2)]
avdValg <- unique(RegData$AvdRESH)
names(avdValg) <- vapply(seq_along(avdValg), function(x) (unique(RegData$SykehusNavn[RegData$AvdRESH == avdValg[x]])),FUN.VALUE = character(1))

#####################################################################

