context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
onServer <- context == "TEST" | context == "QA" |
  context == "PRODUCTION" | context =="DEV" | context == "QAC"
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

}

RegData <- muskel::MuskelPreprosess(RegData=RegData)

diagnosegrvalg <- sort(unique(RegData$Diagnosegr))
names(diagnosegrvalg) <-
  RegData$Diagnosegr_label[match(diagnosegrvalg, RegData$Diagnosegr)]
varvalg <- c('Alder ved førstegangsregistrering' = 'Alder',
         'Alder i dag' = 'AlderDagens',
         'Debutalder' = 'DebutAlder',
         'Alder ved diagnose' = 'DiagnoseAlder',
         'Andel med fysioterapi' = 'Fysioterapi',
         'Høyeste utdanning' = 'Utdanning',
         'Diagnosegrupper' = 'Diagnosegr',
         'Hoveddiagnoser (ICD-10)' = 'DiagICD10',
         'Undergrupper av muskeldystrofier' = 'Muskeldystrofier',
         'Undergrupper av spinal muskelatrofi' = 'SMA',
         'Undergrupper av myotonier/periodiske paralyser' = 'PeriodiskeParalyser',
         'Andel med steroidbehandling' = 'AndelSteroider',
         'Hjerteaffeksjon' = 'HjerteAff',
         'Hjerteoppfølging' = 'Hjerteoppf',
         'Diagnose basert på' = 'DiagByggerPaa',
         'DMD/BMD-diagnose basert på' = 'DiagByggerPaa_v2',
         'Gangfunksjon' = 'Gangfunksjon',
         'Arvegang' = 'Arvegang',
         'Andel genetisk verifisert' = 'AndelGenVerifisert',
         'Type hjerteaffeksjon' = 'TypeHjerteaffeksjon',
         'Tilsvarende sykdom/symptomer i familien' = 'SympFamilie',
         'Respirasjonsstøtte' = 'RespStotte',
         'Kognitiv svikt' = 'KognitivSvikt',
         'Type medikamentell behandling' = 'TypeMedikBehandling',
         'Fysioterapi' = 'Fysioterapi',
         'Årsak til manglende fysioterapi' = 'FysioManglerAarsak',
         'Ergoterapi' = 'Ergoterapi',
         'Oppfølging hos nevrolog/barnelege' = 'OppfolgBarnelegeNevrolog',
         'Oppfølging av psykisk helsetjeneste' = 'PsykiskHelsetjeneste',
         'Rehabiliteringsopphold' = 'OppholdRehab',
         'Tilbud om kostveiledning' = 'TilbudKostveiledning',
         'Tilbud om genetisk veiledning' = 'TilbudGenetiskVeiledning',
         'Ansvarsgruppe/Individuell plan' = 'AnsvarsgruppeIP',
         'Brukerstyrt personlig assistent' = 'BPA',
         'Arbeidsstatus' = 'Arbeid',
         'Uføretrygdet' = 'Uforetrygd',
         'Sivilstatus' = 'Sivilstatus')

avdValg <- unique(RegData$AvdRESH)
names(avdValg) <- RegData$SykehusNavn[match(avdValg, RegData$AvdRESH)]

#####################################################################

