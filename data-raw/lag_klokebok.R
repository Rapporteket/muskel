
klokebok <-read.table(
  system.file(
    file.path('extdata', 'Muskelregisteret_klokeboken_11.11.2024.csv'),
    package = 'muskel'), sep=';',
  stringsAsFactors=FALSE, header=T,
  fileEncoding = 'UTF-8')

usethis::use_data(klokebok, overwrite = TRUE, internal = FALSE)

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
usethis::use_data(varvalg, overwrite = TRUE, internal = FALSE)

varValgGrVar <-
  c("Andel genetisk verifiserte etter diagnosegruppe (spes. diagnose)" =
      "AndelGenVerifisertSpes",
    "Høyeste utdanning" = "HoyesteUtdanning",
    "Hjerteaffeksjon" = "HjerteAff_samlet",
    "Andel med muskelbiopsi" = "DiagBiopsi",
    "Andel genetisk verifisert" = "AndelGenVerifisert_subgr",
    "Fysioterapi" = "Fysioterapi",
    "Ergoterapi" = "Ergoterapi",
    "Andel med DNA-undersøkelse" = "DiagDNA",
    "Gangfunksjon" = "Gangfunksjon",
    "Bruk av smertestillende LGMD/DM1" = "Smertestillende_LGMD_DM1",
    "Bruk av smertestillende MD/CMT/SMA" = "Smertestillende_MD_CMT_SMA",
    "Type hjerteaffeksjon DM1/LGMD2I" = "TypeHjerteaffeksjonSamletDM1_LGMD2I",
    "Psykisk helsetjeneste DM1-2/LGMD" = "PsykiskHelsetjeneste_subgr",
    "Tilbud om kostveiledning" = "Kostveiledning_subgr")
usethis::use_data(varValgGrVar, overwrite = TRUE, internal = FALSE)

varValgtKumAnd <- c(
  'Tid fra debut til diagnose' ='TidDebDiag',
  'Tid fra debut til utredningsstart'= 'TidDebUtred',
  'Tid fra utredning til diagnose'='TidUtredDiag',
  'Alder ved hjerteaffeksjon' = 'AlderHjAff_cumsum' ,
  'Alder ved tap av gangfunksjon' = 'AlderTapGang',
  'Alder for respirasjonsstøtte' = 'AlderRespStotte',
  'Alder for mottak av trygd' = 'TrygdFraAlder')
usethis::use_data(varValgtKumAnd, overwrite = TRUE, internal = FALSE)

registryName <- "muskel"
dbType <- "mysql"
query <- paste0("SELECT
                  AlleVarNum.ForlopsID,
                  AlleVarNum.Undergruppe,
                  AlleVarNum.Undergruppe2,
                  AlleVar.Undergruppe AS Undergruppe_label,
                  AlleVar.Undergruppe2 AS Undergruppe2_label
                  FROM AlleVarNum INNER JOIN AlleVar ON
                AlleVarNum.ForlopsID = AlleVar.ForlopsID")
aux <- rapbase::loadRegData(registryName, query, dbType)

map_undergruppe <-
  data.frame(
    kode=sort(unique(aux$Undergruppe)),
    label=aux$Undergruppe_label[match(sort(unique(aux$Undergruppe)),
                                      aux$Undergruppe)]
  )
map_undergruppe2 <-
  data.frame(
    kode=sort(unique(aux$Undergruppe2)),
    label=aux$Undergruppe2_label[match(sort(unique(aux$Undergruppe2)),
                                       aux$Undergruppe2)]
  )
usethis::use_data(map_undergruppe, overwrite = TRUE, internal = FALSE)
usethis::use_data(map_undergruppe2, overwrite = TRUE, internal = FALSE)
