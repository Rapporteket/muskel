#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

######## Last data ########################################
library(muskel)
library(tidyverse)
library(shinyalert)
library(kableExtra)
library(DT)
library(htmltools)



addResourcePath('rap', system.file('www', package='rapbase'))
regTitle <-  "RAPPORTEKET MUSKELREGISTERET"
logo <- includeHTML(system.file('www/logo.svg', package='rapbase'))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\" style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))


{

# context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
# onServer <- context == "TEST" | context == "QA" | context == "PRODUCTION"
# if (onServer) {
#   RegData <- MuskelHentRegData()
# } else {
#   # rm(list = ls())
#   ForlopsData <- read.table('V:/ForlopsOversikt2019-08-19 13-30-02.txt', header=TRUE, sep=';')
#
#   RegData <- read.table('V:/AlleVarNum2019-08-19 13-29-40.txt', header=TRUE, sep=';')
#
#   skjemaoversikt <- read.table('V:/SkjemaOversikt2019-08-19 13-30-03.txt', header=TRUE, sep=';', stringsAsFactors = F)
#
#   RegDataLabel <- read.table('V:/AlleVar2019-08-19 13-29-38.txt', header=TRUE, sep=';')
#
#   #ForlopsData <- read.table('I:/muskel/ForlopsOversikt2019-03-26 14-57-31.txt', header=TRUE, sep=';')
#   ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
#                                  "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
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
#   RegData$Undergruppe_label <- iconv(RegData$Undergruppe_label, from = 'UTF-8', to = '')
#   RegData$Undergruppe2_label <- iconv(RegData$Undergruppe2_label, from = 'UTF-8', to = '')
#   RegData$ForlopsType1 <- iconv(RegData$ForlopsType1, from = 'UTF-8', to = '')
#
#   #skjemaoversikt <- read.table('I:/muskel/SkjemaOversikt2019-03-26 14-57-31.txt', header=TRUE, sep=';', stringsAsFactors = F)
#   skjemaoversikt$Sykehusnavn <- iconv(skjemaoversikt$Sykehusnavn, from = 'UTF-8', to = '')
#   skjemaoversikt$Skjemanavn <- iconv(skjemaoversikt$Skjemanavn, from = 'UTF-8', to = '')
#   skjemaoversikt$HovedDato <- as.Date(skjemaoversikt$HovedDato)
#
#   rm(list=c('ForlopsData', 'RegDataLabel'))
# }
#
# RegData <- MuskelPreprosess(RegData=RegData)
#
# diagnosegrvalg <- sort(unique(RegData$Diagnosegr))
# names(diagnosegrvalg) <- RegData$Diagnosegr_label[match(diagnosegrvalg, RegData$Diagnosegr)]
# aux <- c('Alder ved førstegangsregistrering', 'Alder', 'Alder i dag', 'AlderDagens',
#          'Debutalder', 'DebutAlder', 'Alder ved diagnose', 'DiagnoseAlder', 'Andel med fysioterapi',
#          'Fysioterapi', 'Høyeste utdanning', 'Utdanning', 'Diagnosegrupper', 'Diagnosegr',
#          'Hoveddiagnoser (ICD-10)', 'DiagICD10', 'Undergrupper av muskeldystrofier', 'Muskeldystrofier',
#          'Undergrupper av spinal muskelatrofi', 'SMA', 'Undergrupper av myotonier/periodiske paralyser',
#          'PeriodiskeParalyser', 'Andel med steroidbehandling', 'AndelSteroider', 'Hjerteaffeksjon',
#          'HjerteAff', 'Hjerteoppfølging', 'Hjerteoppf', 'Diagnose basert på', 'DiagByggerPaa',
#          'DMD/BMD-diagnose basert på', 'DiagByggerPaa_v2', 'Gangfunksjon', 'Gangfunksjon', 'Arvegang',
#          'Arvegang', 'Andel genetisk verifisert', 'AndelGenVerifisert', 'Type hjerteaffeksjon',
#          'TypeHjerteaffeksjon', 'Tilsvarende sykdom/symptomer i familien', 'SympFamilie',
#          'Respirasjonsstøtte', 'RespStotte', 'Kognitiv svikt', 'KognitivSvikt',
#          'Type medikamentell behandling', 'TypeMedikBehandling', 'Fysioterapi', 'Fysioterapi',
#          'Årsak til manglende fysioterapi', 'FysioManglerAarsak', 'Ergoterapi', 'Ergoterapi',
#          'Oppfølging hos nevrolog/barnelege', 'OppfolgBarnelegeNevrolog', 'Oppfølging av psykisk helsetjeneste',
#          'PsykiskHelsetjeneste', 'Rehabiliteringsopphold', 'OppholdRehab', 'Tilbud om kostveiledning',
#          'TilbudKostveiledning', 'Tilbud om genetisk veiledning', 'TilbudGenetiskVeiledning',
#          'Ansvarsgruppe/Individuell plan', 'AnsvarsgruppeIP', 'Brukerstyrt personlig assistent', 'BPA',
#          'Arbeidsstatus', 'Arbeid', 'Uføretrygdet', 'Uforetrygd', 'Sivilstatus', 'Sivilstatus')
#
# varvalg <- aux[seq(2,length(aux), by = 2)]
# names(varvalg) <- aux[-seq(2,length(aux), by = 2)]
#
# #####################################################################
}
library(shiny)



# system.file("shinyApps/dataOGvar.R",package = "muskel") %>%
#   source(encoding = "UTF-8")
# system.file("shinyApps/forAndGr.R",package = "muskel") %>%
#   source(encoding = "UTF-8")
# system.file("shinyApps/kumandel.R",package = "muskel") %>%
#   source(encoding = "UTF-8")
# system.file("shinyApps/tabell.R",package = "muskel") %>%
#   source(encoding = "UTF-8")

RegData <- MuskelPreprosess(RegData=RegData)

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

#####################################################################
varValgGrVar <- c("Andel genetisk verifiserte etter diagnosegruppe (spes. diagnose)" =
                    "AndelGenVerifisertSpes", "Høyeste utdanning" = "HoyesteUtdanning",
                  "Hjerteaffeksjon" = "HjerteAff_samlet", "Andel med muskelbiopsi" = "DiagBiopsi",
                  "Andel genetisk verifisert" = "AndelGenVerifisert_subgr", "Fysioterapi" = "Fysioterapi",
                  "Ergoterapi" = "Ergoterapi", "Andel med DNA-undersøkelse" = "DiagDNA",
                  "Gangfunksjon" = "Gangfunksjon", "Bruk av smertestillende LGMD/DM1" =
                    "Smertestillende_LGMD_DM1","Bruk av smertestillende MD/CMT/SMA" =
                    "Smertestillende_MD_CMT_SMA", "Type hjerteaffeksjon DM1/LGMD2I" =
                    "TypeHjerteaffeksjonSamletDM1_LGMD2I","Psykisk helsetjeneste DM1-2/LGMD" =
                    "PsykiskHelsetjeneste_subgr", "Tilbud om kostveiledning" = "Kostveiledning_subgr")

#shiny module
forGrVarUI <- function(id,vlgtvar =varValgGrVar, datoStart = "2008-01-01",
                       datoSlutt = Sys.Date()){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(

    shiny::sidebarPanel(

      shiny::selectInput(ns("var"), label = "Velg variabel",
                         choices = vlgtvar, selected = vlgtvar[[1]] ),

      shiny::sliderInput(ns("ald"), label = "Debutalder",
                         min = 0, max = 120, value = c(0,120) ),

      shiny::dateRangeInput(ns("dato"), label = "Tidsperiode",
                            start = datoStart, end = datoSlutt,
                            format = "yyyy-mm-dd"),

      shiny::selectInput(ns("kjo"), label = "Kjønn",
                         choices = c("Begge kjønn" = 99 , "Kvinne" = 0, "Mann" = 1),
                         selected = 99),

      shiny::selectInput(ns("avdod"), label = "Inkluder avdøde",
                         choices = c("Ja" , "Nei" ),
                         selected = "Nei"),

      selectInput(inputId = ns("outfile"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
    ),#sidebarPanel

    shiny::mainPanel(
      tabsetPanel(
        shiny::tabPanel("Figur",
                        shiny::plotOutput(ns("Figur"), height="auto"),
                        shiny::downloadButton(ns("lastNedBilde"), "Last ned bilde")),

        shiny::tabPanel("Tabell",
                        shiny::tableOutput(ns("Tabell")),
                        shiny::downloadButton(ns("lastNedTabell"), "Last ned tabell"))
      )
    )#mainpanel
  )#sidebarLayout
}


forGrVar <- function(input, output, session, rID = reshID() ){

  resp <- reactive({
    tabData <- muskel::MuskelFigAndelStabel( RegData = RegData, valgtVar = input$var, datoFra = input$dato[1],
                                             datoTil =   input$dato[2], minald = input$ald[1], maxald = input$ald[2] ,
                                             erMann = as.numeric(input$kjo), avdod = input$avdod ,reshID = rID, outfile = "" )
  })

  output$Figur <- renderPlot({
    muskel::MuskelFigAndelStabel( RegData = RegData, valgtVar = input$var, datoFra = input$dato[1],
                                  datoTil =   input$dato[2], minald = input$ald[1], maxald = input$ald[2] ,
                                  erMann = as.numeric(input$kjo), avdod = input$avdod ,reshID = rID, outfile = "" )
  },
  width = 700, height = 700)

  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0(input$var, Sys.time(), '.', input$outfile)
    },
    content = function(file){
      muskel::MuskelFigAndelStabel( RegData = RegData, valgtVar = input$var, datoFra = input$dato[1],
                                    datoTil =   input$dato[2], minald = input$ald[1], maxald = input$ald[2] ,
                                    erMann = as.numeric(input$kjo), avdod = input$avdod ,reshID = rID, outfile = file )
    }
  )


  output$Tabell <- function() {
    TabellData <- resp()
    Tabell <- as.data.frame( TabellData$Andeler )
    names(Tabell) <- c("Kategori", "Grupper","Andel")
    Tabell %>%
      knitr::kable("html", digits = c(0,0,1)) %>%
      kable_styling("hover", full_width = F)
  }

  output$lastNedTabell <- downloadHandler(
    filename = function(){
      paste0(input$var, Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- resp()
      Tabell <- as.data.frame(TabellData$Andeler)
      names(Tabell) <- c("Kategori", "Grupper","Andel")
      write.csv2(Tabell, file, row.names = F)
    }
  )
}

#alternative til dateInput med mulighet til  bare år, måned og år ..
dateInput2 <- function(inputId, label, minview = "years", maxview = "decades", ...) {
  d <- shiny::dateInput(inputId, label, ...)
  d$children[[2L]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}

#alternative til dateRangeInput med mulighet for å velge bare år, måned og år ..
dateRangeInput2 <- function(inputId, label, minview = "years", maxview = "decades", ...) {
  d <- shiny::dateRangeInput(inputId, label, ...)
  d$children[[2L]]$children[[1]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[3]]$attribs[["data-date-min-view-mode"]] <- minview
  d$children[[2L]]$children[[1]]$attribs[["data-date-max-view-mode"]] <- maxview
  d$children[[2L]]$children[[3]]$attribs[["data-date-max-view-mode"]] <- maxview
  d
}


#converts Null values to -1
convNull <- function(x){
  if (is.null(x)){x <-  -1
  }else {x <-  x}
}



# variabler
varValgtKumAnd <- c('Tid fra debut til diagnose' ='TidDebDiag', 'Tid fra debut til utredningsstart'=
                      'TidDebUtred', 'Tid fra utredning til diagnose'='TidUtredDiag', 'Alder ved hjerteaffeksjon' =
                      'AlderHjAff_cumsum' , 'Alder ved tap av gangfunksjon' = 'AlderTapGang',
                    'Alder for respirasjonsstøtte' = 'AlderRespStotte', 'Alder for mottak av trygd' = 'TrygdFraAlder')

diagnosegrvalg <- sort(unique(RegData$Diagnosegr))
names(diagnosegrvalg) <- RegData$Diagnosegr_label[match(diagnosegrvalg, RegData$Diagnosegr)]



#shinymodule UI
kumulativAndelUI <- function(id,vlgtvar = varValgtKumAnd, datoStart = "2008-01-01" ,
                             datoSlutt = Sys.Date(), utrstart = "1950-01-01", utrslutt = Sys.Date(),
                             pasgr = c('Registrert ved HF'=0, 'Følges opp ved HF'=1, 'Diagnostisert ved HF'=2,
                                       'Bosatt i fylke'=3), diagngr = diagnosegrvalg ){

  ns <- shiny::NS(id)

  shiny::sidebarLayout(

    shiny::sidebarPanel(

      shiny::selectInput(ns("var"), label = "Velg variabel",
                         choices = vlgtvar, selected = vlgtvar[[1]] ),

      shiny::dateRangeInput(ns("dato"), label = "Tidsperiode",
                            start = datoStart, end = datoSlutt,
                            format = "yyyy-mm-dd"),

      dateRangeInput2(ns("utrar"), label = "Utredningsår",
                      start = utrstart, end = utrslutt,
                      minview = "years", format = "yyyy"),

      shiny::sliderInput(ns("ald"), label = "Debutalder",
                         min = 0, max = 120, value = c(0,120) ),

      shiny::selectInput(ns("psgr"), label = "Pasientgruppe" ,
                         choices = pasgr,
                         selected = 0 ),

      shiny::selectInput(ns("enh"), label = "Velg enhet(er)" ,
                         choices = c("Hele landet" = 0, "Egen avdeling" = 2),
                         selected = 0 ),

      shiny::selectInput(ns("diaggrupper"),
                         label = "Velg diagnosegruppe",
                         choices = diagngr,
                         multiple = TRUE),

      shiny::uiOutput(ns("ICD10diag")),

      shiny::uiOutput(ns("undergr")),

      shiny::uiOutput(ns("undergr2")),

      shiny::selectInput(ns("avdod"), label = "Inkluder avdøde",
                         choices = c("Ja" , "Nei" ),
                         selected = "Nei"  ),
      selectInput(inputId = ns("outfile"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))


    ),#sidebarPanel

    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Figur",
                        shiny::plotOutput(ns("Figur"), height="auto", hover = ns("hov")),
                        shiny::downloadButton(ns("lastNedBilde"), "Last ned bilde")),
        shiny::tabPanel("Tabell",
                        shiny::tableOutput(ns("Tabell")),
                        shiny::downloadButton(ns("lastNedTabell"), "Last ned tabell"))
      )#tabsetPanel
    )#mainPanel
  )#sidebarlayout
}

#shiny server mudule
kumulativAndel <- function(input, output, session, rID){

  output$ICD10diag <-shiny::renderUI({
    ns <- session$ns
    if(!is.null(input$diaggrupper)){
      if(!is.null(sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in% as.numeric(input$diaggrupper)]))) ){
        ICd10 <- sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in% as.numeric(input$diaggrupper)]))
        shiny::selectInput(ns("ICD"),label = "Velg diagnosekode(r)",
                           choices = (ICd10), multiple = TRUE)
      }
    }
  })

  output$undergr <- shiny::renderUI({
    ns <- session$ns
    if(!is.null(input$ICD) ){
      undrgr <- sort(unique(RegData$Undergruppe[RegData$DiagICD10 %in% input$ICD]))
      names(undrgr) <- RegData$Undergruppe_label[match(sort(
        unique(RegData$Undergruppe[RegData$DiagICD10 %in% input$ICD])),
        RegData$Undergruppe)]
      if(length(undrgr != 0)) {
        shiny::selectInput(ns("Undrgr"),label = "Velg undergruppe(r)",
                           choices = (undrgr), multiple = TRUE)
      }
    }
  })

  output$undergr2 <- shiny::renderUI({
    ns <- session$ns
    if(!is.null(input$Undrgr) ){
      undrgr2 <-  sort(unique(RegData$Undergruppe2[RegData$Undergruppe %in% input$Undrgr]))
      names(undrgr2) <- RegData$Undergruppe2_label[match(sort(
        unique(RegData$Undergruppe2[RegData$Undergruppe %in% input$Undrgr])),
        RegData$Undergruppe2)]
      if(length(undrgr2) != 0){
        shiny::selectInput(ns("Undrgr2"),label = "Velg undergrupper nivå 2",
                           choices = (undrgr2), multiple = TRUE)
      }
    }
  })

  resp <- reactive({
    tabData <- muskel::MuskelFigCumAndel( RegData = RegData, valgtVar = input$var, datoFra = input$dato[1],
                                          datoTil =   input$dato[2], debutAlderFra = input$ald[1], debutAlderTil = input$ald[2] ,
                                          UtredningsaarFra = lubridate::year(input$utrar[1]),
                                          UtredningsaarTil = lubridate::year(input$utrar[2]) ,
                                          diagnosegr = convNull(input$diaggrupper), diagnose = convNull(input$ICD),
                                          undergr = convNull(input$Undrgr), undergr2 = convNull(input$Undrgr2),
                                          egenavd = as.numeric(input$psgr), enhetsUtvalg = as.numeric(input$enh) ,
                                          avdod = input$avdod ,reshID = rID, outfile = "" )
  })

  #plot figure
  observe({
    output$Figur <- shiny::renderPlot({
      muskel::MuskelFigCumAndel( RegData = RegData, valgtVar = input$var, datoFra = input$dato[1],
                                 datoTil =   input$dato[2], debutAlderFra = input$ald[1], debutAlderTil = input$ald[2] ,
                                 UtredningsaarFra = lubridate::year(input$utrar[1]),
                                 UtredningsaarTil = lubridate::year(input$utrar[2]) ,
                                 diagnosegr = convNull(input$diaggrupper), diagnose = convNull(input$ICD),
                                 undergr = convNull(input$Undrgr), undergr2 = convNull(input$Undrgr2),
                                 egenavd = as.numeric(input$psgr), enhetsUtvalg = as.numeric(input$enh) ,
                                 avdod = input$avdod ,reshID = rID, outfile = "" )
    },
    width = 700, height = 700)
  })


  #download figure
  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0(input$var, Sys.time(), '.', input$outfile)
    },
    content = function(file){
      muskel::MuskelFigCumAndel( RegData = RegData, valgtVar = input$var, datoFra = input$dato[1],
                                 datoTil =   input$dato[2], minald = input$ald[1], maxald = input$ald[2] ,
                                 UtredningsaarFra = lubridate::year(input$utrar[1]),
                                 UtredningsaarTil = lubridate::year(input$utrar[2]) ,
                                 diagnosegr = convNull(input$diaggrupper), diagnose = convNull(input$ICD),
                                 undergr = convNull(input$Undrgr), undergr2 = convNull(input$Undrgr2),
                                 egenavd = as.numeric(input$psgr), enhetsUtvalg = as.numeric(input$enh) ,
                                 avdod = input$avdod ,reshID = rID, outfile = file )
    }
  )
  #Tabeller
  output$Tabell <- function(){
    TabellData <- resp()
    Tabell <- as.data.frame(TabellData$Andel)
    names(Tabell) <- c("Antall år","Andel")
    Tabell  %>%
      knitr::kable("html", digits = c(0,1)) %>%
      kable_styling("hover", full_width = F)

  }
  #nedlasting av tabeller
  output$lastNedTabell <- downloadHandler(
    filename = function(){
      paste0(input$var, Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <-  resp()
      Tabell <- as.data.frame(TabellData$Andeler)
      names(Tabell) <- c("Antall år","Andel")
      write.csv2(Tabell, file, row.names = F)
    }
  )

}#kumulativeAndel

headerFooter <- function(data){
  data <- as.data.frame.matrix(data)
  dataStr <- dim(data)
  hr <-  c("", names(data))
  fr <-  c("sum", data[dataStr[1],1:dataStr[2]])
  sketch <- htmltools::tags$table(
    tableHeader(names = hr) ,
    tableFooter(names = fr ) )
  return(sketch)
}

forlop <- unique(RegData$ForlopsType1)

tabellUI <- function(id, datoStart = "2008-01-01",
                     datoSlutt = Sys.Date(),forltype = forlop){
  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::dateRangeInput(ns("dato"), "Tidsperiode:",
                            start = datoStart, end = datoSlutt,
                            format = "yyyy-mm-dd" ),

      shiny::sliderInput(ns("ald"), label = "Alder",
                         min = 0, max = 120, value = c(0,120) ),

      shiny::selectInput(ns("kjo"), "Kjønn",
                         choices = c("Begge" = 99, "Kvinne" = 0,
                                     "Mann" = 1),selected = 99),

      shiny::selectInput(ns("tidenh"), "Velg tidsenhet",
                         choices = c("Måned" = "maaned", "År" = "aar")),

      shiny::selectInput(ns("forl"), label = "Forløpstype",
                         choices = forltype, multiple = TRUE),


      shiny::selectInput(ns("avdod"), label = "Inkluder avdøde",
                         choices = c("Ja" , "Nei" ),
                         selected = "Nei"),

      shiny::radioButtons(ns("skjemarad"), "",
                          choices = c("Forløp"= "ForlopsID","Pasient"= "PasientID"),
                          inline = TRUE  )
    ),
    shiny::mainPanel(
      shiny::h3(textOutput(ns("txt1"))),
      shiny::h5(textOutput(ns("txt2"))),
      hr(),
      DT::DTOutput(ns("Tabell")),
      h6(textOutput(ns("txt"))),
      shiny::downloadButton(ns("lastNedTabell"), "Last ned tabell")
    )
  )
}



tabell <- function(input, output, session){

  forloptxt <- reactive({if(input$skjemarad=="ForlopsID") {
    "registrerte pasientforløp"
  }else{ "unike pasienter"}  })

  tidenhtxt <- reactive({if(input$tidenh == "maaned") {
    "måned"
  }else{ "år"}  })


  output$txt1 <- renderText({ paste0("Antall " ,forloptxt()," per ",tidenhtxt(), " per avdeling") })


  tabellData <- reactive({ as.data.frame.matrix( muskel::MuskelTabellerForlopspas(RegData, tidFra = input$dato[1],tidTil = input$dato[2],
                                                                                  aldmin = input$ald[1], aldmax = input$ald[2], kjoen = input$kjo,
                                                                                  tidenh = input$tidenh, avd = input$avdod,IDType = input$skjemarad, frlType = input$forl )
  ) })

  observe({
    cont <- headerFooter(tabellData())
    subS <- dim(tabellData())[1]-1

    output$Tabell <-  renderDT(
      as.data.frame.matrix(tabellData())[1:subS, ] , container = cont, extensions = 'FixedHeader',
      options = list(pageLength = 50,fixedHeader = TRUE, lengthChange = FALSE) )
  })

}

# Define UI for application that draws a histogram
ui <- navbarPage(#title = "RAPPORTEKET MUSKELREGISTERET", theme = "bootstrap.css",
                title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                            regTitle),
                windowTitle = regTitle,
                theme = "rap/bootstrap.css",

                 tabPanel("Fordelingsfigurer",
                          shinyalert::useShinyalert(),
                          rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                       organization = uiOutput("appOrgName"),
                                                       addUserInfo = TRUE),
                          # sidebarLayout(
                          sidebarPanel(
                            shinyjs::useShinyjs(),
                            selectInput(inputId = "valgtVar", label = "Velg variabel",
                                        choices = varvalg),
                            dateInput(inputId = 'datoFra', value = '2008-01-01', min = '2008-01-01',
                                      label = "F.o.m. dato", language="nb"),
                            dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                                      label = "T.o.m. dato", language="nb"),
                            sliderInput(inputId="alder", label = "Alder", min = 0,
                                        max = 120, value = c(0, 120)),
                            selectInput(inputId = "egenavd", label = "Pasientgruppe", selected = 0,
                                        choices = c('Registrert ved HF'=0, 'Følges opp ved HF'=1, 'Diagnostisert ved HF'=2,
                                        'Bosatt i fylke'=3)),
                            selectInput(inputId = "enhetsUtvalg", label = "Kjør rapport for", selected = 1,
                                        choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
                            selectInput(inputId = "forlop", label = "Velg forløpstype",
                                        choices = c('Alle'=99, 'Basisregistrering'=1, '5-årsoppfølging'=2, 'Ad-hoc'=3)),
                            selectInput(inputId = "erMann", label = "Kjønn",
                                        choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
                            selectInput(inputId = "diagnosegr", label = "Velg diagnosegruppe(r)",
                                        choices = diagnosegrvalg, multiple = TRUE),
                            uiOutput(outputId = 'icd10_kntr'),
                            uiOutput(outputId = 'undergruppe1'),
                            uiOutput(outputId = 'undergruppe2'),
                            selectInput(inputId = "avdod", label = "Inkluder avdøde",
                                        choices = c('Ja'='Ja', 'Nei'='Nei')),
                            sliderInput(inputId="Utredningsaar", label = "Utredningsår", sep='', min = 1950,
                                        max = as.numeric(format(Sys.Date(), '%Y')), value = c(1950, as.numeric(format(Sys.Date(), '%Y')))),
                            sliderInput(inputId="debutalder", label = "Debutalder", min = 0,
                                        max = 90, value = c(0, 90)),
                            selectInput(inputId = "bildeformat", label = "Velg bildeformat",
                                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Figur",
                                     plotOutput("Figur1", height="auto"), downloadButton("lastNedBilde", "Last ned bilde")),
                            tabPanel("Tabell",
                                     tableOutput("Tabell1"), downloadButton("lastNed", "Last ned tabell"))
                          )
                          )
                 ),
                tabPanel("Fordelinger etter grupperingsvariabler",
                  forGrVarUI(id = "forgrvar")
                ),
                tabPanel("Kummulative andeler",
                         kumulativAndelUI(id = "kumAnd")
                ),
                tabPanel("Pasient og forløpstabeller",
                         tabellUI("muskeltabell")

                ),

                tabPanel("Administrative tabeller",
                          sidebarPanel(
                            dateInput(inputId = 'datoFra2', value = '2008-01-01', min = '2008-01-01',
                                      label = "F.o.m. dato", language="nb"),
                            dateInput(inputId = 'datoTil2', value = Sys.Date(), min = '2012-01-01',
                                      label = "T.o.m. dato", language="nb"),
                            selectInput(inputId = "regstatus", label = "Skjemastatus",
                                        choices = c('Ferdigstilt'=1, 'Kladd'=0))
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Antall skjema",
                                     DTOutput("Tabell_adm1"), downloadButton("lastNed1", "Last ned tabell")),
                            tabPanel("Annen admin rapport",
                                     tableOutput("Tabell_adm2"), downloadButton("lastNed2", "Last ned tabell"))
                          )

                          )
                 )

)


#
server <- function(input, output, session) {

  reshID <- reactive({
    ifelse(onServer,as.numeric(rapbase::getShinyUserReshId(session, testCase = TRUE)),101719)
  })
  userRole <- reactive({
    ifelse(onServer, rapbase::getShinyUserRole(session, testCase = TRUE), 'SC')
  })

  # observe(
  #   if (userRole() != 'SC') {
  #     shinyjs::hide(id = 'alder')
  #   }
  # )

  antskjema <- function() {
    aux <- as.data.frame.matrix(addmargins(table(skjemaoversikt[skjemaoversikt$SkjemaStatus == as.numeric(input$regstatus) &
                                                                  skjemaoversikt$HovedDato >= input$datoFra2 &
                                                                  skjemaoversikt$HovedDato <= input$datoTil2,
                                                                c("Sykehusnavn", "Skjemanavn")], useNA = 'ifany')))
    aux$Avdeling <- row.names(aux)
    ant_skjema <- aux[, c(dim(aux)[2], 1:(dim(aux)[2]-1))]
    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)
  }

  output$Tabell_adm1 = renderDT(
    datatable(antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
              container = antskjema()$sketch,
              rownames = F,
              options = list(pageLength = 25)
    )
  )

  # output$Tabell_adm1 = renderDT(
  #   datatable(dt_test()[-dim(dt_test())[1], ],
  #             container = htmltools::withTags(table(
  #               tableHeader(dt_test()[-dim(dt_test())[1], ]),
  #               tableFooter(c('Sum' , as.numeric(dt_test()[dim(dt_test())[1], 2:dim(dt_test())[2]]))))),
  #               # tableFooter(as.numeric(dt_test()[dim(dt_test())[1], 2:5])))),
  #               # tableFooter(sapply(dt_test()[-dim(dt_test())[1], ], function(x) if(is.numeric(x)) sum(x))))),
  #             rownames = F,
  #             options = list(pageLength = 25)
  #             )
  #
  #   # options = list(pageLength = 25))
  # )



  output$icd10_kntr <- renderUI({selectInput(inputId = "icd10_kntr_verdi", label = "Velg diagnosekode(r)",
                                             choices = if (is.null(input$diagnosegr)){
                                               "-1"
                                             } else {
                                               sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in% as.numeric(input$diagnosegr)]))
                                             },
                                             multiple = TRUE)})

  output$undergruppe1 <- renderUI({selectInput(inputId = "undergruppe1_verdi", label = "Velg undergruppe(r)",
                                               choices = if (is.null(input$icd10_kntr_verdi)){
                                                 setNames(-1, 'Ingen')
                                               } else {
                                                 setNames(sort(unique(RegData$Undergruppe[RegData$DiagICD10 %in% input$icd10_kntr_verdi])),
                                                          RegData$Undergruppe_label[match(sort(
                                                            unique(RegData$Undergruppe[RegData$DiagICD10 %in% input$icd10_kntr_verdi])),
                                                            RegData$Undergruppe)])
                                               },
                                               multiple = TRUE)})

  output$undergruppe2 <- renderUI({selectInput(inputId = "undergruppe2_verdi", label = "Velg undergruppe(r) nivå 2",
                                               choices = if (is.null(input$undergruppe1_verdi)){
                                                 setNames(-1, 'Ingen')
                                               } else {
                                                 setNames(sort(unique(RegData$Undergruppe2[RegData$Undergruppe %in% input$undergruppe1_verdi])),
                                                          RegData$Undergruppe2_label[match(sort(
                                                            unique(RegData$Undergruppe2[RegData$Undergruppe %in% input$undergruppe1_verdi])),
                                                            RegData$Undergruppe2)])
                                               },
                                               multiple = TRUE)})

  observe(
    if (is.null(input$diagnosegr)) {
      shinyjs::hide(id = 'icd10_kntr')
    } else {
      shinyjs::show(id = 'icd10_kntr')
    }
  )
  observe(
    if (is.null(input$icd10_kntr_verdi)){
      shinyjs::hide(id = 'undergruppe1')
    } else {
      shinyjs::show(id = 'undergruppe1')
    }
  )
  observe(
    if (is.null(input$undergruppe1_verdi)){
      shinyjs::hide(id = 'undergruppe2')
    } else {
      shinyjs::show(id = 'undergruppe2')
    }
  )



  output$Figur1 <- renderPlot({

    MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                     erMann = as.numeric(input$erMann), egenavd = as.numeric(input$egenavd), forlop = as.numeric(input$forlop),
                     maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil, avdod = input$avdod,
                     UtredningsaarFra =as.numeric(input$Utredningsaar[1]), UtredningsaarTil =as.numeric(input$Utredningsaar[2]),
                     debutAlderFra = as.numeric(input$debutalder[1]), debutAlderTil =as.numeric(input$debutalder[2]),
                     diagnosegr = if (!is.null(input$diagnosegr)) {as.numeric(input$diagnosegr)} else {-1},
                     diagnose = if (!is.null(input$icd10_kntr_verdi)) {input$icd10_kntr_verdi} else {'-1'},
                     undergr = if (!is.null(input$undergruppe1_verdi)) {as.numeric(input$undergruppe1_verdi)} else {-1},
                     undergr2 = if (!is.null(input$undergruppe2_verdi)) {as.numeric(input$undergruppe2_verdi)} else {-1},
                     reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg)
  }, width = 700, height = 700)
  # , height = function() {                       # Hvis du ønsker automatisk resizing
  #   1*session$clientData$output_Figur1_width
  # }
  # )
  # , diagnose = input$icd10_kntr_verdi

  tabellReager <- reactive({
    TabellData <- MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                                   erMann = as.numeric(input$erMann), egenavd = as.numeric(input$egenavd), forlop = as.numeric(input$forlop),
                                   maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil, avdod = input$avdod,
                                   UtredningsaarFra =as.numeric(input$Utredningsaar[1]), UtredningsaarTil =as.numeric(input$Utredningsaar[2]),
                                   debutAlderFra = as.numeric(input$debutalder[1]), debutAlderTil =as.numeric(input$debutalder[2]),
                                   diagnosegr = if (!is.null(input$diagnosegr)) {as.numeric(input$diagnosegr)} else {-1},
                                   diagnose = if (!is.null(input$icd10_kntr_verdi)) {input$icd10_kntr_verdi} else {'-1'},
                                   undergr = if (!is.null(input$undergruppe1_verdi)) {as.numeric(input$undergruppe1_verdi)} else {-1},
                                   undergr2 = if (!is.null(input$undergruppe2_verdi)) {as.numeric(input$undergruppe2_verdi)} else {-1},
                                   reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg)
  })

  output$Tabell1 <- function() {

    TabellData <- tabellReager()
    if (input$enhetsUtvalg == 1) {
      Tabell1 <- TabellData$Antall %>%
        mutate(Kategori = rownames(.)) %>%
        select(Kategori, everything()) %>%
        mutate(AndelHoved = 100*AntHoved/NHoved) %>%
        mutate(AndelRest= 100*AntRest/Nrest)
      Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
      names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel', 'Antall', 'N', 'Andel')
      Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1)) %>%
        kable_styling("hover", full_width = F) %>%
        add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
    } else {
      Tabell1 <- TabellData$Antall %>%
        mutate(Kategori = rownames(.)) %>%
        select(Kategori, everything()) %>%
        mutate(AndelHoved = 100*AntHoved/NHoved)
      names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel')
      Tabell1 %>%
        knitr::kable("html", digits = c(0,0,0,1)) %>%
        kable_styling("hover", full_width = F)
    }

  }

  output$lastNed <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, Sys.time(), '.csv')
    },

    content = function(file){
      TabellData <- tabellReager()
      if (input$enhetsUtvalg == 1) {
        Tabell1 <- TabellData$Antall %>%
          mutate(Kategori = rownames(.)) %>%
          select(Kategori, everything()) %>%
          mutate(AndelHoved = 100*AntHoved/NHoved) %>%
          mutate(AndelRest= 100*AntRest/Nrest)
        Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
      } else {
        Tabell1 <- TabellData$Antall %>%
          mutate(Kategori = rownames(.)) %>%
          select(Kategori, everything()) %>%
          mutate(AndelHoved = 100*AntHoved/NHoved)
      }
      write.csv2(Tabell1, file, row.names = F)
    }
  )

  output$lastNedBilde <- downloadHandler(
    filename = function(){
      paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
    },

    content = function(file){
      MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                       erMann = as.numeric(input$erMann), egenavd = as.numeric(input$egenavd), forlop = as.numeric(input$forlop),
                       maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil, avdod = input$avdod,
                       UtredningsaarFra =as.numeric(input$Utredningsaar[1]), UtredningsaarTil =as.numeric(input$Utredningsaar[2]),
                       debutAlderFra = as.numeric(input$debutalder[1]), debutAlderTil =as.numeric(input$debutalder[2]),
                       diagnosegr = if (!is.null(input$diagnosegr)) {as.numeric(input$diagnosegr)} else {-1},
                       diagnose = if (!is.null(input$icd10_kntr_verdi)) {input$icd10_kntr_verdi} else {'-1'},
                       undergr = if (!is.null(input$undergruppe1_verdi)) {as.numeric(input$undergruppe1_verdi)} else {-1},
                       undergr2 = if (!is.null(input$undergruppe2_verdi)) {as.numeric(input$undergruppe2_verdi)} else {-1},
                       reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg, outfile = file)
    }
  )


  callModule(forGrVar, "forgrvar", rID = reshID())
  callModule(kumulativAndel, "kumAnd", rID = reshID())
  callModule(tabell, "muskeltabell")

  #Navbarwidget
  output$appUserName <- renderText(rapbase::getUserFullName(session))
  output$appOrgName <- renderText(rapbase::getUserReshId(session))

  # Brukerinformasjon
  userInfo <- rapbase::howWeDealWithPersonalData(session)
  observeEvent(input$userInfo, {
    shinyalert("Dette vet Rapporteket om deg:", userInfo,
               type = "", imageUrl = "rap/logo.svg",
               closeOnEsc = TRUE, closeOnClickOutside = TRUE,
               html = TRUE, confirmButtonText = "Den er grei!")
  })

}

# Run the application
shinyApp(ui = ui, server = server)

