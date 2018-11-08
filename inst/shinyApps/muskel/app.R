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
library(kableExtra)

context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
  RegData <- MuskelHentRegData()
} else {
  # rm(list = ls())
  ForlopsData <- read.table('I:/muskel/ForlopsOversikt2018-09-25 08-59-11.txt', header=TRUE, sep=';')
  ForlopsData <- ForlopsData[, c("ForlopsID", "AvdRESH", "HovedDato", "SykehusNavn", "erMann", "BasisRegStatus", "PasientAlder",
                                 "PasientID", "ForlopsType1Num", "ForlopsType1", "Fylke", "Fylkenr", "Avdod", "AvdodDato")]
  RegData <- read.table('I:/muskel/AlleVarNum2018-09-25 08-59-09.txt', header=TRUE, sep=';')
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
  RegDataLabel <- read.table('I:/muskel/AlleVar2018-09-25 08-59-07.txt', header=TRUE, sep=';')
  RegDataLabel <- RegDataLabel[, c("ForlopsID", "DiagnoseStiltAv",
                                   "Undergruppe", "Undergruppe2", "FoelgesOppAvIns", "Utdanning", "Sivilstatus",
                                   'Arvegang', 'Gangfunksjon')]
  RegData <- merge(RegData, RegDataLabel, by.x = 'ForlopsID', by.y = 'ForlopsID', suffixes = c("","_label"))
  RegData$Undergruppe_label <- iconv(RegData$Undergruppe_label, from = 'UTF-8', to = '')
  RegData$Undergruppe2_label <- iconv(RegData$Undergruppe2_label, from = 'UTF-8', to = '')
  rm(list=c('ForlopsData', 'RegDataLabel'))
  # reshID <- 101719
}

RegData <- MuskelPreprosess(RegData=RegData)

diagnosegrvalg <- sort(unique(RegData$Diagnosegr))
names(diagnosegrvalg) <- RegData$Diagnosegr_label[match(diagnosegrvalg, RegData$Diagnosegr)]
diagnosegrvalg <- c(diagnosegrvalg, 'Ikke valgt'= '-1')
varvalg <- c('PeriodiskeParalyser', 'Alder', 'Utdanning')

######################################################################

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "RAPPORTEKET MUSKELREGISTERET", theme = "bootstrap.css",
                 tabPanel("Fordelingsfigurer",
                          # sidebarLayout(
                            sidebarPanel(
                              shinyjs::useShinyjs(),
                              selectInput(inputId = "valgtVar", label = "Velg variabel",
                                          choices = c('PeriodiskeParalyser', 'Alder', 'Utdanning')),
                              dateInput(inputId = 'datoFra', value = '2008-01-01', min = '2008-01-01',
                                        label = "F.o.m. dato", language="nb"),
                              dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                                        label = "T.o.m. dato", language="nb"),
                              selectInput(inputId = "enhetsUtvalg", label = "Kjør rapport for",
                                          choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
                              sliderInput(inputId="alder", label = "Alder", min = 0,
                                            max = 120, value = c(0, 120)),
                              div(id = 'diagnoser',
                                  selectInput(inputId = "diagnosegr", selected = diagnosegrvalg[5], label = "Velg diagnosegruppe(r)",
                                              choices = diagnosegrvalg, multiple = TRUE),
                                  uiOutput(outputId = 'icd10_kntr')
                              ),
                              selectInput(inputId = "bildeformat", label = "Velg bildeformat",
                                          choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
                            ),
                            mainPanel(tabsetPanel(
                              tabPanel("Figur",
                                       plotOutput("Figur1", height="auto"), downloadButton("lastNedBilde", "Last ned bilde")),
                              tabPanel("Tabell",
                                       tableOutput("Tabell1"), downloadButton("lastNed", "Last ned tabell")),
                              tabPanel("Tabell 2",
                                       tableOutput("Tabell2"))
                            )
                            )

                          # )
                 ),
                 tabPanel("FigType 2",
                          tabsetPanel(
                            tabPanel("Report 2a",
                                     sidebarLayout(
                                       sidebarPanel(),
                                       mainPanel()
                                     )
                            ),
                            tabPanel("Report 2b",
                                     sidebarLayout(
                                       sidebarPanel(),
                                       mainPanel()
                                     )
                            ),
                            tabPanel("Report 2c",
                                     sidebarLayout(
                                       sidebarPanel(),
                                       mainPanel()
                                     )
                            )
                          )
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output, session) {

  if (context == "TEST" | context == "QA" | context == "PRODUCTION") {
      # bruker <- function() {rapbase::getShinyUserRole(session, testCase = TRUE)}
      bruker <- function() {'LC'}
      # reshID <- reactive({rapbase::getShinyUserReshId(session, testCase = TRUE)})
      reshID <- 101719
  } else {
    bruker <- function() {'LC'}
    reshID <- 101719
  }
  if (bruker() != 'SC') {
    shinyjs::hide(id = 'diagnoser')
  }


  output$icd10_kntr <- renderUI({selectInput(inputId = "icd10_kntr_verdi", label = "Velg diagnosekode(r)",
                                             choices = sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in% as.numeric(input$diagnosegr)])),
                                             multiple = TRUE)})

  output$Figur1 <- renderPlot({

    MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                     maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
                     diagnosegr = as.numeric(input$diagnosegr), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg)

  }, width = 700, height = 700)
  # , height = function() {                       # Hvis du ønsker automatisk resizing
  #   1*session$clientData$output_Figur1_width
  # }
  # )

  tabellReager <- reactive({
    TabellData <- MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                                   maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
                                   diagnosegr = as.numeric(input$diagnosegr), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg)
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
      MuskelFigAndeler(RegData = RegData, outfile = file, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                       maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
                       diagnosegr = as.numeric(input$diagnosegr), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg)
    }
  )


  # output$Tabell2 <- function() {
  #   req(input$mpg)
  #   mtcars %>%
  #     mutate(car = rownames(.)) %>%
  #     select(car, everything()) %>%
  #     filter(mpg <= 20) %>%
  #     knitr::kable("html") %>%
  #     kable_styling("striped", full_width = F) %>%
  #     add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
  # }

}

# Run the application
shinyApp(ui = ui, server = server)

