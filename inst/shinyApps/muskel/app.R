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
  rm(list = ls())
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
  reshID <- 101719
}

RegData <- MuskelPreprosess(RegData=RegData)

diagnosegrvalg <- sort(unique(RegData$Diagnosegr))
names(diagnosegrvalg) <- RegData$Diagnosegr_label[match(diagnosegrvalg, RegData$Diagnosegr)]
diagnosegrvalg <- c(diagnosegrvalg, 'Ikke valgt'= '-1')

######################################################################

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(title = "RAPPORTEKET MUSKELREGISTERET", theme = "bootstrap.css",
                 tabPanel("Fordelingsfigurer",
                          sidebarLayout(
                            sidebarPanel(
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
                              selectInput(inputId = "diagnosegr", selected = diagnosegrvalg[5], label = "Velg diagnosegruppe(r)",
                                          choices = diagnosegrvalg, multiple = TRUE),
                              uiOutput(outputId = 'icd10_kntr'),
                              sliderInput("mpg", "mpg Limit",
                                          min = 11, max = 33, value = 20)
                            ),
                            mainPanel(tabsetPanel(
                              tabPanel("Figur",
                                       plotOutput("Figur1", height="auto")),
                              tabPanel("Tabell",
                                       tableOutput("Tabell1"))
                            )
                            )

                          )
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
                 ),
                 tabPanel("FigType 3",
                          tabsetPanel(
                            tabPanel("Report 3a",
                                     sidebarLayout(
                                       sidebarPanel(),
                                       mainPanel()
                                     )
                            ),
                            tabPanel("Report 3b",
                                     sidebarLayout(
                                       sidebarPanel(),
                                       mainPanel()
                                     )
                            ),
                            tabPanel("Report 3c",
                                     sidebarLayout(
                                       sidebarPanel(),
                                       mainPanel()
                                     )
                            ),
                            tabPanel("Report 3d",
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

  output$sampleUcControl <- renderUI({
    selectInput(inputId = "sampleUc", label = "Sample user ctrl",
                choices = c("How", "it", "will", "look"))
  })

  output$icd10_kntr <- renderUI({selectInput(inputId = "icd10_kntr_verdi", label = "Velg diagnosekode(r)",
                                             choices = sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in% as.numeric(input$diagnosegr)])),
                                             multiple = TRUE)})

  output$Figur1 <- renderPlot({

    MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                     maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
                     diagnosegr = as.numeric(input$diagnosegr), reshID = reshID, enhetsUtvalg = input$enhetsUtvalg)

  }, height = function() {
    1*session$clientData$output_Figur1_width
  }
  )

  output$Tabell1 <- function() {
    req(input$mpg)
    mtcars %>%
      mutate(car = rownames(.)) %>%
      select(car, everything()) %>%
      filter(mpg <= input$mpg) %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F) %>%
      add_header_above(c(" ", "Group 1" = 5, "Group 2" = 6))
  }

}

# Run the application
shinyApp(ui = ui, server = server)



#
# # Define UI for application that draws a histogram
# ui <- navbarPage(
#   title = 'MUSKELREGISTERET',
#   # titlePanel('Muskelregisteret'),
#
#   tabPanel(
#     "Fordelingsfigurer",
#     # Sidebar with a slider input for number of bins
#     sidebarLayout(
#       sidebarPanel(
# selectInput(inputId = "valgtVar", label = "Velg variabel",
#             choices = c('PeriodiskeParalyser', 'Alder', 'Utdanning')),
# dateInput(inputId = 'datoFra', value = '2008-01-01', min = '2008-01-01',
#           label = "F.o.m. dato", language="nb"),
# dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
#           label = "T.o.m. dato", language="nb"),
# sliderInput(inputId="alder", label = "Alder", min = 0,
#             max = 120, value = c(0, 120)),
# selectInput(inputId = "diagnosegr", selected = diagnosegrvalg[1], label = "Velg diagnosegruppe(r)",
#             choices = diagnosegrvalg, multiple = TRUE),
# uiOutput(outputId = 'icd10_kntr')
#       ),
#
#       # Show a plot of the generated distribution
#       mainPanel(
#         plotOutput("distPlot", height="auto")
#       )
#     )
#
#
#   )
#
# )
#
#
# # Define server logic required to draw a histogram
# server <- function(input, output, session) {
#
# output$icd10_kntr <- renderUI({selectInput(inputId = "icd10_kntr_verdi", label = "Velg diagnosekode(r)",
#                                           choices = sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in% as.numeric(input$diagnosegr)])),
#                                           multiple = TRUE)})
#
# output$distPlot <- renderPlot({
#
#   MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
#                    maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
#                    diagnosegr = input$diagnosegr)
#
# }, height = function() {
#   session$clientData$output_distPlot_width
# }
# )
#
#
# }
#
# # Run the application
# shinyApp(ui = ui, server = server)




#
#       tabPanel('Noe annet',
#
#                # Sidebar with a slider input for number of bins
#                sidebarLayout(
#                  sidebarPanel(
#                    selectInput(inputId = "valgtVar1", label = "Velg variabel",
#                                choices = c('PeriodiskeParalyser', 'Alder', 'Utdanning'))
#                    # dateInput(inputId = 'datoFra1', value = '2008-01-01', min = '2008-01-01',
#                    #           label = "F.o.m. dato", language="nb"),
#                    # dateInput(inputId = 'datoTil1', value = Sys.Date(), min = '2012-01-01',
#                    #           label = "T.o.m. dato", language="nb"),
#                    # sliderInput(inputId="alder1", label = "Alder", min = 0,
#                    #             max = 120, value = c(0, 120)),
#                    # selectInput(inputId = "diagnosegr1", selected = diagnosegrvalg[1], label = "Velg diagnosegruppe(r)",
#                    #             choices = diagnosegrvalg, multiple = TRUE)
#                  ),
#
#                  # Show a plot of the generated distribution
#                  mainPanel(
#                    plotOutput("distPlot")
#                  )
#                )
#       )
#
#     )
