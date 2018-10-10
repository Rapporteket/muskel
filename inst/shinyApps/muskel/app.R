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
RegData <- MuskelPreprosess(RegData=RegData)

diagnosegrvalg <- sort(unique(RegData$Diagnosegr))
names(diagnosegrvalg) <- RegData$Diagnosegr_label[match(diagnosegrvalg, RegData$Diagnosegr)]
diagnosegrvalg <- c(diagnosegrvalg, 'Ikke valgt'= '')

######################################################################

library(shiny)

# Define UI for application that draws a histogram
ui <- navbarPage(
  title = 'MUSKELREGISTERET',
  # titlePanel('Muskelregisteret'),

  tabPanel(
    "Fordelingsfigurer",
    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        selectInput(inputId = "valgtVar", label = "Velg variabel",
                    choices = c('PeriodiskeParalyser', 'Alder', 'Utdanning')),
        dateInput(inputId = 'datoFra', value = '2008-01-01', min = '2008-01-01',
                  label = "F.o.m. dato", language="nb"),
        dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                  label = "T.o.m. dato", language="nb"),
        sliderInput(inputId="alder", label = "Alder", min = 0,
                    max = 120, value = c(0, 120)),
        selectInput(inputId = "diagnosegr", selected = diagnosegrvalg[1], label = "Velg diagnosegruppe(r)",
                    choices = diagnosegrvalg, multiple = TRUE),
        uiOutput(outputId = 'icd10_kntr')
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("distPlot")
      )
    )


  )

)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$icd10_kntr <- renderUI({selectInput(inputId = "icd10_kntr_verdi", label = "Velg diagnosekode(r)",
                                            choices = sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in% as.numeric(input$diagnosegr)])),
                                            multiple = TRUE)})

  output$distPlot <- renderPlot({

    MuskelFigAndeler(RegData = RegData, valgtVar = input$valgtVar, minald=as.numeric(input$alder[1]),
                     maxald=as.numeric(input$alder[2]), datoFra = input$datoFra, datoTil = input$datoTil,
                     diagnosegr = input$diagnosegr)

  })


}

# Run the application
shinyApp(ui = ui, server = server)




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
