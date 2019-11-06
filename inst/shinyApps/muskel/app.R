#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

######## Last data ########################################
library(shiny)
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



system.file("shinyApps/muskel/dataOgVar.R",package = "muskel") %>%
    source(encoding = "UTF-8")
# system.file("shinyApps/muskel/forAndGr.R",package = "muskel") %>%
#    source(encoding = "UTF-8")
# system.file("shinyApps/muskel/kumandel.R",package = "muskel") %>%
#    source(encoding = "UTF-8")
# system.file("shinyApps/muskel/tabell.R",package = "muskel") %>%
#    source(encoding = "UTF-8")

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
                                     plotOutput("Figur1", height="auto"),
                                     h3(paste0(system.file("shinyApps/muskel/app.R",package = "muskel"),
                                               system.file("shinyApps/muskel/dataOGvar.R",package = "muskel"),
                                               system.file("shinyApps/muskel/forAndGr.R",package = "muskel"),
                                               system.file("shinyApps/muskel/kumandel.R",package = "muskel"),
                                               system.file("shinyApps/muskel/tabell.R",package = "muskel"))),
                                     downloadButton("lastNedBilde", "Last ned bilde")),
                            tabPanel("Tabell",
                                     tableOutput("Tabell1"),

                                     downloadButton("lastNed", "Last ned tabell"))
                          )
                          )
                 ),
                tabPanel("Fordelinger etter grupperingsvariabler",
                  #forGrVarUI(id = "forgrvar")
                ),
                tabPanel("Kummulative andeler",
                         #kumulativAndelUI(id = "kumAnd")
                ),
                tabPanel("Pasient og forløpstabeller",
                         #tabellUI("muskeltabell")

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

#
# #  callModule(forGrVar, "forgrvar", rID = reshID())
#   callModule(kumulativAndel, "kumAnd", rID = reshID())
#   callModule(tabell, "muskeltabell")

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
  #rsconnect::showLogs()
}

# Run the application
shinyApp(ui = ui, server = server)

