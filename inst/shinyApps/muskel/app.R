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
library(lubridate)
library(shinyjs)



system.file("shinyApps/muskel/R/startside.R",package = "muskel") %>%
  source(encoding = "UTF-8")
system.file("shinyApps/muskel/R/dataOgVar.R",package = "muskel") %>%
  source(encoding = "UTF-8")
system.file("shinyApps/muskel/R/forAndGr.R",package = "muskel") %>%
  source(encoding = "UTF-8")
system.file("shinyApps/muskel/R/kumandel.R",package = "muskel") %>%
  source(encoding = "UTF-8")
system.file("shinyApps/muskel/R/tabell.R",package = "muskel") %>%
  source(encoding = "UTF-8")
system.file("shinyApps/muskel/R/dataDump.R",package = "muskel") %>%
  source(encoding = "UTF-8")


addResourcePath('rap', system.file('www', package='rapbase'))
regTitle <-  "RAPPORTEKET MUSKELREGISTERET"
logo <- includeHTML(system.file('www/logo.svg', package='rapbase'))
logoCode <- paste0("var header = $('.navbar> .container-fluid');\n",
                   "header.append('<div class=\"navbar-brand\" style=\"float:left;font-size:75%\">",
                   logo,
                   "</div>');\n",
                   "console.log(header)")
logoWidget <- tags$script(shiny::HTML(logoCode))



# Define UI for application that draws a histogram
ui <- navbarPage(#title = "RAPPORTEKET MUSKELREGISTERET", theme = "bootstrap.css",
                title = div(a(includeHTML(system.file('www/logo.svg', package='rapbase'))),
                            regTitle),
                windowTitle = regTitle,
                theme = "rap/bootstrap.css",
                tabPanel("Startside",
                         shinyalert::useShinyalert(),
                         rapbase::appNavbarUserWidget(user = uiOutput("appUserName"),
                                                      organization = uiOutput("appOrgName"),
                                                      addUserInfo = TRUE),

                         tags$head(tags$link(rel="shortcut icon", href="rap/favicon.ico"),
                         tags$link(rel="stylesheet", type = "text/css", href="dataDump.css")),

                         startside()
                         ),

                 tabPanel("Fordelingsfigurer",

                          # sidebarLayout(
                          sidebarPanel(
                            shinyjs::useShinyjs(),
                            div(id = "sbpFordeling",
                            selectInput(inputId = "valgtVar", label = "Velg variabel",
                                        choices = varvalg),
                            dateInput(inputId = 'datoFra', value = '2008-01-01', min = '2008-01-01',
                                      label = "F.o.m. dato", language="nb"),
                            dateInput(inputId = 'datoTil', value = Sys.Date(), min = '2012-01-01',
                                      label = "T.o.m. dato", language="nb"),
                            sliderInput(inputId="alder", label = "Alder", min = 0,
                                        max = 120, value = c(0, 120)),
                            uiOutput("SC"),
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
                                        choices = c('Ja'='Ja', 'Nei'='Nei'), selected = "Nei"),
                            sliderInput(inputId="Utredningsaar", label = "Utredningsår", sep='', min = 1950,
                                        max = as.numeric(format(Sys.Date(), '%Y')), value = c(1950, as.numeric(format(Sys.Date(), '%Y')))),
                            sliderInput(inputId="debutalder", label = "Debutalder", min = 0,
                                        max = 90, value = c(0, 90)),
                            selectInput(inputId = "bildeformat", label = "Velg bildeformat",
                                        choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))),
                            actionLink(inputId="nullstillFordeling",
                                              style="color:black" ,
                                              label = "Nullstill Valg")
                          ),

                          mainPanel(tabsetPanel(id = "tab",
                            tabPanel("Figur", value = "andelFig",
                                     plotOutput("Figur1", height="auto"),
                                     downloadButton("lastNedBilde", "Last ned bilde")),
                            tabPanel("Tabell",value = "andelTab",
                                     tableOutput("Tabell1"),
                                     downloadButton("lastNed", "Last ned tabell"))
                          ))
                 ),
                tabPanel("Fordelinger etter grupperingsvariabler",
                  forGrVarUI(id = "forgrvar")
                ),
                tabPanel("Kumulative andeler",
                  kumulativAndelUI(id = "kumAnd")
                ),
                tabPanel("Administrative tabeller",
                         tabellUI("muskeltabell")


                ),
                tabPanel(
                  "Datadump", dataDumpUI("dataDumpMuskel")

                ),
                shiny::navbarMenu("Verktøy",
                                  # shiny::tabPanel(
                                  #   "Utsending",
                                  #   shiny::sidebarLayout(
                                  #     shiny::sidebarPanel(
                                  #       rapbase::autoReportOrgInput("norgastDispatch"),
                                  #       rapbase::autoReportInput("norgastDispatch")
                                  #     ),
                                  #     shiny::mainPanel(
                                  #       rapbase::autoReportUI("norgastDispatch")
                                  #     )
                                  #   )
                                  # ),

                                  shiny::tabPanel(
                                    "Eksport",
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(
                                        rapbase::exportUCInput("muskelExport")
                                      ),
                                      shiny::mainPanel(
                                        rapbase::exportGuideUI("muskelExportGuide")
                                      )
                                    )
                                  ),

                                  shiny::tabPanel(
                                    "Bruksstatistikk",
                                    shiny::sidebarLayout(
                                      shiny::sidebarPanel(rapbase::statsInput("muskelStats")),
                                      shiny::mainPanel(
                                        rapbase::statsUI("muskelStats"),
                                        rapbase::statsGuideUI("muskelStatsGuide")
                                      )
                                    )
                                  )
                )

)


#
server <- function(input, output, session) {

  if (rapbase::isRapContext()) {
    rapbase::appLogger(session = session, msg = "Muskel: shiny app starter")
    reshID <- rapbase::getUserReshId(session)
    userRole <- rapbase::getUserRole(session)
  } else {
    reshID <- 101719
    userRole <- 'SC'
  }

  # reshID <- reactive({
  #   ifelse(onServer,as.numeric(rapbase::getUserReshId(session)),101719)
  # })
  # userRole <- reactive({
  #   ifelse(onServer, rapbase::getUserRole(session), 'SC')
  # })
  # if (onServer){
  #   rapbase::appLogger(session, msg = "Muskel: shiny app starter")
  # }

  observeEvent(req(input$nullstillFordeling), {shinyjs::reset("sbpFordeling")})


  output$icd10_kntr <- renderUI({selectInput(inputId = "icd10_kntr_verdi", label = "Velg diagnosekode(r)",
                                             choices = if (is.null(input$diagnosegr)  ){
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
  output$SC <- renderUI({
    if (userRole == "SC"){
      selectInput("shSelect", label = "Velg Avdeling",
                  choices = avdValg, selected = reshID)
    }
  })

  resh <- reactive({
    if (userRole == "SC") {
      input$shSelect
    }else{
      reshID
    }
  })

  observe(
    if (is.null(input$diagnosegr) | length(input$diagnosegr) != 1) {
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
                     reshID = resh(), enhetsUtvalg = input$enhetsUtvalg)
  }, width = 700, height = 700)

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
                                   reshID = resh(), enhetsUtvalg = input$enhetsUtvalg)
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
                       reshID = resh(), enhetsUtvalg = input$enhetsUtvalg, outfile = file)
    }
  )


  callModule(forGrVar, "forgrvar", rID = reshID, ss = session)
  callModule(kumulativAndel, "kumAnd", rID = reshID, ss = session)
  callModule(tabell, "muskeltabell", ss = session)
  callModule(dataDump, "dataDumpMuskel", mainSession = session, reshID = reshID,userRole=userRole)


  # Eksport  #
  rapbase::exportUCServer("muskelExport", "muskel")
  ## veileding
  rapbase::exportGuideServer("muskelExportGuide", "muskel")

  ## Stats
  rapbase::statsServer("muskelStats", registryName = "muskel",
                       eligible = (userRole == "SC"))
  rapbase::statsGuideServer("muskelStatsGuide", registryName = "muskel")


  shiny::observe({
    if (onServer) {
      if (input$tab == "andelFig") {
        mldandel <- paste(
          "Muskel: figur - fordeling. variabel -",
          input$valgtVar
        )
      } else if (input$tab == "andelTab") {
        mldandel <- paste(
          "Muskel: tabell - fordeling. variabel -",
          input$valgtVar
        )
      }
      rapbase::repLogger(
        session = session,
        msg = mldandel
      )
      mldNLF <- paste(
        "Muskel: nedlasting figur - Fordeling. variabel -",
        input$valgtVar
      )
      mldNLT <- paste(
        "Muskel: nedlasting tabell - Fordeling. variabel -",
        input$valgtVar
      )
      shinyjs::onclick(
        "lastNedBilde",
        rapbase::repLogger(
          session = session,
          msg = mldNLF
        )
      )
      shinyjs::onclick(
        "lastNed",
        rapbase::repLogger(
          session = session,
          msg = mldNLT
        )
      )
    }
  })

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

