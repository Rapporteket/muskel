#' ui-modul for fordelingsfigurer i NORNMD sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul fordelingsfigur
#'
#' @export
fordelingsfig_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(
      # width = 3,
      id = ns("id_fordeling_panel"),
      shinyjs::useShinyjs(),
      div(
        id = "sbpFordeling",
        uiOutput(ns("valgtVar_ui")),
        dateInput(inputId = ns('datoFra'), value = '2008-01-01',
                  min = '2008-01-01',
                  label = "F.o.m. dato", language="nb"),
        dateInput(inputId = ns('datoTil'), value = Sys.Date(),
                  min = '2012-01-01',
                  label = "T.o.m. dato", language="nb"),
        sliderInput(inputId=ns("alder"), label = "Alder", min = 0,
                    max = 120, value = c(0, 120)),
        # uiOutput("SC"),
        selectInput(
          inputId = ns("egenavd"), label = "Pasientgruppe", selected = 0,
          choices = c('Registrert ved HF'=0, 'Følges opp ved HF'=1,
                      'Diagnostisert ved HF'=2,
                      'Bosatt i fylke'=3)),
        selectInput(
          inputId = ns("enhetsUtvalg"), label = "Kjør rapport for",
          selected = 1,
          choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1,
                      'Egen avd.'=2)),
        selectInput(inputId = ns("forlop"), label = "Velg forløpstype",
                    choices = c('Alle'=99, 'Basisregistrering'=1,
                                '5-årsoppfølging'=2, 'Ad-hoc'=3)),
        selectInput(inputId = ns("erMann"), label = "Kjønn",
                    choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1)),
        uiOutput(outputId = ns("diagnosegr_ui")),
        uiOutput(outputId = ns('icd10_kntr')),
        uiOutput(outputId = ns('undergruppe1')),
        uiOutput(outputId = ns('undergruppe2')),
        selectInput(inputId = ns("avdod"), label = "Inkluder avdøde",
                    choices = c('Ja'='Ja', 'Nei'='Nei'), selected = "Nei"),
        sliderInput(
          inputId=ns("Utredningsaar"), label = "Utredningsår",
          sep='', min = 1950,
          max = as.numeric(format(Sys.Date(), '%Y')),
          value = c(1950, as.numeric(format(Sys.Date(), '%Y')))),
        sliderInput(inputId=ns("debutalder"), label = "Debutalder", min = 0,
                    max = 90, value = c(0, 90)),
        selectInput(inputId = ns("bildeformat"), label = "Velg bildeformat",
                    choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))
      ),
      actionLink(inputId=ns("nullstillFordeling"),
                 style="color:black" ,
                 label = "Nullstill Valg")
    ),
    mainPanel(
      tabsetPanel(
        id = ns("tab"),
        tabPanel("Figur", value = "andelFig",
                 plotOutput(ns("Figur1"), height="auto"),
                 downloadButton(ns("lastNedBilde"), "Last ned bilde")),
        tabPanel("Tabell",value = "andelTab",
                 uiOutput(ns("utvalg")),
                 tableOutput(ns("Tabell1")),
                 downloadButton(ns("lastNed"), "Last ned tabell"))
      )
    )
  )
}

#' server-modul for fordelingsfigurer i NORNMD sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul fordelingsfigur
#'
#' @export
fordelingsfig_server <- function(id, RegData, reshID) {
  moduleServer(
    id,
    function(input, output, session) {

      observeEvent(req(input$nullstillFordeling), {
        shinyjs::reset("sbpFordeling")})

      # resh <- reactive({reshID})

      output$valgtVar_ui <-
        renderUI({
          ns <- session$ns
          selectInput(inputId = ns("valgtVar"), label = "Velg variabel",
                      choices = muskel::varvalg)
        })


      output$diagnosegr_ui <-
        renderUI({
          ns <- session$ns
          selectInput(
            inputId = ns("diagnosegr"),
            label = "Velg diagnosegruppe(r)",
            choices = setNames(
              sort(unique(RegData$Diagnosegr)),
              RegData$Diagnosegr_label[match(sort(unique(RegData$Diagnosegr)),
                                             RegData$Diagnosegr)]),
            multiple = TRUE)
        })

      output$icd10_kntr <-
        renderUI({
          ns <- session$ns
          if (!is.null(input$diagnosegr) ){
            selectInput(
              inputId = ns("icd10_kntr_verdi"),
              label = "Velg diagnosekode(r)",
              choices = if (!is.null(input$diagnosegr) ){
                sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in%
                                                as.numeric(input$diagnosegr)]))
              },
              multiple = TRUE)
          }
        })

      output$undergruppe1 <-
        renderUI({
          ns <- session$ns
          if (!is.null(input$icd10_kntr_verdi)){
            selectInput(
              inputId = ns("undergruppe1_verdi"),
              label = "Velg undergruppe(r)",
              choices = if (!is.null(input$icd10_kntr_verdi)){
                setNames(sort(unique(
                  RegData$Undergruppe[RegData$DiagICD10 %in%
                                        input$icd10_kntr_verdi])),
                  RegData$Undergruppe_label[match(sort(
                    unique(RegData$Undergruppe[RegData$DiagICD10 %in%
                                                 input$icd10_kntr_verdi])),
                    RegData$Undergruppe)])},
              multiple = TRUE)
          }
        })

      output$undergruppe2 <-
        renderUI({
          ns <- session$ns
          if (!is.null(input$undergruppe1_verdi)){
            selectInput(
              inputId = ns("undergruppe2_verdi"),
              label = "Velg undergruppe(r) nivå 2",
              choices = if (!is.null(input$undergruppe1_verdi)){
                setNames(sort(unique(
                  RegData$Undergruppe2[RegData$Undergruppe %in%
                                         input$undergruppe1_verdi])),
                  RegData$Undergruppe2_label[match(sort(
                    unique(RegData$Undergruppe2[RegData$Undergruppe %in%
                                                  input$undergruppe1_verdi])),
                    RegData$Undergruppe2)])},
              multiple = TRUE)
          }
        })


      tabellReager <- reactive({
        TabellData <- MuskelBeregnAndeler(
          RegData = RegData,
          valgtVar = req(input$valgtVar),
          minald=as.numeric(input$alder[1]),
          erMann = as.numeric(input$erMann),
          egenavd = as.numeric(input$egenavd),
          forlop = as.numeric(input$forlop),
          maxald=as.numeric(input$alder[2]),
          datoFra = input$datoFra, datoTil = input$datoTil,
          avdod = input$avdod,
          UtredningsaarFra =as.numeric(input$Utredningsaar[1]),
          UtredningsaarTil =as.numeric(input$Utredningsaar[2]),
          debutAlderFra = as.numeric(input$debutalder[1]),
          debutAlderTil =as.numeric(input$debutalder[2]),
          diagnosegr = if (!is.null(input$diagnosegr)) {
            as.numeric(input$diagnosegr)} else {-1},
          diagnose = if (!is.null(input$icd10_kntr_verdi)) {
            input$icd10_kntr_verdi} else {'-1'},
          undergr = if (!is.null(input$undergruppe1_verdi)) {
            as.numeric(input$undergruppe1_verdi)} else {-1},
          undergr2 = if (!is.null(input$undergruppe2_verdi)) {
            as.numeric(input$undergruppe2_verdi)} else {-1},
          reshID = reshID(), enhetsUtvalg = input$enhetsUtvalg)
      })

      output$Figur1 <- renderPlot({
        MuskelPlotAndeler(plotdata=tabellReager())
      }, width = 700, height = 700)

      output$utvalg <- renderUI({
        TabellData <- tabellReager()
        tagList(
          h3(HTML(paste0(TabellData$tittel, '<br />'))),
          h5(HTML(paste0(TabellData$utvalgTxt, '<br />')))
        )})

      tabellfunk_fordeling <- function(data, enhetsUtvalg) {
        TabellData <- data
        if (enhetsUtvalg == 1) {
          Tabell1 <- TabellData$Antall %>%
            dplyr::mutate(Kategori = rownames(.)) %>%
            dplyr::select(Kategori, everything()) %>%
            dplyr::mutate(AndelHoved = 100*AntHoved/NHoved) %>%
            dplyr::mutate(AndelRest= 100*AntRest/Nrest)
          Tabell1 <- Tabell1[, c(1,2,4,6,3,5,7)]
        } else {
          Tabell1 <- TabellData$Antall %>%
            dplyr::mutate(Kategori = rownames(.)) %>%
            dplyr::select(Kategori, everything()) %>%
            dplyr::mutate(AndelHoved = 100*AntHoved/NHoved)
        }
      }

      output$Tabell1 <- function() {
        Tabell1 <- tabellfunk_fordeling(tabellReager(), input$enhetsUtvalg)
        if (input$enhetsUtvalg == 1) {
          names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel',
                              'Antall', 'N', 'Andel')
          Tabell1 %>% knitr::kable("html", digits = c(0,0,0,1,0,0,1),
                                   row.names = FALSE) %>%
            kableExtra::kable_styling("hover", full_width = F) %>%
            kableExtra::add_header_above(c(" ", "Din avdeling" = 3, "Landet forøvrig" = 3))
        } else {
          names(Tabell1) <- c('Kategori', 'Antall', 'N', 'Andel')
          Tabell1 %>%
            knitr::kable("html", digits = c(0,0,0,1)) %>%
            kableExtra::kable_styling("hover", full_width = F)
        }
      }

      output$lastNed <- downloadHandler(
        filename = function(){
          paste0(input$valgtVar, Sys.time(), '.csv')
        },

        content = function(file){
          Tabell1 <- tabellfunk_fordeling(tabellReager(), input$enhetsUtvalg)
          write.csv2(Tabell1, file, row.names = F)
        }
      )

      output$lastNedBilde <- downloadHandler(
        filename = function(){
          paste0(input$valgtVar, Sys.time(), '.', input$bildeformat)
        },

        content = function(file){
          MuskelPlotAndeler(plotdata=tabellReager(), outfile = file)
        }
      )

      shiny::observe({
        if (rapbase::isRapContext()) {
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

    }
  )
}


