#' ui-modul for kumulativ andel i NORNMD sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul
#'
#' @export
#'
kumulativAndel_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::sidebarLayout(
    shiny::sidebarPanel(div(
      id = ns("snPanel"),
      shiny::uiOutput(ns("var_ui")),
      shiny::dateRangeInput(ns("dato"), label = "Tidsperiode",
                            start = "2008-01-01", end = Sys.Date(),
                            language = "no",separator = "til",
                            format = "yyyy-mm-dd"),

      muskel::dateRangeInput2(ns("utrar"), label = "Utredningsår",
                              start = "1950-01-01", end = Sys.Date(),
                              language = "no",separator = "til",
                              minview = "years", format = "yyyy"),

      shiny::sliderInput(ns("ald"), label = "Debutalder",
                         min = 0, max = 90, value = c(0,90) ),

      shiny::selectInput(
        inputId = ns("psgr"), label = "Pasientgruppe",
        choices = c('Registrert ved HF'=0, 'Følges opp ved HF'=1,
                    'Diagnostisert ved HF'=2,
                    'Bosatt i fylke'=3), selected = 0),

      shiny::selectInput(ns("enh"), label = "Velg enhet(er)" ,
                         choices = c("Hele landet" = 0, "Egen avdeling" = 2),
                         selected = 0 ),

      uiOutput(outputId = ns("diagnosegr_ui")),
      uiOutput(outputId = ns('icd10_ui')),
      uiOutput(outputId = ns('undergruppe1_ui')),
      uiOutput(outputId = ns('undergruppe2_ui')),

      shiny::selectInput(ns("avdod"), label = "Inkluder avdøde",
                         choices = c("Ja" , "Nei" ),
                         selected = "Nei"  ),
      selectInput(inputId = ns("outfile"), label = "Velg bildeformat",
                  choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))),
      shiny::actionLink(inputId=ns("nullstill"),
                        style="color:black" ,
                        label = "Nullstill Valg")


    ),#sidebarPanel

    shiny::mainPanel(
      shiny::tabsetPanel(
        id = ns("tab"),
        shiny::tabPanel(
          "Figur", value = "fig",
          shiny::plotOutput(ns("Figur"), height="auto", hover = ns("hov")),
          shiny::downloadButton(ns("lastNedBilde"), "Last ned bilde")),
        shiny::tabPanel(
          "Tabell", value = "tab",
          shiny::tableOutput(ns("Tabell")),
          shiny::downloadButton(ns("lastNedTabell"), "Last ned tabell"))
      )#tabsetPanel
    )#mainPanel
  )#sidebarlayout
}

#' server-modul for kumulativ andel i NORNMD sin shiny-app på Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul
#'
#' @export
#'
kumulativAndel_server <- function(id, RegData, reshID, ss){
  moduleServer(
    id,
    function(input, output, session) {

      output$var_ui <- shiny::renderUI({
        ns <- session$ns
        shiny::selectInput(ns("var"), label = "Velg variabel",
                           choices = muskel::varValgtKumAnd,
                           selected = muskel::varValgtKumAnd[[1]])
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

      output$icd10_ui <-
        renderUI({
          ns <- session$ns
          if (!is.null(input$diagnosegr) ){
            selectInput(
              inputId = ns("icd10"),
              label = "Velg diagnosekode(r)",
              choices = if (!is.null(input$diagnosegr) ){
                sort(unique(RegData$DiagICD10[RegData$Diagnosegr %in%
                                                as.numeric(input$diagnosegr)]))
              },
              multiple = TRUE)
          }
        })

      output$undergruppe1_ui <-
        renderUI({
          ns <- session$ns
          if (!is.null(input$icd10)){
            selectInput(
              inputId = ns("undergruppe1"),
              label = "Velg undergruppe(r)",
              choices = if (!is.null(input$icd10)){
                setNames(sort(unique(
                  RegData$Undergruppe[RegData$DiagICD10 %in%
                                        input$icd10])),
                  RegData$Undergruppe_label[match(sort(
                    unique(RegData$Undergruppe[RegData$DiagICD10 %in%
                                                 input$icd10])),
                    RegData$Undergruppe)])},
              multiple = TRUE)
          }
        })

      output$undergruppe2_ui <-
        renderUI({
          ns <- session$ns
          if (!is.null(input$undergruppe1)){
            selectInput(
              inputId = ns("undergruppe2"),
              label = "Velg undergruppe(r) nivå 2",
              choices = if (!is.null(input$undergruppe1)){
                setNames(sort(unique(
                  RegData$Undergruppe2[RegData$Undergruppe %in%
                                         input$undergruppe1])),
                  RegData$Undergruppe2_label[match(sort(
                    unique(RegData$Undergruppe2[RegData$Undergruppe %in%
                                                  input$undergruppe1])),
                    RegData$Undergruppe2)])},
              multiple = TRUE)
          }
        })

      observeEvent(req(input$nullstill), {shinyjs::reset("snPanel")})
      resp <- reactive({
        tabData <- muskel::MuskelFigCumAndel(
          RegData = RegData, valgtVar = input$var, datoFra = min(input$dato),
          datoTil =   max(input$dato), debutAlderFra = input$ald[1], debutAlderTil = input$ald[2] ,
          UtredningsaarFra = lubridate::year(min(input$utrar)),
          UtredningsaarTil = lubridate::year(max(input$utrar)) ,
          diagnosegr = convNull(input$diagnosegr), diagnose = convNull(input$icd10),
          undergr = convNull(input$undergruppe1), undergr2 = convNull(input$undergruppe2),
          egenavd = as.numeric(input$psgr), enhetsUtvalg = as.numeric(input$enh) ,
          avdod = input$avdod ,reshID = reshID, outfile = "" )
      })

      #figure
      # observe({
        output$Figur <- shiny::renderPlot({
          muskel::MuskelFigCumAndel(
            RegData = RegData, valgtVar = req(input$var), datoFra = min(input$dato),
            datoTil =   max(input$dato), debutAlderFra = input$ald[1], debutAlderTil = input$ald[2] ,
            UtredningsaarFra = as.numeric(lubridate::year(min(input$utrar))),
            UtredningsaarTil = as.numeric(lubridate::year(max(input$utrar))) ,
            diagnosegr = convNull(input$diagnosegr), diagnose = convNull(input$icd10),
            undergr = convNull(input$undergruppe1), undergr2 = convNull(input$undergruppe2),
            egenavd = as.numeric(input$psgr), enhetsUtvalg = as.numeric(input$enh) ,
            avdod = input$avdod ,reshID = reshID, outfile = "" )
        },
        width = 700, height = 700)
      # })


      #download figure
      output$lastNedBilde <- downloadHandler(
        filename = function(){
          paste0(input$var, Sys.time(), '.', input$outfile)
        },
        content = function(file){
          muskel::MuskelFigCumAndel(
            RegData = RegData, valgtVar = input$var, datoFra = min(req(input$dato)),
            datoTil =   max(req(input$dato)), minald = input$ald[1], maxald = input$ald[2] ,
            UtredningsaarFra = lubridate::year(min(req(input$utrar))),
            UtredningsaarTil = lubridate::year(max(req(input$utrar))) ,
            diagnosegr = convNull(input$diagnosegr), diagnose = convNull(input$icd10),
            undergr = convNull(input$undergruppe1), undergr2 = convNull(input$undergruppe2),
            egenavd = as.numeric(input$psgr), enhetsUtvalg = as.numeric(input$enh) ,
            avdod = input$avdod ,reshID = reshID, outfile = file )
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
      shiny::observe({
        if (rapbase::isRapContext()) {
          if (req(input$tab) == "fig") {
            mldKA <- paste(
              "Muskel: figur - kummulative andeler. variabel -",
              input$var
            )
          } else if (req(input$tab) == "tab") {
            mldKA <- paste(
              "Muskel: tabell - kummulative andeler. variabel -",
              input$var
            )
          }
          rapbase::repLogger(
            session = ss,
            msg = mldKA
          )
          mldNLF <- paste(
            "Muskel: nedlasting figur - kummulative andeler. variabel -",
            input$var
          )
          mldNLT <- paste(
            "Muskel: nedlasting tabell - kummulative andeler. variabel -",
            input$var
          )
          shinyjs::onclick(
            "lastNedBilde",
            rapbase::repLogger(
              session = ss,
              msg = mldNLF
            )
          )
          shinyjs::onclick(
            "lastNedTabell",
            rapbase::repLogger(
              session = ss,
              msg = mldNLT
            )
          )
        }
      })
    }
  )

}
