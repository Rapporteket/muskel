#' ui-modul for grupperte fordelingsfigurer i NORNMD sin shiny-app på
#' Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul fordelingsfigur
#'
#' @export
fordeling_grvar_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::sidebarLayout(

    shiny::sidebarPanel(
      div(
        id=ns("sbPanel"),

        shiny::uiOutput(ns("var_ui")),

        shiny::sliderInput(ns("ald"), label = "Alder",
                           min = 0, max = 120, value = c(0,120) ),

        shiny::dateRangeInput(ns("dato"), label = "Tidsperiode",
                              start = "2008-01-01", end = Sys.Date(),
                              language = "no",separator = "til",
                              format = "yyyy-mm-dd"),

        shiny::selectInput(
          ns("kjo"), label = "Kjønn",
          choices = c("Begge kjønn" = 99 , "Kvinne" = 0, "Mann" = 1),
          selected = 99),

        shiny::selectInput(ns("avdod"), label = "Inkluder avdøde",
                           choices = c("Ja" , "Nei" ),
                           selected = "Nei"),

        selectInput(inputId = ns("outfile"), label = "Velg bildeformat",
                    choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))),
      shiny::actionLink(inputId=ns("nullstill"),
                        style="color:black" ,
                        label = "Nullstill Valg")
    ),#sidebarPanel

    shiny::mainPanel(
      tabsetPanel(
        id = ns("tab"),
        shiny::tabPanel(
          "Figur",value = "fig",
          shiny::plotOutput(ns("Figur"), height="auto"),
          shiny::downloadButton(ns("lastNedBilde"), "Last ned bilde")),

        shiny::tabPanel(
          "Tabell", value = "tab",
          shiny::tableOutput(ns("Tabell")),
          shiny::downloadButton(ns("lastNedTabell"), "Last ned tabell"))
      )
    )
  )
}

#' server-modul for grupperte fordelingsfigurer i NORNMD sin shiny-app på
#' Rapporteket
#'
#' Kun til bruk i Shiny
#'
#' @return Modul fordelingsfigur
#'
#' @export
fordeling_grvar_server <- function(id, RegData, reshID, ss){
  moduleServer(
    id,
    function(input, output, session) {

      output$var_ui <- renderUI({
        ns <- session$ns
        shiny::selectInput(ns("var"), label = "Velg variabel",
                           choices = muskel::varValgGrVar,
                           selected = muskel::varValgGrVar[[1]] )
      })

      resp <- reactive({
        tabData <- muskel::MuskelFigAndelStabel(
          RegData = RegData, valgtVar = req(input$var), datoFra = input$dato[1],
          datoTil =   input$dato[2], minald = input$ald[1], maxald = input$ald[2] ,
          erMann = as.numeric(input$kjo), avdod = input$avdod ,reshID = reshID,
          outfile = "" )
      })

      output$Figur <- renderPlot({
        muskel::MuskelFigAndelStabel(
          RegData = RegData, valgtVar = req(input$var),
          datoFra = min(req(input$dato)),
          datoTil =   max(req(input$dato)),
          minald = input$ald[1], maxald = input$ald[2] ,
          erMann = as.numeric(input$kjo), avdod = input$avdod,
          reshID = reshID, outfile = "" )
      },
      width = 700, height = 700)

      output$lastNedBilde <- downloadHandler(
        filename = function(){
          paste0(input$var, Sys.time(), '.', input$outfile)
        },
        content = function(file){
          muskel::MuskelFigAndelStabel(
            RegData = RegData, valgtVar = input$var, datoFra = min(input$dato),
            datoTil =   max(input$dato), minald = input$ald[1],
            maxald = input$ald[2],
            erMann = as.numeric(input$kjo), avdod = input$avdod,
            reshID = reshID, outfile = file )
        }
      )
      observeEvent(req(input$nullstill), {shinyjs::reset("sbPanel")})

      output$Tabell <- function() {
        TabellData <- resp()
        Tabell <- as.data.frame( TabellData$Andeler )
        names(Tabell) <- c("Kategori", "Grupper","Andel")
        antallKat <- length(Tabell[["Kategori"]])
        Tabell <- Tabell %>%
          tidyr::spread(key = Kategori, value = Andel )

        Tabell %>%
          knitr::kable("html", digits = c(0, rep(1, antallKat-1 ) )) %>%
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
          Tabell <- Tabell %>%
            tidyr::spread(key = Kategori, value = Andel )
          write.csv2(Tabell, file, row.names = F)
        }
      )

      shiny::observe({
        if (rapbase::isRapContext()) {
          if (req(input$tab) == "fig") {
            mldandel <- paste(
              "Muskel: figur - fordeling etter grupperingsvariabler. variabel -",
              input$var
            )
          } else if (req(input$tab) == "tab") {
            mldandel <- paste(
              "Muskel: tabell - fordeling etter grupperingsvariabler. variabel -",
              input$var
            )
          }
          rapbase::repLogger(
            session = ss,
            msg = mldandel
          )
          mldNLF <- paste(
            "Muskel: nedlasting figur - fordeling etter grupperingsvariabler. variabel -",
            input$var
          )
          mldNLT <- paste(
            "Muskel: nedlasting tabell - fordeling etter grupperingsvariabler. variabel -",
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
