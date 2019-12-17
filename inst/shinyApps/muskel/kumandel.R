
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

        shiny::sidebarPanel(div(id = ns("snPanel"),
                shiny::selectInput(ns("var"), label = "Velg variabel",
                               choices = vlgtvar, selected = vlgtvar[[1]] ),

                shiny::dateRangeInput(ns("dato"), label = "Tidsperiode",
                                  start = datoStart, end = datoSlutt,
                                  language = "no",separator = "til",
                                  format = "yyyy-mm-dd"),

                dateRangeInput2(ns("utrar"), label = "Utredningsår",
                                start = utrstart, end = utrslutt,
                                language = "no",separator = "til",
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
                            choices = c('pdf', 'png', 'jpg', 'bmp', 'tif', 'svg'))),
                shiny::actionLink(inputId=ns("nullstill"),
                                  style="color:black" ,
                                  label = "Nullstill Valg")


        ),#sidebarPanel

        shiny::mainPanel(
            shiny::tabsetPanel(id = ns("tab"),
              shiny::tabPanel("Figur", value = "fig",
                       shiny::plotOutput(ns("Figur"), height="auto", hover = ns("hov")),
                       shiny::downloadButton(ns("lastNedBilde"), "Last ned bilde")),
              shiny::tabPanel("Tabell", value = "tab",
                       shiny::tableOutput(ns("Tabell")),
                       shiny::downloadButton(ns("lastNedTabell"), "Last ned tabell"))
            )#tabsetPanel
        )#mainPanel
    )#sidebarlayout
}

#shiny server mudule
kumulativAndel <- function(input, output, session, rID, ss){

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
  observeEvent(req(input$nullstill), {shinyjs::reset("snPanel")})
  resp <- reactive({
    tabData <- muskel::MuskelFigCumAndel( RegData = RegData, valgtVar = input$var, datoFra = min(input$dato),
                                          datoTil =   max(input$dato), debutAlderFra = input$ald[1], debutAlderTil = input$ald[2] ,
                                          UtredningsaarFra = lubridate::year(min(input$utrar)),
                                          UtredningsaarTil = lubridate::year(max(input$utrar)) ,
                                          diagnosegr = convNull(input$diaggrupper), diagnose = convNull(input$ICD),
                                          undergr = convNull(input$Undrgr), undergr2 = convNull(input$Undrgr2),
                                          egenavd = as.numeric(input$psgr), enhetsUtvalg = as.numeric(input$enh) ,
                                          avdod = input$avdod ,reshID = rID, outfile = "" )
  })

  #plot figure
  observe({
  output$Figur <- shiny::renderPlot({
    muskel::MuskelFigCumAndel( RegData = RegData, valgtVar = input$var, datoFra = min(input$dato),
                               datoTil =   max(input$dato), debutAlderFra = input$ald[1], debutAlderTil = input$ald[2] ,
                               UtredningsaarFra = lubridate::year(min(input$utrar)),
                               UtredningsaarTil = lubridate::year(max(input$utrar)) ,
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
      muskel::MuskelFigCumAndel( RegData = RegData, valgtVar = input$var, datoFra = min(req(input$dato)),
                                 datoTil =   max(req(input$dato)), minald = input$ald[1], maxald = input$ald[2] ,
                                 UtredningsaarFra = lubridate::year(min(req(input$utrar))),
                                 UtredningsaarTil = lubridate::year(max(req(input$utrar))) ,
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
  shiny::observe({
    if (onServer) {
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
      raplog::repLogger(
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
        raplog::repLogger(
          session = ss,
          msg = mldNLF
        )
      )
      shinyjs::onclick(
        "lastNedTabell",
        raplog::repLogger(
          session = ss,
          msg = mldNLT
        )
      )
    }
  })

}#kumulativeAndel

