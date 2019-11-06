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


    tabellData <- reactive({ as.data.frame.matrix(
        muskel::MuskelTabellerForlopspas(RegData,
                                         tidFra = req(input$dato[1]),
                                         tidTil = req(input$dato[2]),
                                         aldmin = req(input$ald[1]),
                                         aldmax = req(input$ald[2]),
                                         kjoen = req(input$kjo),
                                         tidenh = req(input$tidenh),
                                         avd = req(input$avdod),
                                         IDType = req(input$skjemarad),
                                         frlType = req(input$forl) )
    ) })

    observe({
        cont <- headerFooter(tabellData())
        subS <- dim(tabellData())[1]-1

        output$Tabell <-  renderDT(
            as.data.frame.matrix(tabellData())[1:subS, ] , container = cont, extensions = 'FixedHeader',
            options = list(pageLength = 50,fixedHeader = TRUE, lengthChange = FALSE) )
    })

}
