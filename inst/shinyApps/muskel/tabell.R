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

tabellUI <- function(
    id, datoStart = "2008-01-01",
    datoSlutt = Sys.Date(),forltype = forlop){

    ns <- shiny::NS(id)
    shiny::sidebarLayout(
        shiny::sidebarPanel(
            shiny::dateRangeInput(
                ns("dato"), "Tidsperiode:",
                language = "no",separator = "til",
                start = datoStart, end = datoSlutt,
                format = "yyyy-mm-dd"
            ),
            shiny::sliderInput(
                ns("ald"), label = "Alder",
                min = 0, max = 120, value = c(0,120)
            ),
            shiny::selectInput(
                ns("kjo"), "Kjønn",
                choices = c(
                    "Begge" = 99, "Kvinne" = 0, "Mann" = 1),
                selected = 99
            ),
            shiny::selectInput(
                ns("tidenh"), "Velg tidsenhet",
                choices = c("Måned" = "maaned", "År" = "aar")
            ),
            shiny::selectInput(
                ns("forl"), label = "Forløpstype",
                choices = forltype, multiple = TRUE
            ),
            shiny::selectInput(
                ns("avdod"), label = "Inkluder avdøde",
                choices = c("Ja" , "Nei" ), selected = "Nei"
            ),
            shiny::radioButtons(
                ns("skjemarad"), "",
                choices = c("Forløp"= "ForlopsID","Pasient"= "PasientID"),
                inline = TRUE
            )
        ),
        shiny::mainPanel(
            shiny::h3(textOutput(ns("txt1")),
                      style = "text-align:center"),
            shiny::uiOutput(ns("tidsIntervall")),
            #hr(),
            DT::DTOutput(ns("Tabell")),
            shiny::downloadButton(ns("lastNedTabell"), "Last ned tabell")
        )
    )
}



tabell <- function(input, output, session, ss){

    forloptxt <- reactive({if(input$skjemarad=="ForlopsID") {
        "registrerte pasientforløp"
    }else{ "unike pasienter"}  })

    tidenhtxt <- reactive({if(input$tidenh == "maaned") {
        "måned"
    }else{ "år"}  })


    output$txt1 <- renderText({ paste0("Antall " ,forloptxt()," per ",tidenhtxt(), " per avdeling") })

    output$tidsIntervall <- renderUI({
        ns <- session$ns
        ic <- icon("calendar-alt")
        st <- "color : grey ; background-color:white "
        if ( req(input$tidenh) == "maaned") {
            tagList(
                shiny::fluidRow(
                    column(3,offset = 9,
                        shiny::actionButton(ns("tre"), "3 mnd",
                                            ic, style = st, width = "30%"),
                        shiny::actionButton(ns("seks"), "6 mnd",
                                            ic, style = st,width = "30%"),
                        shiny::actionButton(ns("et"), "1 år", ic,
                                            style =st,width = "30%")
                    )
                )
            )
        }
    })

    #
    observeEvent(input$tre,{
        valgtDato <- as.Date(max(input$dato)) -
            lubridate::day(as.Date(max(input$dato))) + 1

        shiny::updateDateRangeInput(
            session,
            inputId = "dato",
            start = valgtDato %m-% months(3),
        )
    })
    observeEvent(input$seks,{
        valgtDato <- as.Date(max(input$dato)) -
            lubridate::day(as.Date(max(input$dato))) + 1

        shiny::updateDateRangeInput(
            session,
            inputId = "dato",
            start = valgtDato %m-% months(6),
        )
    })
    observeEvent(input$et,{
        valgtDato <- as.Date(max(input$dato)) -
            lubridate::day(as.Date(max(input$dato))) + 1

        shiny::updateDateRangeInput(
            session,
            inputId = "dato",
            start = valgtDato %m-% months(12),
        )
    })



    ##table data
    tabellData <- reactive({ as.data.frame.matrix(
        muskel::MuskelTabellerForlopspas(
            RegData,
            tidFra = req(input$dato[1]),
            tidTil = req(input$dato[2]),
            aldmin = req(input$ald[1]),
            aldmax = req(input$ald[2]),
            kjoen = req(input$kjo),
            tidenh = req(input$tidenh),
            avd = req(input$avdod),
            IDType = req(input$skjemarad),
            frlType = (input$forl))
        )
    })

    #render table
    observe({
        cont <- headerFooter(tabellData())
        subS <- dim(tabellData())[1]-1
        raplog::repLogger(
            session = ss,
            msg = "Muskel: tabell unikepasienter/pasientforløp"
        )

        output$Tabell <-  renderDT(
            as.data.frame.matrix(tabellData())[1:subS, ] %>%
                DT::datatable(
                    container = cont,
                    #extensions = 'FixedHeader',
                    selection = "none",
                    options = list(
                        pageLength = 50,
                        fixedHeader = TRUE,
                        lengthChange = FALSE,
                        dom = "t"))
        )
    })

    output$lastNedTabell <- downloadHandler(
        filename = function() {
            if (req(input$skjemarad) == "PasientID") {
                paste0("pasienttabell",
                       req(input$tidenh),
                       Sys.time(), ".csv")
            } else {
                paste0("forlopstabell",
                       req(input$tidenh),
                       Sys.time(), ".csv")
            }
        },
        content = function(file) {
            tab <- tabellData()
            write.csv2(tab, file, row.names = T)
        }
    )
    observe({
        shinyjs::onclick(
            "lastNedTabell",
            raplog::repLogger(
                session = ss,
                msg = "Muskel: nedlasting tabell unikepasienter/pasientforløp"
            )
        )
    })

}
