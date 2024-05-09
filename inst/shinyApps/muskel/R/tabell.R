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

tabellUI <- function(id){

  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      id = ns("sbpanel"),
      shiny::uiOutput(ns("sidebar"))
    ),
    shiny::mainPanel(
      width = 9,
      tabsetPanel(id = ns("tab"),
                  tabPanel("Antall skjema", value = "antskjema",
                           DTOutput(ns("Tabell_adm1")),
                           downloadButton(ns("lastNedAdm1"), "Last ned tabell")),
                  tabPanel("Antall unike Pasienter/pasientforløp", value = "pasforl",
                           shiny::h3(textOutput(ns("txt1")),
                                     style = "text-align:center"),
                           shiny::uiOutput(ns("tidsIntervall")),
                           DT::DTOutput(ns("Tabell")),
                           shiny::downloadButton(ns("lastNedTabell"), "Last ned tabell")
                  ),
                  # tabPanel("Tabell til sykehusinnkjøp", value = "sykehusinnkjop")
                  # shiny::uiOutput(ns("sykehusinnkjop_ui"))
                  tabPanel("Tabell til sykehusinnkjøp", value = "sykehusinnkjop",
                           shiny::tableOutput(ns("TabellSykehusinnkjop")),
                           shiny::downloadButton(ns("lastNedSykehusinnkjop"), "Last ned tabell")
                  )
      )
    ))

}



tabell <- function(input, output, session, ss, forltype = forlop, userRole){

  if (userRole != "SC") {
    shiny::hideTab("tab", target = "sykehusinnkjop")
  }


  output$sidebar <- renderUI({
    ns <- session$ns
    if (input$tab == "pasforl") {
      tagList(div(id = ns("sbPas"),
                  shiny::dateRangeInput(
                    ns("dato"), "Tidsperiode:",
                    language = "no",separator = "til",
                    start = "2008-01-01", end = Sys.Date(),
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
                    choices = c("Måned" = "maaned", "År" = "aar"),
                    selected = "aar"
                  ),
                  shiny::selectInput(
                    ns("forl"), label = "Forløpstype",
                    choices = forltype, multiple = TRUE,
                    selected = forltype[1]
                  ),
                  shiny::selectInput(
                    ns("avdod"), label = "Inkluder avdøde",
                    choices = c("Ja" , "Nei" ), selected = "Nei"
                  ),
                  shiny::radioButtons(
                    ns("skjemarad"), "",
                    choices = c("Forløp"= "ForlopsID","Pasient"= "PasientID"),
                    inline = TRUE
                  )),
              shiny::actionLink(inputId = ns("nullstillPas"),
                                style="color:black",
                                label = "Nullstill Valg")

      )
    } else if (input$tab %in% c("antskjema", "sykehusinnkjop")) {
      tagList(div(id = ns("sbSkj"),
                  dateInput(inputId = ns('datoFra2'), value = '2008-01-01', min = '2008-01-01',
                            label = "F.o.m. dato", language="nb"),
                  dateInput(inputId = ns('datoTil2'), value = Sys.Date(), min = '2012-01-01',
                            label = "T.o.m. dato", language="nb"),
                  selectInput(inputId = ns("regstatus"), label = "Skjemastatus",
                              choices = c('Ferdigstilt'=1, 'Kladd'=0))),
              shiny::actionLink(inputId=ns("nullstillSkj"),
                                style="color:black" ,
                                label = "Nullstill Valg")

      )
    }
  })
  observeEvent(req(input$nullstillSkj), {shinyjs::reset("sbSkj")})
  observeEvent(req(input$nullstillPas), {shinyjs::reset("sbPas")})

  antskjema <- function() {
    aux <- as.data.frame.matrix(addmargins(table(SkjemaOversikt[SkjemaOversikt$SkjemaStatus == as.numeric(req(input$regstatus)) &
                                                                  SkjemaOversikt$HovedDato >= req(input$datoFra2) &
                                                                  SkjemaOversikt$HovedDato <= req(input$datoTil2),
                                                                c("Sykehusnavn", "Skjemanavn")], useNA = 'ifany')))
    aux$Avdeling <- row.names(aux)
    ant_skjema <- aux[, c(dim(aux)[2], 1:(dim(aux)[2]-1))]
    sketch <- htmltools::withTags(table(
      tableHeader(ant_skjema[-dim(ant_skjema)[1], ]),
      tableFooter(c('Sum' , as.numeric(ant_skjema[dim(ant_skjema)[1], 2:dim(ant_skjema)[2]])))))
    list(ant_skjema=ant_skjema, sketch=sketch)
  }

  output$Tabell_adm1 = DT::renderDT(
    DT::datatable(
      antskjema()$ant_skjema[-dim(antskjema()$ant_skjema)[1], ],
      container = antskjema()$sketch,
      rownames = F,
      selection = "none",
      options = list(
        pageLength = 50,
        fixedHeader = TRUE,
        lengthChange = FALSE,
        dom = "t")
    )
  )

  output$lastNedAdm1 <- shiny::downloadHandler(
    filename = paste0(
      "Skjematabel", Sys.Date(),".csv"
    ),
    content = function (file) {write.csv2(antskjema()$ant_skjema, file, row.names = F)}
  )

  tabell_shusinnkjop <- function() {
    tabell_sma <- SMAoversikt %>%
      dplyr::mutate(ASSESSMENT_DATE = as.Date(ASSESSMENT_DATE)) %>%
      dplyr::arrange(ASSESSMENT_DATE) %>%
      dplyr::summarise(
        ASSESSMENT_DATE_baseline = first(ASSESSMENT_DATE),
        HFMSE_baseline = first(KLINISK_HFMSE, order_by = ASSESSMENT_DATE),
        RULM_baseline = first(KLINISK_RULM, order_by = ASSESSMENT_DATE),
        x6MWT_baseline = first(KLINISK_6MWT, order_by = ASSESSMENT_DATE),
        ATEND_baseline = first(KLINISK_ATEND, order_by = ASSESSMENT_DATE),
        BIPAP_baseline = first(KLINISK_BIPAP, order_by = ASSESSMENT_DATE),
        FUNKSJONSSTATUS_baseline = first(KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE),
        ASSESSMENT_DATE_latest = last(ASSESSMENT_DATE),
        HFMSE_latest = ifelse(last(ASSESSMENT_DATE)==first(ASSESSMENT_DATE),
                              NA, last(KLINISK_HFMSE, order_by = ASSESSMENT_DATE)),
        RULM_latest = ifelse(last(ASSESSMENT_DATE)==first(ASSESSMENT_DATE),
                             NA, last(KLINISK_RULM, order_by = ASSESSMENT_DATE)),
        x6MWT_latest = ifelse(last(ASSESSMENT_DATE)==first(ASSESSMENT_DATE),
                              NA, last(KLINISK_6MWT, order_by = ASSESSMENT_DATE)),
        ATEND_latest = ifelse(last(ASSESSMENT_DATE)==first(ASSESSMENT_DATE),
                              NA, last(KLINISK_ATEND, order_by = ASSESSMENT_DATE)),
        BIPAP_latest = ifelse(last(ASSESSMENT_DATE)==first(ASSESSMENT_DATE),
                              NA, last(KLINISK_BIPAP, order_by = ASSESSMENT_DATE)),
        FUNKSJONSSTATUS_latest = ifelse(last(ASSESSMENT_DATE)==first(ASSESSMENT_DATE),
                                        NA, last(KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE)),
        Tidsdiff_dager = difftime(ASSESSMENT_DATE_latest, ASSESSMENT_DATE_baseline, units = "days"),
        .by = PATIENT_ID) %>%
      dplyr::filter(Tidsdiff_dager != 0)
  }

  output$TabellSykehusinnkjop <- function() {
    tabell_sma <- tabell_shusinnkjop()
    names(tabell_sma) <- c("PATIENT_ID", "ASSESSMENT_DATE", "HFMSE", "RULM", "6MWT", "ATEND", "BIPAP", "FUNKSJONSSTATUS",
                           "ASSESSMENT_DATE ", "HFMSE ", "RULM ", "6MWT ", "ATEND ", "BIPAP ", "FUNKSJONSSTATUS ",
                           "Tidsdiff_dager")
    tabell_sma %>% knitr::kable("html", row.names = F) %>%
      kableExtra::kable_styling("hover", full_width = F) %>%
      kableExtra::add_header_above(c(" ", "Baseline" = 7, "Siste måling" = 7, " "))
  }

  output$lastNedSykehusinnkjop <- shiny::downloadHandler(
    filename = paste0(
      "sykehusinnkjop", Sys.Date(),".csv"
    ),
    content = function (file) {write.csv2(tabell_shusinnkjop(), file,
                                          row.names = F, fileEncoding = "Latin1")}
  )


  observe({
    if ( input$tab == "pasforl" ) {
      forloptxt <- reactive({if(req(input$skjemarad)=="ForlopsID") {
        "registrerte pasientforløp"
      }else{ "unike pasienter"}  })

      tidenhtxt <- reactive({if(req(input$tidenh) == "maaned") {
        "måned"
      }else{ "år"}  })


      output$txt1 <- renderText({ paste0("Antall " ,forloptxt()," per ",tidenhtxt(), " per avdeling") })
    }
  })

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
    valgtDato <- as.Date(max(req(input$dato))) -
      lubridate::day(as.Date(max(req(input$dato)))) + 1

    shiny::updateDateRangeInput(
      session,
      inputId = "dato",
      start = valgtDato %m-% months(3),
    )
  })
  observeEvent(input$seks,{
    valgtDato <- as.Date(max(req(input$dato))) -
      lubridate::day(as.Date(max(req(input$dato)))) + 1

    shiny::updateDateRangeInput(
      session,
      inputId = "dato",
      start = valgtDato %m-% months(6),
    )
  })
  observeEvent(input$et,{
    valgtDato <- as.Date(max(req(input$dato))) -
      lubridate::day(as.Date(max(req(input$dato)))) + 1

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
      tidFra = min(req(input$dato)),
      tidTil = max(req(input$dato)),
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
    rapbase::repLogger(
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
      rapbase::repLogger(
        session = ss,
        msg = "Muskel: nedlasting tabell unikepasienter/pasientforløp"
      )
    )
    shinyjs::onclick(
      "lastNedAdm1",
      rapbase::repLogger(
        session = ss,
        msg = "Muskel: Nedlasting tabell admin-skjema"
      )
    )
    if ( input$tab == "antskjema"){
      rapbase::repLogger(
        session = ss,
        msg = "Muskel: tabell - admin-skjema"
      )
    }
  })

}
