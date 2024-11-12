dataDumpUI <- function(id) {
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      id=ns("sidebarDataDump"),
      shiny::selectInput(
        ns("ddselect"),
        "Valg datadump:",
        choices =
          list("AlleVarNum","ForlopsOversikt","SkjemaOversikt",
               "SMAoversikt", "TilSykehusinnkjøp"),
        selected = "AlleVarNum"
      ),
      shiny::dateRangeInput(
        ns("ddDateRange"),
        "Tidsperiode",
        start = "2008-01-01" ,
        end = Sys.Date() ,
        min = "2008-01-01" ,
        max = Sys.Date(),
        separator = "til",
        language = "no"
      ),
      shiny::fluidRow(
        column(4, offset = 6,
               shiny::actionLink(class ="actnLink aarlink5",
                                 ns("siste5"),
                                 label = "5-år "
               ),
               shiny::actionLink(class ="actnLink aarlink1",
                                 ns("sistear"),
                                 label = "1-år "
               )),
        column(2,
               shiny::actionLink(class ="actnLink",
                                 ns("nullstill"),
                                 label = "Nullstill"
               ))),
      fluidRow(
        column(6, offset = 6,
               shiny::downloadButton(
                 style="float:right;margin-top:5px",
                 ns("dataDumpNedLasting"),
                 label = "Last ned!",
               ))
      )),
    shiny::mainPanel(
      fluidRow(
        column(
          width = 10,
          shiny::tags$h3('Datadump - Muskel', align='center'),
          shiny::tags$hr(),
          shiny::tags$p(
            'Her kan du laste ned forskjellige varianter av datadump for
          muskel. Lokale brukere vil bare kunne laste ned data
          for egen avdeling.'
          ),
          shiny::tags$h5(
            shiny::tags$b(shiny::tags$u(
              'Forklaring til de ulike datadump-typene:'
            ))),
          shiny::tags$div(
            class = "container",
            shiny::tags$h5(
              shiny::tags$b('AlleVarNum '),
              'inneholder alle kliniske variabler i registeret og benytter
            tallkodene til kategoriske variabler.'
            ),
            shiny::tags$h5(
              shiny::tags$b('ForlopsOversikt '),
              'inneholder en del administrative data relevant for forløpene.'),
            shiny::tags$h5(
              shiny::tags$b('SkjemaOversikt '), '
            er en oversikt over status til alle registreringer i registreret,
            også uferdige.')
          )
        ))
    )
  )
}

dataDump <- function(input, output, session, userRole, reshID, mainSession){

  #add HovedDato to allevar and allevarnum
  AddHovedDatoVariabels <-   reactive({
    switch (
      input$ddselect,
      "AlleVarNum" = ", ForlopsOversikt.HovedDato ",
      "ForlopsOversikt" = "",
      "SkjemaOversikt" = "",
      "SMAoversikt" = "",
      "TilSykehusinnkjøp" = ""
    )
  })
  AddHovedDatoJoin <-   reactive({
    switch (input$ddselect,
            "AlleVarNum" = "INNER JOIN ForlopsOversikt ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID ",
            "ForlopsOversikt" = "",
            "SkjemaOversikt" = "",
            "SMAoversikt" = "",
            "TilSykehusinnkjøp" = ""
    )
  })

  qry <-   reactive({
    if (input$ddselect %in% c("SMAoversikt", "TilSykehusinnkjøp")) {
      if (userRole ==  "SC") {"SELECT * FROM SMAoversikt"}
      else {paste0("SELECT * FROM SMAoversikt WHERE CENTREID = ", reshID)}
    } else {
      if (userRole ==  "SC") {
        paste0(
          "SELECT ", input$ddselect, ".* ", AddHovedDatoVariabels(),
          " FROM ", input$ddselect, " ", AddHovedDatoJoin())
      } else {
        paste0(
          "SELECT ",input$ddselect, ".* ", AddHovedDatoVariabels(),
          " FROM ", input$ddselect, " ", AddHovedDatoJoin(), " WHERE ",
          input$ddselect, ".AvdRESH = ", reshID
        )
      }
    }
  })

  observeEvent(input$sistear,{
    valgtDato <- as.Date(max(req(input$ddDateRange)))
    shiny::updateDateRangeInput(
      session,
      inputId = "ddDateRange",
      start = valgtDato %m-% months(12),
    )
  })

  observeEvent(input$siste5,{
    valgtDato <- as.Date(max(req(input$ddDateRange)))
    shiny::updateDateRangeInput(
      session,
      inputId = "ddDateRange",
      start = valgtDato %m-% years(5),
    )
  })
  observeEvent(input$nullstill, {shinyjs::reset("sidebarDataDump")})

  output$dataDumpNedLasting <- shiny::downloadHandler(
    filename = function(){
      paste0(input$ddselect,"Muskel",Sys.Date(),".csv")
    },
    content = function(file){
      dataDump <- rapbase::loadRegData(
        registryName = "muskel",
        query = qry(),
        dbType = "mysql"
      )
      if (input$ddselect %in% c("SMAoversikt", "TilSykehusinnkjøp")) {
        dataDump <- dataDump %>%
          dplyr::filter(
            as.Date(ASSESSMENT_DATE) %>%
              dplyr::between(
                min(input$ddDateRange),
                max(input$ddDateRange)
              )
          )
        if (input$ddselect == "TilSykehusinnkjøp") {
          dataDump <- dataDump %>%
            dplyr::mutate(ASSESSMENT_DATE = as.Date(ASSESSMENT_DATE)) %>%
            dplyr::arrange(ASSESSMENT_DATE) %>%
            dplyr::filter(STATUS == 1,
                          BEHANDLNG_SPINRAZA ==1) %>%
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
            dplyr::select(-PATIENT_ID) %>%
            dplyr::filter(Tidsdiff_dager != 0)
        }
      } else {
        dataDump <- dataDump %>%
          dplyr::filter(
            HovedDato %>%
              dplyr::between(
                min(input$ddDateRange),
                max(input$ddDateRange)
              )
          )
      }
      write.csv2(dataDump, file, fileEncoding = "UTF-8")
    }
  )
  shinyjs::onclick(
    "dataDumpNedLasting",
    rapbase::repLogger(
      session = mainSession,
      msg = paste0(
        "Muskel: datadump ", input$ddselect, " ",
        min(input$ddDateRange),"-",max(input$ddDateRange)
      )
    )
  )
}
