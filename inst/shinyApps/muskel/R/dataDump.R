dataDumpUI <- function(id) {
  ns <- NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(id=ns("sidebarDataDump"),
      shiny::selectInput(
        ns("ddselect"),
        "Valg datadump:",
        choices =
          list("AlleVarNum","AlleVar","ForlopsOversikt","SkjemaOversikt"),
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
      fluidRow( column(width = 10,
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
        shiny::tags$div(class = "container",
          shiny::tags$h5(
            shiny::tags$b('AlleVar '),
            'inneholder alle kliniske variabler i registeret og benytter
            etikettene til kategoriske variabler.'
          ),
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
    switch (input$ddselect,
      "AlleVarNum" = ", ForlopsOversikt.ForlopsID, ForlopsOversikt.HovedDato ",
      "AlleVar" = ", ForlopsOversikt.ForlopsID, ForlopsOversikt.HovedDato ",
      "ForlopsOversikt" = "",
      "SkjemaOversikt" = ""
    )
  })
  AddHovedDatoJoin <-   reactive({
    switch (input$ddselect,
            "AlleVarNum" = "INNER JOIN ForlopsOversikt
            ON AlleVarNum.ForlopsID = ForlopsOversikt.ForlopsID ",
            "AlleVar" = "INNER JOIN ForlopsOversikt
            ON AlleVar.ForlopsID = ForlopsOversikt.ForlopsID ",
            "ForlopsOversikt" = "",
            "SkjemaOversikt" = ""
    )
  })

  qry <-   reactive({
    if (userRole ==  "SC") {
       paste0(
         "SELECT ", input$ddselect, ".* ", AddHovedDatoVariabels(),
         " FROM ", input$ddselect, " ", AddHovedDatoJoin())
    } else {
       paste0(
         "SELECT ",input$ddselect, ".* ", AddHovedDatoVariabels(),
         " FROM ", input$ddselect, " ", AddHovedDatoJoin(), " WHERE ", input$ddselect, ".AvdRESH = ", reshID))
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
      dataDump <- rapbase::LoadRegData(
        registryName = "muskel",
        query = qry(),
        dbType = "mysql"
      )
      dataDump <- dataDump %>%
        dplyr::filter(
        HovedDato %>%
          dplyr::between(
            min(input$ddDateRange),
            max(input$ddDateRange)
        )
      )
      write_csv2(dataDump, file)
    }
  )
  shinyjs::onclick(
    "dataDumpNedLasting",
    raplog::repLogger(
      session = mainSession,
      msg = paste0(
        "Muskel: datadump ", input$ddselect, " ",
        min(input$ddDateRange),"-",max(input$ddDateRange)
      )
    )
  )
}
