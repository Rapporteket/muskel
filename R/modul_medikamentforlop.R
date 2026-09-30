#' ui-modul for medikamenttabeller i Muskelregisteret
#'
#' @export
#'
medikament_sma_ui <- function(id){

  ns <- shiny::NS(id)
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      width = 3,
      id = ns("sbpanel"),
      dateInput(inputId = ns('datoFra'),
                value = '2020-01-01', min = '2020-01-01',
                label = "F.o.m. dato", language="nb"),
      dateInput(inputId = ns('datoTil'),
                value = Sys.Date(), min = '2021-01-01',
                label = "T.o.m. dato", language="nb"),
      selectInput(inputId = ns("regstatus"),
                  label = "Skjemastatus",
                  choices = c('Ferdigstilt'=1, 'Kladd'=0,
                              'Opprettet' = -1),
                  multiple = TRUE, selected = 1),
      selectInput(inputId = ns("type_medikamentforlop"),
                  label = "Type medikamentforløp",
                  choices = c('Type 1'=1, 'Type 2'=2),
                  multiple = TRUE, selected = 1),
      shiny::sliderInput(
        inputId = ns("antall_oppf"),
        label = "Antall oppfølginger i forløp",
        min = 1,
        max = 20,
        value = c(2, 15)
      )
    ),
    shiny::mainPanel(
      width = 9,
      tabsetPanel(
        id = ns("smatab"),

        tabPanel(
          "Medikament per pasient",
          # value = "pasient_tab",
          # shiny::tableOutput(ns("pasient_tab")),
          DT::DTOutput(ns("pasient_tab")),
          shiny::downloadButton(ns("last_ned_pasient_tab"),
                                "Last ned tabell")
        ),

        tabPanel(
          "Medikament per forløp",
          # value = "forlop_tab",
          # shiny::tableOutput(ns("forlop_tab")),
          DT::DTOutput(ns("forlop_tab")),
          shiny::downloadButton(ns("last_ned_forlop_tab"),
                                "Last ned tabell")
        )
      )
    )
  )
}


#' server-modul for medikamenttabeller i Muskelregisteret
#'
#' @export
#'
medikament_sma_server <- function(
    id, SMAoversikt, user){
  moduleServer(
    id,
    function(input, output, session) {

      tabell_shusinnkjop <- function() {
        tabell_sma <- SMAoversikt %>%
          dplyr::mutate(ASSESSMENT_DATE = as.Date(ASSESSMENT_DATE)) %>%
          dplyr::filter(
            ASSESSMENT_DATE >= req(input$datoFra),
            ASSESSMENT_DATE <= req(input$datoTil),
            STATUS %in% as.numeric(req(input$regstatus)),
            user$role() == "SC" | CENTREID == user$org(),
            BEHANDLNG_SPINRAZA == 1) %>%
          dplyr::arrange(ASSESSMENT_DATE) %>%
          dplyr::summarise(
            CENTREID = paste0(unique(CENTREID), collapse = ","),
            ASSESSMENT_DATE_baseline = dplyr::first(ASSESSMENT_DATE),
            HFMSE_baseline = dplyr::first(
              KLINISK_HFMSE, order_by = ASSESSMENT_DATE),
            RULM_baseline = dplyr::first(
              KLINISK_RULM, order_by = ASSESSMENT_DATE),
            x6MWT_baseline = dplyr::first(
              KLINISK_6MWT, order_by = ASSESSMENT_DATE),
            ATEND_baseline = dplyr::first(
              KLINISK_ATEND, order_by = ASSESSMENT_DATE),
            BIPAP_baseline = dplyr::first(
              KLINISK_BIPAP, order_by = ASSESSMENT_DATE),
            FUNKSJONSSTATUS_baseline = dplyr::first(
              KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE),
            ASSESSMENT_DATE_latest = dplyr::last(ASSESSMENT_DATE),
            HFMSE_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_HFMSE, order_by = ASSESSMENT_DATE)),
            RULM_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_RULM, order_by = ASSESSMENT_DATE)),
            x6MWT_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_6MWT, order_by = ASSESSMENT_DATE)),
            ATEND_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_ATEND, order_by = ASSESSMENT_DATE)),
            BIPAP_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_BIPAP, order_by = ASSESSMENT_DATE)),
            FUNKSJONSSTATUS_latest = ifelse(
              dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE)),
            Tidsdiff_dager = difftime(
              ASSESSMENT_DATE_latest, ASSESSMENT_DATE_baseline, units = "days"),
            FUNKSJONSSTATUS_all = paste0(BEHANDLNG_FUNKSJONSSTATUS, collapse = ","),
            BEHANDLING_all = paste0(BEHANDLNG_BEHANDLING, collapse = ","),
            N = dplyr::n(),
            .by = PATIENT_ID) |>
          dplyr::filter(N >= input$antall_oppf[1],
                        N <= input$antall_oppf[2])
      }


      output$pasient_tab <- DT::renderDT({

        tabell_sma <- tabell_shusinnkjop()

        names(tabell_sma) <- c(
          "PATIENT_ID", "CENTREID",
          "ASSESSMENT_DATE", "HFMSE",
          "RULM", "6MWT", "ATEND",
          "BIPAP", "KLINISK FUNKSJONSSTATUS",
          "ASSESSMENT_DATE", "HFMSE",
          "RULM", "6MWT", "ATEND",
          "BIPAP", "KLINISK FUNKSJONSSTATUS",
          "Tidsdiff_dager", "FUNKSJONSSTATUS ALLE",
          "BEHANDLING ALLE", "N"
        )

        DT::datatable(
          tabell_sma,
          rownames = FALSE,
          escape = FALSE,
          class = "compact stripe hover",
          options = list(
            scrollX = TRUE,
            pageLength = 25,
            autoWidth = TRUE
          ),

          container = htmltools::withTags(

            table(
              class = "display",

              thead(

                # Øverste header-rad
                tr(

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "PATIENT_ID"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "CENTREID"
                  ),

                  th(
                    colspan = 7,
                    style = paste(
                      "background:#eef5ff;",
                      "font-weight:bold;",
                      "text-align:center;",
                      "border-right:6px solid #999;",
                      "border-bottom:2px solid #666;"
                    ),
                    "Baseline"
                  ),

                  th(
                    colspan = 7,
                    style = paste(
                      "background:#fff8e8;",
                      "font-weight:bold;",
                      "text-align:center;",
                      "border-bottom:2px solid #666;"
                    ),
                    "Siste måling"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "Tidsdiff_dager"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "FUNKSJONSSTATUS ALLE"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "BEHANDLING ALLE"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "N"
                  )
                ),

                # Nederste header-rad
                tr(

                  th(style = "background:#eef5ff;", "ASSESSMENT_DATE"),
                  th(style = "background:#eef5ff;", "HFMSE"),
                  th(style = "background:#eef5ff;", "RULM"),
                  th(style = "background:#eef5ff;", "6MWT"),
                  th(style = "background:#eef5ff;", "ATEND"),
                  th(style = "background:#eef5ff;", "BIPAP"),

                  th(
                    style = paste(
                      "background:#eef5ff;",
                      "border-right:6px solid #999;"
                    ),
                    "KLINISK FUNKSJONSSTATUS"
                  ),

                  th(style = "background:#fff8e8;", "ASSESSMENT_DATE"),
                  th(style = "background:#fff8e8;", "HFMSE"),
                  th(style = "background:#fff8e8;", "RULM"),
                  th(style = "background:#fff8e8;", "6MWT"),
                  th(style = "background:#fff8e8;", "ATEND"),
                  th(style = "background:#fff8e8;", "BIPAP"),
                  th(style = "background:#fff8e8;", "KLINISK FUNKSJONSSTATUS")
                )
              )
            )
          )
        ) %>%

          # Fortsett separatoren ned gjennom dataene
          DT::formatStyle(
            columns = 9,
            borderRight = "6px solid #999"
          )

      })

      output$last_ned_pasient_tab <- downloadHandler(

        filename = function() {
          paste0(
            "sma_pasient_",
            format(Sys.Date(), "%Y%m%d"),
            ".xlsx"
          )
        },

        content = function(file) {

          tabell_sma <- tabell_shusinnkjop()

          names(tabell_sma) <- c(
            "PATIENT_ID", "CENTREID",
            "ASSESSMENT_DATE", "HFMSE",
            "RULM", "6MWT", "ATEND",
            "BIPAP", "KLINISK FUNKSJONSSTATUS",
            "ASSESSMENT_DATE", "HFMSE",
            "RULM", "6MWT", "ATEND",
            "BIPAP", "KLINISK FUNKSJONSSTATUS",
            "Tidsdiff_dager", "FUNKSJONSSTATUS ALLE",
            "BEHANDLING ALLE", "N"
          )

          wb <- openxlsx::createWorkbook()

          openxlsx::addWorksheet(
            wb,
            "Forløp"
          )

          # Gruppe-rad
          openxlsx::writeData(
            wb,
            sheet = 1,
            x = matrix(
              c(
                "",
                "",
                "Baseline",
                rep("", 6),
                "Siste måling",
                rep("", 6),
                "",
                "",
                "",
                ""
              ),
              nrow = 1
            ),
            startRow = 1,
            colNames = FALSE
          )

          # Merge grupper
          openxlsx::mergeCells(
            wb,
            1,
            cols = 3:9,
            rows = 1
          )

          openxlsx::mergeCells(
            wb,
            1,
            cols = 10:16,
            rows = 1
          )

          # Kolonnenavn
          openxlsx::writeData(
            wb,
            sheet = 1,
            x = as.data.frame(t(names(tabell_sma))),
            startRow = 2,
            colNames = FALSE
          )

          # Data
          openxlsx::writeData(
            wb,
            sheet = 1,
            x = tabell_sma,
            startRow = 3,
            colNames = FALSE
          )

          # Stiler
          group_style <- openxlsx::createStyle(
            textDecoration = "bold",
            halign = "center",
            fgFill = "#D9EAF7",
            border = "bottom"
          )

          last_style <- openxlsx::createStyle(
            textDecoration = "bold",
            halign = "center",
            fgFill = "#F8F0D8",
            border = "bottom"
          )

          header_style <- openxlsx::createStyle(
            textDecoration = "bold"
          )

          openxlsx::addStyle(
            wb, 1, group_style,
            rows = 1, cols = 3:9,
            gridExpand = TRUE
          )

          openxlsx::addStyle(
            wb, 1, last_style,
            rows = 1, cols = 10:16,
            gridExpand = TRUE
          )

          openxlsx::addStyle(
            wb, 1, header_style,
            rows = 2,
            cols = 1:ncol(tabell_sma),
            gridExpand = TRUE
          )

          openxlsx::freezePane(
            wb,
            sheet = 1,
            firstRow = TRUE,
            firstCol = TRUE
          )

          openxlsx::setColWidths(
            wb,
            1,
            cols = 1:ncol(tabell_sma),
            widths = "auto"
          )

          openxlsx::saveWorkbook(
            wb,
            file,
            overwrite = TRUE
          )
          rapbase::repLogger2(
            user = user,
            msg = paste0("Laster ned oversikt over SMA medikamentforløp
                             per pasient.")
          )
        }
      )

      tabell_shusinnkjop2 <- function() {
        tabell_sma <- SMAoversikt |>
          dplyr::arrange(PATIENT_ID, ASSESSMENT_DATE) |>
          dplyr::filter(STATUS %in% as.numeric(req(input$regstatus))) |>
          dplyr::mutate(
            forlop_id = cumsum(
              dplyr::row_number() == 1 |
                BEHANDLNG_BEHANDLING != dplyr::lag(BEHANDLNG_BEHANDLING)
            ),
            .by = PATIENT_ID
          ) |>
          dplyr::relocate(PATIENT_ID, forlop_id) |>
          dplyr::mutate(ASSESSMENT_DATE = as.Date(ASSESSMENT_DATE)) |>
          dplyr::filter(
            ASSESSMENT_DATE >= req(input$datoFra),
            ASSESSMENT_DATE <= req(input$datoTil),
            user$role() == "SC" | CENTREID == user$org(),
            BEHANDLNG_SPINRAZA == 1) |>
          dplyr::summarise(
            Medikamentforlop_SMA = dplyr::first(BEHANDLNG_BEHANDLING),
            CENTREID = paste0(unique(CENTREID), collapse = ","),
            ASSESSMENT_DATE_baseline = dplyr::first(
              ASSESSMENT_DATE, order_by = ASSESSMENT_DATE),
            HFMSE_baseline = dplyr::first(
              KLINISK_HFMSE, order_by = ASSESSMENT_DATE),
            RULM_baseline = dplyr::first(
              KLINISK_RULM, order_by = ASSESSMENT_DATE),
            x6MWT_baseline = dplyr::first(
              KLINISK_6MWT, order_by = ASSESSMENT_DATE),
            ATEND_baseline = dplyr::first(
              KLINISK_ATEND, order_by = ASSESSMENT_DATE),
            BIPAP_baseline = dplyr::first(
              KLINISK_BIPAP, order_by = ASSESSMENT_DATE),
            FUNKSJONSSTATUS_baseline = dplyr::first(
              KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE),
            ASSESSMENT_DATE_latest = dplyr::last(ASSESSMENT_DATE),
            HFMSE_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_HFMSE, order_by = ASSESSMENT_DATE)),
            RULM_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_RULM, order_by = ASSESSMENT_DATE)),
            x6MWT_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_6MWT, order_by = ASSESSMENT_DATE)),
            ATEND_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_ATEND, order_by = ASSESSMENT_DATE)),
            BIPAP_latest = ifelse(dplyr::last(
              ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_BIPAP, order_by = ASSESSMENT_DATE)),
            FUNKSJONSSTATUS_latest = ifelse(
              dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
              NA, dplyr::last(KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE)),
            Tidsdiff_dager = difftime(
              ASSESSMENT_DATE_latest, ASSESSMENT_DATE_baseline, units = "days"),
            FUNKSJONSSTATUS_all = paste0(BEHANDLNG_FUNKSJONSSTATUS, collapse = ","),
            BEHANDLING_all = paste0(BEHANDLNG_BEHANDLING, collapse = ","),
            N = dplyr::n(),
            .by = c(PATIENT_ID, forlop_id)) |>
          dplyr::select(-forlop_id) |>
          dplyr::filter(
            N >= input$antall_oppf[1],
            N <= input$antall_oppf[2],
            Medikamentforlop_SMA %in% input$type_medikamentforlop)
      }

      output$forlop_tab <- DT::renderDT({

        tabell_sma <- tabell_shusinnkjop2()

        names(tabell_sma) <- c(
          "PATIENT_ID", "Medikamentforlop_SMA", "CENTREID",
          "ASSESSMENT_DATE", "HFMSE",
          "RULM", "6MWT", "ATEND",
          "BIPAP", "KLINISK FUNKSJONSSTATUS",
          "ASSESSMENT_DATE", "HFMSE",
          "RULM", "6MWT", "ATEND",
          "BIPAP", "KLINISK FUNKSJONSSTATUS",
          "Tidsdiff_dager", "FUNKSJONSSTATUS ALLE",
          "BEHANDLING ALLE", "N"
        )

        DT::datatable(
          tabell_sma,
          rownames = FALSE,
          escape = FALSE,
          class = "compact stripe hover",
          options = list(
            scrollX = TRUE,
            pageLength = 25,
            autoWidth = TRUE
          ),

          container = htmltools::withTags(

            table(
              class = "display",

              thead(

                # Øverste header-rad
                tr(

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "PATIENT_ID"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "Medikamentforlop_SMA"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "CENTREID"
                  ),

                  th(
                    colspan = 7,
                    style = paste(
                      "background:#eef5ff;",
                      "font-weight:bold;",
                      "text-align:center;",
                      "border-right:6px solid #999;",
                      "border-bottom:2px solid #666;"
                    ),
                    "Baseline"
                  ),

                  th(
                    colspan = 7,
                    style = paste(
                      "background:#fff8e8;",
                      "font-weight:bold;",
                      "text-align:center;",
                      "border-bottom:2px solid #666;"
                    ),
                    "Siste måling"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "Tidsdiff_dager"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "FUNKSJONSSTATUS ALLE"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "BEHANDLING ALLE"
                  ),

                  th(
                    rowspan = 2,
                    style = "vertical-align:middle;",
                    "N"
                  )
                ),

                # Nederste header-rad
                tr(

                  th(style = "background:#eef5ff;", "ASSESSMENT_DATE"),
                  th(style = "background:#eef5ff;", "HFMSE"),
                  th(style = "background:#eef5ff;", "RULM"),
                  th(style = "background:#eef5ff;", "6MWT"),
                  th(style = "background:#eef5ff;", "ATEND"),
                  th(style = "background:#eef5ff;", "BIPAP"),

                  th(
                    style = paste(
                      "background:#eef5ff;",
                      "border-right:6px solid #999;"
                    ),
                    "KLINISK FUNKSJONSSTATUS"
                  ),

                  th(style = "background:#fff8e8;", "ASSESSMENT_DATE"),
                  th(style = "background:#fff8e8;", "HFMSE"),
                  th(style = "background:#fff8e8;", "RULM"),
                  th(style = "background:#fff8e8;", "6MWT"),
                  th(style = "background:#fff8e8;", "ATEND"),
                  th(style = "background:#fff8e8;", "BIPAP"),
                  th(style = "background:#fff8e8;", "KLINISK FUNKSJONSSTATUS")
                )
              )
            )
          )
        ) %>%

          # Fortsett separatoren ned gjennom dataene
          DT::formatStyle(
            columns = 10,
            borderRight = "6px solid #999"
          )

      })

      output$last_ned_forlop_tab <- downloadHandler(

        filename = function() {
          paste0(
            "sma_forlop_",
            format(Sys.Date(), "%Y%m%d"),
            ".xlsx"
          )
        },

        content = function(file) {

          tabell_sma <- tabell_shusinnkjop2()

          names(tabell_sma) <- c(
            "PATIENT_ID", "Medikamentforlop_SMA", "CENTREID",
            "ASSESSMENT_DATE", "HFMSE",
            "RULM", "6MWT", "ATEND",
            "BIPAP", "KLINISK FUNKSJONSSTATUS",
            "ASSESSMENT_DATE", "HFMSE",
            "RULM", "6MWT", "ATEND",
            "BIPAP", "KLINISK FUNKSJONSSTATUS",
            "Tidsdiff_dager", "FUNKSJONSSTATUS ALLE",
            "BEHANDLING ALLE", "N"
          )

          wb <- openxlsx::createWorkbook()

          openxlsx::addWorksheet(
            wb,
            "Forløp"
          )

          # Gruppe-rad
          openxlsx::writeData(
            wb,
            sheet = 1,
            x = matrix(
              c(
                "",
                "",
                "",
                "Baseline",
                rep("", 6),
                "Siste måling",
                rep("", 6),
                "",
                "",
                "",
                ""
              ),
              nrow = 1
            ),
            startRow = 1,
            colNames = FALSE
          )

          # Merge grupper
          openxlsx::mergeCells(
            wb,
            1,
            cols = 4:10,
            rows = 1
          )

          openxlsx::mergeCells(
            wb,
            1,
            cols = 11:17,
            rows = 1
          )

          # Kolonnenavn
          openxlsx::writeData(
            wb,
            sheet = 1,
            x = as.data.frame(t(names(tabell_sma))),
            startRow = 2,
            colNames = FALSE
          )

          # Data
          openxlsx::writeData(
            wb,
            sheet = 1,
            x = tabell_sma,
            startRow = 3,
            colNames = FALSE
          )

          # Stiler
          group_style <- openxlsx::createStyle(
            textDecoration = "bold",
            halign = "center",
            fgFill = "#D9EAF7",
            border = "bottom"
          )

          last_style <- openxlsx::createStyle(
            textDecoration = "bold",
            halign = "center",
            fgFill = "#F8F0D8",
            border = "bottom"
          )

          header_style <- openxlsx::createStyle(
            textDecoration = "bold"
          )

          openxlsx::addStyle(
            wb, 1, group_style,
            rows = 1, cols = 4:10,
            gridExpand = TRUE
          )

          openxlsx::addStyle(
            wb, 1, last_style,
            rows = 1, cols = 11:17,
            gridExpand = TRUE
          )

          openxlsx::addStyle(
            wb, 1, header_style,
            rows = 2,
            cols = 1:ncol(tabell_sma),
            gridExpand = TRUE
          )

          openxlsx::freezePane(
            wb,
            sheet = 1,
            firstRow = TRUE,
            firstCol = TRUE
          )

          openxlsx::setColWidths(
            wb,
            1,
            cols = 1:ncol(tabell_sma),
            widths = "auto"
          )

          openxlsx::saveWorkbook(
            wb,
            file,
            overwrite = TRUE
          )
          rapbase::repLogger2(
            user = user,
            msg = paste0("Laster ned oversikt over SMA medikamentforløp.")
          )
        }
      )

    }
  )

}
