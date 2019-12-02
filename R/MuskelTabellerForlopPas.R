#' Forløps of pasient tabeller for Muskelregisteret
#'
#' Denne funksjonen tar inn registerdata lager tabeller
#' av antall pasienter/forløp per sykehus per tidsenhet
#'
#' @param RegDt en dataramme
#' @param tidfra
#'
#'
#' @return tabelldata med antall pasienter/forløp per sykhus
#' per tidsenhet
#'
#' @export
#'

MuskelTabellerForlopspas <- function(RegDt = RegData, tidFra = "2008-01-01", tidTil = Sys.Date() ,
                       aldmin = 0, aldmax = 120, kjoen = 99, tidenh = "aar",
                       frlType = NULL, avd = "Nei", IDType = "PasientID"){

    if (kjoen == 99) {
        kjoen <- c(0,1)
    }
    if (!exists("frlType")) {
        frlType <-  unique(RegData$ForlopsType1)
    } else if(is.null(frlType) ) {
        frlType <-  unique(RegData$ForlopsType1)
    } else {
        frlType <- frlType
    }

    if(avd == "Ja"){
        avd <-  c("Ja","Nei")
    }

    lenFraTil <- length(seq(as.Date(tidFra), as.Date(tidTil), by = "month")) - 1
    if (tidenh == "maaned" & lenFraTil < 14) {
        tidenh <- "underEtAar"
    }


    tabData <- RegData %>%
        dplyr::select( PasientID,
                       ForlopsID,
                       SykehusNavn,
                       HovedDato,
                       erMann,
                       PasientAlder,
                       Avdod,
                       ForlopsType1Num,
                       ForlopsType1) %>%
        dplyr::mutate(maaned = factor(lubridate::month(HovedDato),
             labels = c("Jan","Feb","Mar", "Apr", "Mai",
            "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Des")) ,
            aar = lubridate::year(HovedDato),
            underEtAar = paste(maaned, "-", aar)) %>%
        dplyr::filter( as.Date(HovedDato) %>%
                           dplyr::between(as.Date(tidFra) ,as.Date(tidTil)),
                       PasientAlder %>% dplyr::between( aldmin,aldmax),
                       erMann %in% kjoen,
                       ForlopsType1 %in% frlType,
                       Avdod %in% avd) %>%
        dplyr::select(PasientID, ForlopsID, SykehusNavn, maaned, aar, underEtAar) %>%
        dplyr::arrange(aar,maaned)

    if (length (tabData[[tidenh]]) == 0) {
        return (data.frame(
            registreringer="0 registreringer i den valgte tidsperioden",
            Fra = as.character(lubridate::as_date(tidFra)),
            Til = as.character( lubridate::as_date(tidTil))))
    } else {

        tabData <- tabData%>%
            dplyr::filter(!duplicated( tabData[[IDType]]))
        tabData$underEtAar <- ordered(
            tabData$underEtAar,
            levels = unique(tabData$underEtAar)
        )

        utData <-  stats::addmargins( table(tabData[["SykehusNavn"]],
                                         tabData[[tidenh]] ) )
        return( utData)
    }
}
