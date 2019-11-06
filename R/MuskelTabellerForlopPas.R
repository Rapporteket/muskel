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

    if (kjoen == 99) { kjoen <- c(0,1)}
    if(is.null(frlType) ) {frlType <-  unique(RegData$ForlopsType1)}
    if(avd == "Ja"){avd <-  c("Ja","Nei")}



    utData <- RegData %>%
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
            aar = lubridate::year(HovedDato) ) %>%
        dplyr::filter( as.Date(HovedDato) %>%
                           dplyr::between(as.Date(tidFra) ,as.Date(tidTil)),
                       PasientAlder %>% dplyr::between( aldmin,aldmax),
                       erMann %in% kjoen,
                       ForlopsType1 %in% frlType,
                       Avdod %in% avd) %>%
        dplyr::select(PasientID, ForlopsID, SykehusNavn, maaned, aar) %>%
        dplyr::arrange(aar,maaned)
    tData <- utData%>%
        dplyr::filter(!duplicated( utData[[IDType]]))

    tab <-  stats::addmargins( table(tData[["SykehusNavn"]],
                                         tData[[tidenh]] ) )
}
