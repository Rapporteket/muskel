library(muskel)
library(dplyr)
rm(list = ls())

SMAoversikt <- rapbase::loadRegData(
  registryName = "muskel",
  dbType = "mysql",
  query = "SELECT *
             FROM SMAoversikt"
)


oppsumm <- SMAoversikt %>%
  dplyr::mutate(ASSESSMENT_DATE = as.Date(ASSESSMENT_DATE)) %>%
  dplyr::arrange(ASSESSMENT_DATE) %>%
  dplyr::summarise(
    datoer = paste0(ASSESSMENT_DATE, collapse = ","),
    beh_beh = paste0(BEHANDLNG_BEHANDLING, collapse = ","),
    beh_spin = paste0(BEHANDLNG_SPINRAZA, collapse = ","),
    beh_avry = paste0(BEHANDLNG_EVRYSDI, collapse = ","),
    beh_funk = paste0(BEHANDLNG_FUNKSJONSSTATUS, collapse = ","),
    beh_ongoing = paste0(BEHANDLING_ONGOING, collapse = ","),
    .by = PATIENT_ID
  )






tabell_sma <- SMAoversikt %>%
  dplyr::mutate(ASSESSMENT_DATE = as.Date(ASSESSMENT_DATE)) %>%
  dplyr::arrange(ASSESSMENT_DATE) %>%
  dplyr::summarise(
    ASSESSMENT_DATE_baseline = dplyr::first(ASSESSMENT_DATE),
    HFMSE_baseline = dplyr::first(KLINISK_HFMSE, order_by = ASSESSMENT_DATE),
    RULM_baseline = dplyr::first(KLINISK_RULM, order_by = ASSESSMENT_DATE),
    x6MWT_baseline = dplyr::first(KLINISK_6MWT, order_by = ASSESSMENT_DATE),
    ATEND_baseline = dplyr::first(KLINISK_ATEND, order_by = ASSESSMENT_DATE),
    BIPAP_baseline = dplyr::first(KLINISK_BIPAP, order_by = ASSESSMENT_DATE),
    FUNKSJONSSTATUS_baseline = dplyr::first(KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE),
    ASSESSMENT_DATE_latest = dplyr::last(ASSESSMENT_DATE),
    HFMSE_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                          NA, dplyr::last(KLINISK_HFMSE, order_by = ASSESSMENT_DATE)),
    RULM_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                         NA, dplyr::last(KLINISK_RULM, order_by = ASSESSMENT_DATE)),
    x6MWT_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                          NA, dplyr::last(KLINISK_6MWT, order_by = ASSESSMENT_DATE)),
    ATEND_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                          NA, dplyr::last(KLINISK_ATEND, order_by = ASSESSMENT_DATE)),
    BIPAP_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                          NA, dplyr::last(KLINISK_BIPAP, order_by = ASSESSMENT_DATE)),
    FUNKSJONSSTATUS_latest = ifelse(dplyr::last(ASSESSMENT_DATE)==dplyr::first(ASSESSMENT_DATE),
                                    NA, dplyr::last(KLINISK_FUNKSJONSSTATUS, order_by = ASSESSMENT_DATE)),
    Tidsdiff_dager = difftime(ASSESSMENT_DATE_latest, ASSESSMENT_DATE_baseline, units = "days"),
    .by = PATIENT_ID) %>%
  dplyr::filter(Tidsdiff_dager != 0)






