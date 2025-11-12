#' Generer SMArapport og returner filnavn og sti til fil.
#'
#' @export
lag_SMArapport <- function(baseName, reshID=0) {

  # rapbase::autLogger(user = brukernavn, registryName = 'NORNMD',
  #                   reshId = reshID[[1]], msg = "Abonnement: kvartalsrapport")

  src <- system.file(paste0(baseName, '.qmd'), package="muskel")
  tmpFile <- tempfile(paste0(baseName, Sys.Date()), fileext = '.qmd')

  owd <- setwd(tempdir())
  on.exit(setwd(owd))
  file.copy(src, tmpFile, overwrite = TRUE)

  quarto::quarto_render(tmpFile, output_format = "pdf")
  utfil <- paste0(substr(tmpFile, 1, nchar(tmpFile)-3), 'pdf')

  # rapbase::subLogger(author = brukernavn, registryName = 'NORGAST',
  #                   reshId = reshID[[1]], msg = paste("Sendt: ", utfil))

  return(utfil)
}
