#' Render documents from rmarkdown files at Rapporteket
#'
#' Function that renders documents at Rapporteket from rmarkdown source files.
#' Output formats may be (vanilla) HTML or PDF based on our own pandoc latex
#' template or fragments of html when the result is to be embedded in existing
#' web pages. Rmarkdown allow parameters to be part of report processing. Thus,
#' parameters that are specific to reports must be provided (as a named list)
#' when calling \code{muskelRenderRmd()}.
#'
#' @param sourceFile Character string providing the path to the rmarkdown
#' source file.
#' @param outputType Character string specifying the output format. Must be one
#' of \code{c("pdf_document", "html_document", "html_fragment")}.
#' @param params List of report parameters (as named values) to override the
#' corresponding entries under \emph{params} in the rmarkdown document yaml
#' header. Default is \code{NULL} in which case no parameters as defined in the
#' rmarkdown document will be overridden.
#'
#' @return Character string with path to the rendered file or, if
#' \code{outputType} is set to "html_fragment", a character string providing an
#' html fragment. Files are named according to \code{tempfile()} with an empty
#' pattern and with the extension according to \code{outputType}.
#'
#' @export
#'
muskelRenderRmd <- function(sourceFile, outputType = "html_document",
                            params = list()) {

  outputType <- as.character(outputType)

  stopifnot(file.exists(sourceFile))
  # stopifnot(outputType %in% c("html", "html_fragment", "pdf"))

  # do work in tempdir and return to origin on exit
  owd <- setwd(tempdir())
  on.exit(setwd(owd))

  # copy all files to temporary workdir
  file.copy(sourceFile, ".", overwrite = TRUE)

  f <- rmarkdown::render(
    input = basename(sourceFile),
    output_format = outputType,
    output_file = tempfile(pattern = ""),
    clean = TRUE,
    params = params,
    envir = new.env(),
    quiet = TRUE
  )

  if (outputType == "html_fragment") {
    return(shiny::HTML(readLines(f)))
  } else {
    return(f)
  }
}
