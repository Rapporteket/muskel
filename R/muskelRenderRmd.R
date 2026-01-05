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
muskelRenderRmd2 <- function(sourceFile,
                            outputType = c("html_document", "html_fragment", "pdf_document"),
                            params = list(),
                            keep_tex_on_pdf = TRUE,
                            verbose = FALSE) {

  # outputType <- match.arg(as.character(outputType))
  stopifnot(file.exists(sourceFile))

  src_dir <- normalizePath(dirname(sourceFile), mustWork = TRUE)
  src_rmd <- normalizePath(sourceFile, mustWork = TRUE)
  rmd_basename <- basename(src_rmd)

  # Isolated work directory
  work_dir <- file.path(tempdir(), paste0("rmd_work_", as.integer(runif(1, 1e6, 9e6))))
  dir.create(work_dir, recursive = TRUE, showWarnings = FALSE)

  # Copy entire source directory contents (files + subfolders) into work_dir
  # So that images, CSS, .bib, etc., are available during rendering
  to_copy <- list.files(src_dir, all.files = FALSE, full.names = TRUE, recursive = FALSE)
  if (length(to_copy)) {
    file.copy(to_copy, work_dir, recursive = TRUE, overwrite = TRUE)
  }
  # Ensure the specific Rmd file exists in work_dir
  if (!file.exists(file.path(work_dir, rmd_basename))) {
    file.copy(src_rmd, file.path(work_dir, rmd_basename), overwrite = TRUE)
  }

  # Build output_format with a resource path that includes work_dir and src_dir
  resource_path_arg <- paste0("--resource-path=", paste(c(work_dir, src_dir), collapse = ":"))

  output_format <- switch(
    outputType,
    "html_document" = rmarkdown::html_document(
      pandoc_args = resource_path_arg
    ),
    "html_fragment" = rmarkdown::html_fragment(
      pandoc_args = resource_path_arg
    ),
    "pdf_document"  = rmarkdown::pdf_document(
      latex_engine = "pdflatex",
      keep_tex = isTRUE(keep_tex_on_pdf),
      pandoc_args = resource_path_arg
    )
  )

  render_env <- new.env(parent = globalenv())

  f <- rmarkdown::render(
    input = file.path(work_dir, rmd_basename),
    output_format = output_format,
    output_file = tempfile(pattern = "", tmpdir = work_dir),
    knit_root_dir = work_dir,
    intermediates_dir = work_dir,   # critical: internal preamble & friends live here
    output_dir = work_dir,
    params = params,
    envir = render_env,
    quiet = !isTRUE(verbose),
    clean = TRUE
  )

  if (outputType == "html_fragment") {
    return(shiny::HTML(paste(readLines(f, warn = FALSE), collapse = "\n")))
  } else {
    return(f)
  }
}

