#' Automatisk linjebryting av lange tekstetiketter
#'
#' Automatisk linjebryting av lange tekstetiketter
#'
#' Her kan detaljer skrives
#'
#' @param x En tekststreng eller vektor av tekststrenger
#' @param len Lengden strengen skal brytes ved
#'
#' @return En tekststreng med tekstbrudd på angitt lengde
#'
#' @export
#'

# Core wrapping function
wrap.it <- function(x, len)
{
  sapply(x, function(y) paste(strwrap(y, len),
                              collapse = "\n"),
         USE.NAMES = FALSE)
}
