#' Easy fontface changes in multiple Rmd outputs
#'
#' Shorthand functions to add a fontface when there are multiple Rmd output
#' types. For bold face: [latexWordHelpers::bf()], for italic:
#' [latexWordHelpers::it()], for bold-italic [latexWordHelpers::bi()], for
#' semibold [latexWordHelpers::sb()].
#'
#' @param x A string or an object coercible to a string
#'
#' @returns A formatted string appropriate for the current output
#' @export
#' @rdname fontface
#'
#' @examples
#' bf("hello world")
#' it("hello world")
#' bi("hello world")
#' sb("hello world")
bf <- function(x) {
  if (ltx())
    return(paste0('\\textbf{', x, '}'))
  else
    return(paste0('**', x, '**'))
}

#' @export
#' @rdname fontface
it <- function(x) {
  if (ltx())
    return(paste0('\\emph{', x, '}'))
  else
    return(paste0('*', x, '*'))
}

#' @export
#' @rdname fontface
bi <- function(x) {
  if (ltx())
    return(paste0('\\textbf{\\emph{', x, '}}'))
  else
    return(paste0('***', x, '***'))
}

#' @export
#' @rdname fontface
sb <- function(x) {
  if (ltx()) {
    return(paste("\\fontseries{semibold}\\selectfont", x,
                 "\\fontseries{regular}\\selectfont"))
  } else {
    return(paste0('**', x, '**'))
  }
}