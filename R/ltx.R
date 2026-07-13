#' Send text to latex output
#'
#' Pass any number of strings (or objects coercible to strings via [paste()]),
#' and they will be printed to latex output only. Note that arguments will be
#' collapsed with an empty string as the separator.
#'
#' If no arguments are passed to the function, it becomes a logical test
#' equivalent to [knitr::is_latex_output()].
#'
#' @param ... Any number of strings or objects coercible to strings.
#'
#' @returns If the current output is latex and arguments are passed to `...`,
#'   returns a string. If the current output is not latex and arguments are
#'   passed to `...`, returns an empty string. If the current output is latex
#'   and arguments are not passed to `...`, returns `TRUE`. If the current
#'   output is latex and arguments are not passed to `...`, returns `FALSE`.
#' @export
#'
#' @examples
#' \dontrun{
#'   cat(ltx("hello", " world"))
#'   if (ltx()) cat("\\textbf{hello world}")
#' }
#'
#' @seealso [latexWordHelpers::word()]
ltx <-  function(...) {
  x <- c(...)
  return_text <- length(x) > 0
  return_bool <- length(x) == 0
  if (knitr::is_latex_output()) {
    if (return_text) return(paste(x, collapse = ""))
    if (return_bool) return(TRUE)
  } else {
    if (return_text) return("")
    if (return_bool) return(FALSE)
  }
}

