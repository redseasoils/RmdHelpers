#' Send text to Word output
#'
#' Pass any number of strings (or objects coercible to strings via [paste()]),
#' and they will be printed to Word output only. Note that arguments will be
#' collapsed with an empty string as the separator.
#'
#' If no arguments are passed to the function, it becomes a logical test
#' evaluating if the output is *not* latex.
#'
#' @param ... Any number of strings or objects coercible to strings.
#'
#' @returns If the current output is Word and arguments are passed to `...`,
#'   returns a string. If the current output is not Word and arguments are
#'   passed to `...`, returns an empty string. If the current output is Word
#'   and arguments are not passed to `...`, returns `TRUE`. If the current
#'   output is Word and arguments are not passed to `...`, returns `FALSE`.
#' @export
#'
#' @examples
#' \dontrun{
#'   cat(word("hello", " ", "world"))
#'   if (word()) cat("**hello world**")
#' }
#'
#' @seealso [latexWordHelpers::ltx()]
word <- function(...) {
  x <- c(...)
  return_text <- length(x) > 0
  return_bool <- length(x) == 0
  if (!knitr::is_latex_output()) {
    if (return_text) return(paste(x, collapse = ""))
    if (return_bool) return(TRUE)
  } else {
    if (return_text) return("")
    if (return_bool) return(FALSE)
  }
}