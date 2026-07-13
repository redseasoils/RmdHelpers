#' Make lists for multiple Rmd outputs
#'
#' Compact functions to make lists in multiple Rmd output formats.
#'
#' @param ... Any number of strings or objects coercible to strings via
#'   [paste()]. For [latexWordHelpers::itemize()], these inputs will be combined
#'   into a single item in the output with an empty string as the separator. For
#'   [latexWordHelpers::enumerate()], each argument to `...` becomes one item in
#'   the list.
#' @param x Set to `0` (the default) to begin an unordered list, and to `1` to
#'   end an unordered list via [latexWordHelpers::itemize()].
#' @param y Options to pass to item in latex output (i.e. \item[y]{...}).
#' @param l Depth of item in (nested) lists. Defaults to 1. Only necessary if
#'   there is a non-latex output, as item depth is determined via the number of
#'   tab separators preceding the item.
#' @param start Number at which to start the ordered list.
#'
#' @returns A formatted string appropriate for the current output type.
#' @rdname list
#' @export
#'
#' @examples
#' # Unordered list, with one item's symbol changed to a diamond
#' itemize()
#' item("Hello world")
#' item("Hello world", "2", y = "\\$\\diamond\\$")
#' itemize(1)
#'
#' # Unordered, nested lists
#' itemize() # start level 1 list
#' item("Hello world")
#' itemize() # start level 2 list
#' item("Hello world", "2", y = "\\$\\diamond\\$", l = 2)
#' itemize(1) # end level 2 list
#' itemize(1) # end level 1 list
#'
#' # Ordered list
#' enumerate("One", "Two", "Three")
#'
#' # Ordered list starting at 2
#' enumerate("One", "Two", "Three", start = 2)
itemize <- function(x = 0) {
  if (ltx()) {
    if (!x) return("\\begin{itemize}") else return("\\end{itemize}")
  } else {
    return(paste0(br(), br()))
  }
}

#' @rdname list
#' @export
item <- function(..., y = NULL, l = 1) {
  x <- paste(c(...), collapse = "")
  if (ltx()) {
    r <- "\\item"
    if (!is.null(y)) r <- paste0(r, "[", y, "]")
    return(paste(r, x, ' '))
  } else {
    return(paste0(rep('  ', l - 1), "+ ", x, br(), br()))
  }
}

#' @rdname list
#' @export
enumerate <- function(..., start = 1) {
  x <- c(...)
  w <- lapply(1:length(x), \(y) paste0(y + start - 1, ". ", x[y]))
  w <- c(br(), br(), w, br(), br())
  l <- lapply(1:length(x), \(y) paste0("\\item{", x[y], "}"))
  if (!start == 1) l <- c(sprintf("\\setcounter{enumi}{%s}", start), l)
  l <- c("\\begin{enumerate}", l, "\\end{enumerate}")
  if (ltx())
    return(paste(l, collapse = ""))
  else
    return(paste(w, collapse = br()))
}