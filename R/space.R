#' Insert space characters with one function for multiple Rmd output types
#'
#' Add linebreaks or new pages to multiple Rmd output types with compact
#' functions.
#'
#' @param x If output is latex, this argument specifies which type of line break
#'   to use: `0` for `\newline` and `1` for `\linebreak` (the default). If
#'   output is not latex this argument is ignored.
#'
#' @returns A formatted string, appropriate for current output type
#' @export
#' @rdname space
#'
#' @examples
#' cat(br(1))
#' cat(br(0))
#' cat(newpage())
br <- function(x = 1) {
  if (ltx()) {
    if (x) {
      return("\\newline")
    } else {
      return("\\linebreak")
    }
  } else {
    return("  \n")
  }
}

#' @export
#' @rdname space
newpage <- function() {
  if (ltx())
    return("\\newpage\n\n")
  else
    return('\n\n<div style="page-break-after: always;"></div>\n\n')
}
