#' Change font color for multiple Rmd output types
#'
#' Apply specified font color to specified text, formatted appropriately for
#' different Rmd outputs. RGB colors of the UIUC brand are supplied via
#' [colors()].
#'
#' @param ... Any number of strings (or objects coercible to strings via
#'   [paste0()]), which will be pasted together via [paste0()] with font color
#'   specified in `color`.
#' @param color Color to apply to font.
#'
#' @returns A formatted string, appropriate for the current output type.
#' @export
#' @rdname color
#'
#' @examples
#' color("hello world", color = latexWordHelpers::colors()$IlliniOrange)
color <- function(..., color) {
  x <- paste(c(...), collapse = "")
  if (ltx()) {
    return(paste0("\\textcolor{", color, "}{", x, "}"))
  } else {
    return(paste0('<span style="color: ', color, ';">', x, '</span>'))
  }
}

#' @export
#' @rdname color
colors <- function() {
  list(
    IlliniOrange = "rgb(255, 95, 5)",
    AltgeldOrange = "rgb(200, 65, 19)",
    IlliniBlue = "rgb(19, 41, 75)",
    IlliniGray = "rgb(90, 90, 90)",
  )
}