#' Create a Gentelella dashboard body content
#'
#' @param ... Items to place in the body
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaBody <- function(...) {
  htmltools::withTags({
    shiny::div(
      class = "right_col",
      role = "main",
      style = "min-height: 1775px;",
      ...
    )
  })
}

