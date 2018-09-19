#' Create a Gentelella dashboard body content
#'
#' @param ... Items to place in the body such as \link{tabItems} an \link{tabItem}
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




#' A container for tab items
#'
#' @param ... Items to put in the container. Each item should be a \code{\link{tabItem}}.
#'
#' @export
tabItems <- function(...) {
  shiny::tags$div(class = "tab-content", ...)
}




#' One tab to put inside a tab items container
#'
#' @param tabName The name of a tab. This must correspond to the \code{tabName}
#'   of a \code{\link{sidebarItem}}.
#' @param ... Contents of the tab.
#'
#' @export
tabItem <- function(tabName = NULL, ...) {
  if (is.null(tabName))
    stop("Need tabName")

  shiny::tags$div(
    role = "tabpanel",
    class = "tab-pane container-fluid",
    id = paste0("shiny-tab-", tabName),
    shiny::br(),
    ...
  )
}
