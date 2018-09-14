#' Create a Gentelella dashboard navbar
#'
#' @param ... Any left UI elements
#' @param navbarItems Items placed on the right side
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaNavbar <- function(..., navbarItems = NULL){
  htmltools::withTags({
    shiny::div(
      class = "top_nav",
      shiny::div(
        class = "nav_menu",
        shiny::tags$nav(
          shiny::div(
            class = "nav toggle",
            shiny::a(id = "menu_toggle", shiny::icon("bars"))
          ),
          ...,
          shiny::tags$ul(
            class = "nav navbar-nav navbar-right",
            lapply(navbarItems, li)
          )
        )
      )
    )
  })
}
