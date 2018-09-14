#' Create a Gentelella dashboard sidebar
#'
#' @param ... Items to place in the sidebar such as gentelellaSidebarMenu, gentelellaSidebarProfile
#' @param site_title Dashboard title
#' @param url Link to external ressource
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaSidebar <- function(..., site_title = HTML(paste(icon("paw"), "Shiny Gentelella")),
                              url = NULL){
  htmltools::withTags({
    shiny::div(
      class = "col-md-3 left_col",
      shiny::div(
        class = "left_col scroll-view",
        shiny::div(
          class = "navbar nav_title",
          style = "border: 0;",
          shiny::a(
            class = "site_title",
            site_title,
            href = url,
            target = "_blank"
          )
        ),
        shiny::div(class = "clearfix"),
        shiny::br(),
        ...
      )
    )
  })
}


#' Create a Gentelella dashboard sidebar menu
#'
#' @param ... Items to place in the sidebar menu
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaSidebarMenu <- function(...) {

  items <- list(...)

  shiny::div(
    id = "sidebar-menu",
    class = "main_menu_side hidden-print main_menu",
    shiny::div(
      class = "menu_section",
      shiny::h3(format(Sys.Date(), format = "%a - %d %B, %Y")),
      shiny::tags$ul(
        class = "nav side-menu",
        shiny::tagList(lapply(items, tags$li))
      )
    )
  )
}

#' Create a Gentelella dashboard sidebar menu item
#'
#' @param icon item icon
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaSidebarItem <- function(tabName = NULL, icon = NULL, nested_element = NULL){
  htmltools::withTags({
    shiny::tagList(
      shiny::a(icon, tabName, if(!is.null(nested_element)) span(class = "fa fa-chevron-down")),
      shiny::tags$ul(
        class = "nav child_menu",
        shiny::tagList(lapply(nested_element, tags$li)
        )
      )
    )
  })
}
