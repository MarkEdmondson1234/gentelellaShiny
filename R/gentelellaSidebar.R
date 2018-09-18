#' Create a Gentelella dashboard sidebar
#'
#' @param ... Items to place in the sidebar such as gentelellaSidebarMenu, gentelellaSidebarProfile
#' @param site_title Dashboard title
#' @param url Link to external ressource
#' @param fixed Whether the sidebar is fixed. FALSE by default.
#' @param footer Sidebar footer content, if any.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaSidebar <- function(..., site_title = HTML(paste(icon("paw"), "Shiny Gentelella")),
                              url = NULL, fixed = FALSE, footer = NULL){

  sidebarCl <- "col-md-3 left_col"
  if (fixed) sidebarCl <- paste0(sidebarCl, " menu_fixed")

  shiny::tags$div(
    class = sidebarCl,
    shiny::tags$div(
      class = "left_col scroll-view",
      shiny::tags$div(
        class = "navbar nav_title",
        style = "border: 0;",
        shiny::tags$a(
          class = "site_title",
          site_title,
          href = url,
          target = "_blank"
        )
      ),
      shiny::tags$div(class = "clearfix"),
      shiny::tags$br(),
      # sidebar Menu and Items
      ...,
      # sidebar footer
      shiny::tags$div(
        class = "sidebar-footer hidden-small",
        footer
      )
    )
  )
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
#' @param tabName item name
#' @param icon item icon
#' @param nested_element nested_element, if any
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaSidebarItem <- function(tabName = NULL, icon = NULL, nested_element = NULL){
  htmltools::withTags({
    shiny::tagList(
      shiny::a(icon, tabName, if (!is.null(nested_element)) span(class = "fa fa-chevron-down")),
      shiny::tags$ul(
        class = "nav child_menu",
        shiny::tagList(lapply(nested_element, tags$li)
        )
      )
    )
  })
}



#' Create a Gentelella dashboard sidebar profile item
#'
#' @param img profile image
#' @param name user name
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaSidebarProfile <- function(img = NULL, name = NULL) {
 shiny::tagList(
   shiny::tags$div(
     class = "profile clearfix",
     shiny::tags$div(
       class = "profile_pic",
       shiny::tags$img(src = img, class = "img-circle profile_img")
     ),
     shiny::tags$div(
       class = "profile_info",
       shiny::tags$span("Welcome, "),
       shiny::tags$h2(name)
     )
   ),
   br()
 )
}
