#' Create a Gentelella dashboard sidebar
#'
#' @param ... Items to place in the sidebar such as \link{sidebarMenu}, \link{sidebarProfile}
#' @param site_title Dashboard title
#' @param url Link to external ressource
#' @param fixed Whether the sidebar is fixed. FALSE by default.
#' @param footer Sidebar footer content, if any.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaSidebar <- function(..., site_title = shiny::HTML(paste(shiny::icon("paw"), "Shiny Gentelella")),
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


#' Create a Gentelella dashboard sidebar date
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
sidebarDate <- function() {
  shiny::div(
    id = "sidebar-menu",
    class = "main_menu_side hidden-print main_menu",
    shiny::div(
      class = "menu_section",
      shiny::h3(format(Sys.Date(), format = "%a - %d %B, %Y"))
    )
  )
}



#' Create a Gentelella dashboard sidebar menu
#'
#' @param ... Slot for \link{sidebarItem}
#' @param title Menu section title
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
sidebarMenu <- function(..., title = NULL) {

  items <- list(...)

  shiny::div(
    id = "sidebar-menu",
    class = "main_menu_side hidden-print main_menu",
    shiny::div(
      class = "menu_section",
      shiny::tags$h3(title),
      shiny::tags$ul(
        class = "nav side-menu",
        shiny::tagList(lapply(items, shiny::tags$li))
      )
    )
  )
}

#' Create a Gentelella dashboard sidebar menu item
#'
# #' @param ... Slot for \link{sidebarSubItem}
#' @param tabName item name
#' @param icon item icon
#' @param nested_element nested_element, if any
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
sidebarItem <- function(tabName = NULL, icon = NULL){

  if (is.null(tabName))
    stop("Need tabName")

  #subitems <- list(...)

  htmltools::withTags({
    #if (length(subitems) == 0) {
      shiny::tags$a(
        id = paste0("tab-", tabName),
        href = paste0("#shiny-tab-", tabName),
        `data-toggle` = "tab",
        `data-value` = tabName,
        shiny::tags$p(shiny::icon(icon), tabName)
      )
    #} else {
      #shiny::tagList(
      #  shiny::a(shiny::icon(icon), tabName, shiny::span(class = "fa fa-chevron-down")),
      #  shiny::tags$ul(
      #    class = "nav child_menu",
      #    shiny::tagList(lapply(subitems, shiny::tags$li)
      #    )
      #  )
      #)
    #}
  })
}



# #' Create a Gentelella dashboard sidebar menu item
# #'
# #' @param tabName item name
# #' @param icon item icon
# #' @param nested_element nested_element, if any
# #'
# #' @author David Granjon, \email{dgranjon@@ymail.com}
# #'
# #' @export
# sidebarSubItem <- function(tabName = NULL, icon = NULL){
#   htmltools::withTags({
#     shiny::a(
#       id = paste0("tab-", tabName),
#       href = paste0("#shiny-tab-", tabName),
#       `data-toggle` = "tab",
#       `data-value` = tabName,
#       shiny::icon(icon),
#       shiny::tags$p(tabName)
#     )
#   })
# }


#' Create a Gentelella dashboard sidebar profile item
#'
#' @param img profile image
#' @param name user name
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
sidebarProfile <- function(img = NULL, name = NULL) {
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
   shiny::br()
 )
}
