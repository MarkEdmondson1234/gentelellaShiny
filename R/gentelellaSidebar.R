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
#' @import shiny
gentelellaSidebar <- function(...,
                              site_title = shiny::HTML(paste(shiny::icon("paw"),
                                                             "Shiny Gentelella")),
                              url = NULL, fixed = FALSE, footer = NULL){

  sidebarCl <- "col-md-3 left_col"
  if (fixed) sidebarCl <- paste0(sidebarCl, " menu_fixed")

  tags$div(
    class = sidebarCl,
    tags$div(
      class = "left_col scroll-view",
      tags$div(
        class = "navbar nav_title",
        style = "border: 0;",
        tags$a(
          class = "site_title",
          site_title,
          href = url,
          target = "_blank"
        )
      ),
      tags$div(class = "clearfix"),
      # sidebar Menu and Items
      ...,
      # sidebar footer
      tags$div(
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
#' @import shiny
sidebarDate <- function() {
  div(
    id = "sidebar-menu",
    class = "main_menu_side hidden-print main_menu",
    div(
      class = "menu_section",
      h3(format(Sys.Date(), format = "%a - %d %B, %Y"))
    )
  )
}



#' Create a Gentelella dashboard sidebar menu
#'
#' @param ... Slot for \link{sidebarItem}
#' @param title Menu section title
#'
#' @author Mark Edmondson, \email{m@@sunholo.com}
#'
#' @export
#' @import shiny
sidebarMenu <- function(..., title = NULL) {

  items <- list(...)

  div(
    id = "sidebar-menu",
    class = "main_menu_side hidden-print main_menu",
    div(
      class = "menu_section",
      tags$h3(title),
      tags$ul(
        class = "nav side-menu",
        tagList(lapply(items, tags$li))
      )
    )
  )
}

#' Create a Gentelella dashboard sidebar menu item
#'
#' @param ... item name
#' @param tabName item id. Must be unique
#' @param icon item icon
#' @param badgeName item badge name if any.
#' @param badgeStatus item badge status: "danger", "warning", "info", "success" or "primary".
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
#' @importFrom htmltools withTags
sidebarItem <- function(..., tabName = NULL, icon = NULL,
                        badgeName = NULL, badgeStatus = "danger"){

  if (is.null(tabName))
    stop("Need tabName")

  withTags({
      tags$a(
        id = paste0("tab-", tabName),
        href = paste0("#shiny-tab-", tabName),
        `data-toggle` = "tab",
        `data-value` = tabName,
        tags$p(
          icon,
          ...,
          if (!is.null(badgeName)) {
            label(
              status = badgeStatus,
              name = badgeName,
              position = "pull-right"
            )
          }
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
#' @import shiny
sidebarProfile <- function(img = NULL, name = NULL) {
 tagList(
   tags$div(
     class = "profile clearfix",
     tags$div(
       class = "profile_pic",
       tags$img(src = img, class = "img-circle profile_img")
     ),
     tags$div(
       class = "profile_info",
       tags$span("Welcome, "),
       tags$h2(name)
     )
   ),
   br()
 )
}
