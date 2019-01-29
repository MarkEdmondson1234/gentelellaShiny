#' Create a Gentelella dashboard navbar
#'
#' @param ... Any left UI elements
#' @param navbarItems Items placed on the right side
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @importFrom htmltools withTags
#' @import shiny
gentelellaNavbar <- function(..., navbarItems = NULL){
  withTags({
    div(
      class = "top_nav",
      div(
        class = "nav_menu",
        tags$nav(
          div(
            class = "nav toggle",
            a(id = "menu_toggle", icon("bars"))
          ),
          ...,
          tags$ul(
            class = "nav navbar-nav navbar-right",
            navbarItems
          )
        )
      )
    )
  })
}



#' Create a Gentelella notification menu
#'
#' @param ... slot for \link{notifItem}
#' @param id menu id. Must be unique.
#' @param icon menu icon
#' @param status menu status: "danger", "warning", "info", "success" or "primary"
#' @param expanded Whether the dropdown menu is open or not. FALSE by default.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'    ui = gentelellaPageCustom(
#'     navbar = gentelellaNavbar(
#'     navbarItems = notif(
#'       id = "menunotif",
#'       icon = icon("envelope-o"),
#'       status = "danger",
#'       expanded = TRUE,
#'       lapply(X = 1:5, FUN = function(i) {
#'         notifItem(
#'           title = "John Doe",
#'           date = "3 min ago",
#'           img = paste0("https://image.flaticon.com/icons/svg/163/16382", i,".svg"),
#'           "Film festivals used to be do-or-die moments
#'           for movie makers. They were where..."
#'         )
#'       })
#'      )
#'     ),
#'     sidebar = gentelellaSidebar(),
#'     body = gentelellaBody(),
#'     footer = gentelellaFooter()
#'    ),
#'    server <- function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
notif <- function(..., id, icon = icon("envelope-o"),
                            status = "primary", expanded = FALSE) {

  len <- length(...)

  tags$li(
    class = if (expanded) "dropdown open" else "dropdown",
    role = "presentation",
    tags$a(
      class = "dropdown-toggle info-number",
      href = "javascript:;",
      `data-toggle` = "dropdown",
      `aria-expanded` = expanded,
      icon,
      label(name = len, status = status, mode = "badge")
    ),
    # content
    tags$ul(
      class = "dropdown-menu list-unstyled msg_list",
      role = "menu",
      id = id,
      ...
    )
  )
}



#' Create a Gentelella notification item
#'
#' @param ... notification content
#' @param title notification title
#' @param date notification date
#' @param img image url or path
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
notifItem <- function(..., title = NULL, date = NULL, img = NULL) {
 tags$li(
   tags$a(
     tags$span(
       class = "image",
       tags$img(src = img),
       tags$span(
        tags$span(title),
        tags$span(class = "time", date)
       ),
       tags$span(class = "message", ...)
     )
   )
 )
}
