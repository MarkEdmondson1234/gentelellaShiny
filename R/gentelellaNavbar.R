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
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
notif <- function(..., id, icon = "envelope-o",
                            status = "primary", expanded = FALSE) {

  len <- length(...)

  if (expanded) "true" else "false"

  shiny::tags$li(
    class = "dropdown",
    role = "presentation",
    shiny::tags$a(
      class = "dropdown-toggle info-number",
      href = "javascript:;",
      `data-toggle` = "dropdown",
      `aria-expanded` = expanded,
      shiny::icon(icon),
      label(name = len, status = status, mode = "badge")
    ),
    # content
    shiny::tags$ul(
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
notifItem <- function(..., title = NULL, date = NULL, img = NULL) {
 shiny::tags$li(
   shiny::tags$a(
     shiny::tags$span(
       class = "image",
       shiny::tags$img(src = img),
       shiny::tags$span(
        shiny::tags$span(title),
        shiny::tags$span(class = "time", date)
       ),
       shiny::tags$span(class = "message", ...)
     )
   )
 )
}
