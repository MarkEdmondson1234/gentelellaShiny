#' A box to put dashboard elements in
#'
#' @param ... elements to put in the box
#' @param width Box width
#' @param height Box height
#' @param title Box title
#' @param subtitle Box subtitle
#' @param collapsible Whether the box can be collapsed. TRUE by default.
#' @param closable Whether the box can be closed. FALSE by default.
#' @param dropdownMenu Dropdown menu: list of dropdown links. See example below.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     box(
#'      "test",
#'      width = 4,
#'      height = NULL,
#'      title = "Box title",
#'      closable = TRUE,
#'      dropdownMenu = list(
#'       a(href = "http://www.google.com", "Test", target = "_blank"),
#'       a(href = "http://www.google.com", "Test", target = "_blank")
#'      )
#'     )
#'    )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @export
box <- function(..., width = 4, height = NULL,
                title = "Box title", subtitle = NULL, collapsible = TRUE,
                closable = FALSE, dropdownMenu = NULL){

  menuItems <- list(
    if (collapsible) shiny::a(class = "collapse-link", shiny::icon("chevron-up")),
    if (!is.null(dropdownMenu)) dropdownMenu,
    if (closable) shiny::a(class = "close-link", icon("close"))
  )

  shiny::tags$div(
    class = paste0(paste(c("col-md","col-sm"), width, sep = "-", collapse = " "), " col-xs-12"),
    shiny::tags$div(
      class = "x_panel tile",
      style = paste0("height: ", height, "px;"),
      shiny::tags$div(
        class = "x_title",
        shiny::tags$h2(title, shiny::tags$small(class = "pull-right", subtitle)),
        shiny::tags$ul(
          class = "nav navbar-right panel_toolbox",
          ## add more items to li menu if passed.
          shiny::tagList(lapply(X = 1:length(menuItems), FUN = function(i) {
            current_item_class <- class(menuItems[[i]])
            current_items <- menuItems[[i]]
            # this handles the case where the user provide a dropdown menu as
            # input
            if (current_item_class == "list") {
              shiny::tagList(
                shiny::tags$li(
                  class = "dropdown",
                  shiny::tags$a(
                    class = "dropdown-toggle",
                    `aria-expanded` = "false",
                    `data-toggle` = "dropdown",
                    href = "#",
                    role = "button",
                    shiny::icon("wrench")
                  ),
                  shiny::tags$ul(
                    class = "dropdown-menu",
                    role = "menu",
                    lapply(current_items, shiny::tags$li)
                  )
                )
              )
            } else {
              shiny::tags$li(menuItems[[i]])
            }
          })
          )
        ),
        shiny::tags$div(class = "clearfix")
      ),
      shiny::tags$div(
        class = "x_content",
        shiny::tagList(...)
      )
    )
  )
}





#' A social box to present a product or people
#'
#' @param ... elements to put in the box
#' @param width Box width
#' @param height Box height
#' @param title Box title
#' @param url_1 Box link 1
#' @param url_2 Box link 2,
#' @param media_1 Media name like twitter, facebook, ...
#' @param media_2 Media name like twitter, facebook, ...
#' @param profile_img Box profile image
#' @param footer Box footer content, if any.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     socialBox(
#'      title = "Social Box",
#'      url_1 = "https://www.facebook.com",
#'      url_2 = "https://twitter.com",
#'      media_1 = "facebook",
#'      media_2 = "twitter",
#'      profile_img = "https://image.flaticon.com/icons/svg/17/17004.svg",
#'      footer = "If you\'ve decided to go in development
#'      mode and tweak all of this a bit, there are few
#'      things you should do.",
#'      socialStats(
#'       socialStatsItem(value = 123, name = "Articles"),
#'       socialStatsItem(value = 1234, name = "Followers"),
#'       socialStatsItem(value = 435, name = "Following")
#'      )
#'     )
#'    )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @export
socialBox <- function(..., width = 3, height = 390,
                                title = NULL, url_1 = NULL, url_2 = NULL,
                                media_1 = NULL, media_2 = NULL,
                                profile_img = NULL, footer = NULL) {
  shiny::div(
    class = paste0("col-md-", width, " col-xs-12 widget widget_tally_box"),
    shiny::div(
      class = paste0("x_panel fixed_height_", height),
      shiny::div(
        class = "x_content",
        shiny::div(
          class = "flex",
          shiny::tags$ul(
            class = "list-inline widget_profile_box",
            shiny::tags$li(
              shiny::a(
                href = url_1,
                target = "_blank",
                shiny::icon(media_1)
              )
            ),
            shiny::tags$li(
              shiny::img(
                src = profile_img,
                class = "img-circle profile_img"
              )
            ),
            shiny::tags$li(
              shiny::a(
                href = url_2,
                target = "_blank",
                shiny::icon(media_2)
              )
            )
          )
        ),
        shiny::h3(class = "name", title),
        shiny::div(
          class = "flex",
          ...
        ),
        shiny::p(footer)
      )
    )
  )
}




#' A box with a ribbon
#'
#' @param ... elements to put in the box
#' @param width Box width
#' @param height Box height
#' @param ribbon_text Ribbon text
#' @param title Box title
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     ribbonBox(
#'       ribbon_text = "30 % Off",
#'       title = "Ribbon Box",
#'       "If you've decided to go in development mode and
#'       tweak all of this a bit, there are few things
#'       you should do.",
#'       pieChart(id = "chart1", value = 10),
#'       pieChart(id = "chart2", value = 20)
#'      )
#'     )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @export
ribbonBox <- function(..., width = 3, height = 390, ribbon_text = NULL,
                                title = NULL) {
  shiny::div(
    class = paste0("col-md-", width, " col-xs-12 widget widget_tally_box"),
    shiny::div(
      class = paste0("x_panel ui-ribbon-container fixed_height_", height),
      # ribbon
      shiny::div(
        class = "ui-ribbon-wrapper",
        shiny::div(class = "ui-ribbon", ribbon_text)
      ),
      # title
      shiny::div(
        class = "x_title",
        shiny::h2(title),
        shiny::div(class = "clearfix")
      ),
      # content
      shiny::div(class = "x_content", ...)
    )
  )
}



#' A value box
#'
#' @param value value
#' @param title value box title
#' @param description description if any
#' @param icon value box icon if any
#' @param width value box width. 3 by default
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     valueBox(
#'      value = 179,
#'      title = "New Sign ups",
#'      description = "Lorem ipsum psdea itgum rixt",
#'      icon = "caret-square-o-right"
#'     ),
#'     valueBox(
#'      value = 345,
#'      title = "New Customers",
#'      description = "Lorem ipsum psdea itgum rixt",
#'      icon = "comments-o"
#'     )
#'    )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @export
valueBox <- function(value, title = NULL, description = NULL, icon = NULL, width = 3) {
 shiny::div(
   class = paste0("animated flipInY col-lg-3 col-md-", width," col-sm-6 col-xs-12"),
   shiny::div(
     class = "tile-stats",
     shiny::div(
       class = "icon",
       shiny::icon(icon)
     ),
     shiny::div(class = "count", value),
     shiny::h3(title),
     shiny::p(description)
   )
 )
}


#' A Contact box
#'
#' @param ... elements to put in the box. Do not put too large items.
#' @param width Box width. 3 by default
#' @param head_title Box header title
#' @param main_title Box main title
#' @param img Any picture url or path
#' @param footer_left Footer left content
#' @param footer_right Footer right content
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     contactBox(
#'      head_title = "Digital Strategist",
#'      main_title = "Nicole Pearson",
#'      img = "https://image.flaticon.com/icons/svg/145/145859.svg",
#'      footer_left = gentelellaStars(4),
#'      quickList(
#'       quickListItem(icon = "calendar-o", name = "Settings"),
#'       quickListItem(icon = "bars", name = "Subscription")
#'      )
#'     )
#'    )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @export
contactBox <- function(..., head_title = NULL, main_title = NULL,
                                 img = NULL, footer_left = NULL,
                                 footer_right = NULL, width = 4) {
 shiny::tags$div(
   class = paste0("col-md-4 col-sm-", width," col-xs-12 profile_details"),
   shiny::tags$div(
     class = "well profile_view",
     shiny::tags$div(
       class = "col-sm-12",
       shiny::tags$h4(class = "brief", shiny::tags$i(head_title)),
       # content
       shiny::tags$div(
         class = "left col-xs-7",
         shiny::tags$h2(main_title),
         shiny::tags$p(
           shiny::tags$strong("About: "),
           ...
         )
       ),
       # image
       shiny::tags$div(
         class = "right col-xs-5 text-center",
         shiny::tags$img(src = img, class = "img-circle img-responsive")
       )
     ),
     # footer
     shiny::tags$div(
       class = "col-xs-12 bottom text-center",
       shiny::tags$div(class = "col-xs-12 col-sm-6 emphasis", footer_left),
       shiny::tags$div(class = "col-xs-12 col-sm-6 emphasis", footer_right)
     )
   )
 )
}
