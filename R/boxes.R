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
#'   ui = gentelellaPageCustom(
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
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
box <- function(..., width = 4, height = NULL,
                title = "Box title", subtitle = NULL, collapsible = TRUE,
                closable = FALSE, dropdownMenu = NULL){

  menuItems <- list(
    if (collapsible) a(class = "collapse-link", icon("chevron-up")),
    if (!is.null(dropdownMenu)) dropdownMenu,
    if (closable) a(class = "close-link", icon("close"))
  )

  tags$div(
    class = paste0(paste(c("col-md","col-sm"), width, sep = "-", collapse = " "), " col-xs-12"),
    tags$div(
      class = "x_panel tile",
      style = paste0("height: ", height, "px;"),
      tags$div(
        class = "x_title",
        tags$h2(title, tags$small(class = "pull-right", subtitle)),
        tags$ul(
          class = "nav navbar-right panel_toolbox",
          ## add more items to li menu if passed.
          tagList(lapply(X = 1:length(menuItems), FUN = function(i) {
            current_item_class <- class(menuItems[[i]])
            current_items <- menuItems[[i]]
            # this handles the case where the user provide a dropdown menu as
            # input
            if (current_item_class == "list") {
              tagList(
                tags$li(
                  class = "dropdown",
                  tags$a(
                    class = "dropdown-toggle",
                    `aria-expanded` = "false",
                    `data-toggle` = "dropdown",
                    href = "#",
                    role = "button",
                    icon("wrench")
                  ),
                  tags$ul(
                    class = "dropdown-menu",
                    role = "menu",
                    lapply(current_items, tags$li)
                  )
                )
              )
            } else {
              tags$li(menuItems[[i]])
            }
          })
          )
        ),
        tags$div(class = "clearfix")
      ),
      tags$div(
        class = "x_content",
        tagList(...)
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
#'   ui = gentelellaPageCustom(
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
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
socialBox <- function(..., width = 3, height = 390,
                                title = NULL, url_1 = NULL, url_2 = NULL,
                                media_1 = NULL, media_2 = NULL,
                                profile_img = NULL, footer = NULL) {
  div(
    class = paste0("col-md-", width, " col-xs-12 widget widget_tally_box"),
    div(
      class = paste0("x_panel fixed_height_", height),
      div(
        class = "x_content",
        div(
          class = "flex",
          tags$ul(
            class = "list-inline widget_profile_box",
            tags$li(
              a(
                href = url_1,
                target = "_blank",
                icon(media_1)
              )
            ),
            tags$li(
              img(
                src = profile_img,
                class = "img-circle profile_img"
              )
            ),
            tags$li(
              a(
                href = url_2,
                target = "_blank",
                icon(media_2)
              )
            )
          )
        ),
        h3(class = "name", title),
        div(
          class = "flex",
          ...
        ),
        p(footer)
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
#' @param ribbon_color Ribbon background color: "red", "orange", "blue", "purple", "green".
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
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
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
ribbonBox <- function(..., width = 3, height = 390, ribbon_text = NULL,
                      title = NULL, ribbon_color = NULL) {

  ribbonCl <- "ui-ribbon"
  if (!is.null(ribbon_color)) ribbonCl <- paste0(ribbonCl, " bg-", ribbon_color)

  div(
    class = paste0("col-md-", width, " col-xs-12 widget widget_tally_box"),
    div(
      class = paste0("x_panel ui-ribbon-container fixed_height_", height),
      # ribbon
      div(
        class = "ui-ribbon-wrapper",
        div(class = ribbonCl, ribbon_text)
      ),
      # title
      div(
        class = "x_title",
        h2(title),
        div(class = "clearfix")
      ),
      # content
      div(class = "x_content", ...)
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
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     valueBox(
#'      value = 179,
#'      title = "New Sign ups",
#'      description = "Lorem ipsum psdea itgum rixt",
#'      icon = icon("caret-square-o-right")
#'     ),
#'     valueBox(
#'      value = 345,
#'      title = "New Customers",
#'      description = "Lorem ipsum psdea itgum rixt",
#'      icon = icon("comments-o")
#'     )
#'    )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
valueBox <- function(value, title = NULL, description = NULL, icon = NULL, width = 3) {
 div(
   class = paste0("animated flipInY col-lg-3 col-md-", width," col-sm-6 col-xs-12"),
   div(
     class = "tile-stats",
     div(
       class = "icon",
       icon
     ),
     div(class = "count", value),
     h3(title),
     p(description)
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
#' @note Does not work perfectly
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     contactBox(
#'      head_title = "Digital Strategist",
#'      main_title = "Nicole Pearson",
#'      img = "https://image.flaticon.com/icons/svg/145/145859.svg",
#'      footer_left = "Some text",
#'      footer_right = pieChart(value = 45, id = "contactChart"),
#'      quickList(
#'       quickListItem(icon = icon("calendar-o"), name = "Settings"),
#'       quickListItem(icon = icon("bars"), name = "Subscription")
#'      )
#'     )
#'    )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
contactBox <- function(..., head_title = NULL, main_title = NULL,
                       img = NULL, footer_left = NULL,
                       footer_right = NULL, width = 4) {
 tags$div(
   class = paste0("col-md-4 col-sm-", width," col-xs-12 profile_details"),
   tags$div(
     class = "well profile_view",
     tags$div(
       class = "col-sm-12",
       tags$h4(class = "brief", tags$i(head_title)),
       # content
       tags$div(
         class = "left col-xs-7",
         tags$h2(main_title),
         tags$p(
           tags$strong("About: "),
           ...
         )
       ),
       # image
       tags$div(
         class = "right col-xs-5 text-center",
         tags$img(src = img, class = "img-circle img-responsive")
       )
     ),
     # footer
     tags$div(
       class = "col-xs-12 bottom text-center",
       tags$div(class = "col-xs-12 col-sm-6 emphasis", footer_left),
       tags$div(class = "col-xs-12 col-sm-6 emphasis", footer_right)
     )
   )
 )
}

#' Graph box
#'
#' A box for graphs
#'
#' @param width width of box
#' @param boxtitle Title of box
#' @param subtitle Sub-title of box
#' @param datepicker a dateRangeInput or similar
#' @param ... Other elements to appear in the box such as graphs.
#'
#' @return a box with a datepicker to put plots in
#' @export
#' @import shiny
graph_box <- function(...,
                      width = 12,
                      boxtitle = "Impressive Title",
                      subtitle = "sub-title",
                      datepicker = dateRangeInput("datepicker_id", NULL)
){

  withTags({
    div(class = paste(c("col-md","col-sm","col-xs"), width, sep = "-", collapse = " "),
        div(class = "dashboard_graph",
            div(class = "row x_title",
                div(class = "col-md-6",
                    h3(boxtitle,
                       tags$small(subtitle)
                    )
                ),
                div(class = "col-md-6",
                    ## ideally class would be pull-right but bug prevents seeing Sunday...
                    div(id="reportrange", class = "", style="padding: 10px 5px 1px",
                        datepicker)
                )
            ),
            tagList(...),
            div(class="clearfix")
        )
    )

  })
}

#' A box to put dashboard elements in
#'
#' Its 320 pixels high by default
#'
#' @param ... Elements to put in the box.
#' @param width Width
#' @param height Box height.
#' @param box_title Title above.
#' @param menuItems A list of other things to appear in top menu.
#'
#' @return A box to put elements in
#' @export
#' @import shiny
dashboard_box <- function(...,
                          width=4,
                          height=320,
                          box_title = "Box title",
                          menuItems = list(a(class = "collapse-link", icon("chevron-up")))
){

  withTags({
    div(class = paste0(paste(c("col-md","col-sm"), width, sep = "-", collapse = " "), " col-xs-12"),
        div(class = "x_panel tile", style = paste0("height: ", height, "px;"),
            div(class = "x_title",
                h2(box_title),
                tags$ul(class = "nav navbar-right panel_toolbox",
                   ## add more items to li menu if passed.
                   tagList(lapply(menuItems, tags$li))
                ),
                div(class="clearfix")
            ),
            div(class = "x_content",
                tagList(...)
            )
        )
    )
  })
}
