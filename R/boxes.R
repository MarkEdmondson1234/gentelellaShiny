#' A box to put dashboard elements in
#'
#' @param ... elements to put in the box
#' @param width Box width
#' @param height Box height
#' @param box_title Title above
#' @param menuItems A list of other things to appear in top menu
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaBox(
#'      "test",
#'      width = 4,
#'      height = NULL,
#'      title = "Box title",
#'      menuItems = list(
#'        a(class = "collapse-link", icon("chevron-up")),
#'        list(
#'          a(href = "http://www.google.com", "Test", target = "_blank"),
#'          a(href = "#", "Test2")
#'        ),
#'        a(class = "close-link", icon("close"))
#'      )
#'     )
#'    )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @export
gentelellaBox <- function(..., width = 4, height = NULL, title = "Box title",
                          menuItems = list(a(class = "collapse-link", icon("chevron-up")))){

  shiny::tags$div(
    class = paste0(paste(c("col-md","col-sm"), width, sep = "-", collapse = " "), " col-xs-12"),
    shiny::tags$div(
      class = "x_panel tile",
      style = paste0("height: ", height, "px;"),
      shiny::tags$div(
        class = "x_title",
        shiny::tags$h2(title),
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
                    icon("wrench")
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
        shiny::tags$div(class="clearfix")
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
#' @param media_1 Media name like twitter, facebook, ...
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
#'     gentelellaSocialBox(
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
gentelellaSocialBox <- function(..., width = 3, height = 390,
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
#'     gentelellaRibbonBox(
#'       ribbon_text = "30 % Off",
#'       title = "Ribbon Box",
#'       "If you've decided to go in development mode and
#'       tweak all of this a bit, there are few things
#'       you should do.",
#'       gentelellaGauge(10)
#'      )
#'     )
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @export
gentelellaRibbonBox <- function(..., width = 3, height = 390, ribbon_text = NULL,
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
#'     gentelellaValueBox(
#'      value = 179,
#'      title = "New Sign ups",
#'      description = "Lorem ipsum psdea itgum rixt",
#'      icon = "caret-square-o-right"
#'     ),
#'     gentelellaValueBox(
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
gentelellaValueBox <- function(value, title = NULL, description = NULL, icon = NULL, width = 3) {
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


#' Create a gentelella tabSetPanel
#'
#'
#' @param ... Slot for gentelellaTabPanel.
#' @param id TabSetPanel id. Should be unique.
#' @param right If TabSetPanel start from the right side. FALSE by default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaTabSetPanel(
#'      id = "tabset1",
#'      gentelellaTabPanel(
#'        tabName = "Home",
#'        active = TRUE,
#'        "Raw denim you probably haven't heard of
#'        them jean shorts Austin. Nesciunt tofu stumptown
#'        aliqua, retro synth master cleanse. Mustache
#'        cliche tempor, williamsburg carles vegan helvetica.
#'        Reprehenderit butcher retro keffiyeh dreamcatcher synth.
#'        Cosby sweater eu banh mi, qui irure terr."
#'      ),
#'      gentelellaTabPanel(
#'        tabName = "Profile",
#'        active = FALSE,
#'        sliderInput(
#'          "obs",
#'          "Number of observations:",
#'          min = 0,
#'          max = 1000,
#'          value = 500
#'        ),
#'        plotOutput("distPlot")
#'      )
#'     )
#'    )
#'   ),
#'   server = function(input, output, session) {
#'    output$distPlot <- renderPlot({
#'     hist(rnorm(input$obs))
#'    })
#'   }
#'  )
#' }
#'
#' @export
gentelellaTabSetPanel <- function(..., id, right = FALSE) {

  tabSetCl <- if (right) {
    "nav nav-tabs bar_tabs right"
  } else {
    "nav nav-tabs bar_tabs"
  }

  tabItems <- list(...)
  len_items <- length(tabItems)

  tabMenu <- lapply(X = 1:len_items, FUN = function(i) {
    current_item <- tabItems[[i]]
    current_item_cl <- current_item$attribs$class
    current_item_name <- current_item$attribs$id
    active <- sum(grep(x = current_item_cl, pattern = "active")) == 1
    shiny::tags$li(
      class = if (active == 1) "active" else NA,
      role = "presentation",
      shiny::tags$a(
        href = paste0("#", current_item_name),
        id = paste0(current_item_name, "-tab"),
        role = "tab",
        `data-toggle` = "tab",
        `aria-expanded` = if (active == 1) "true" else "false",
        current_item_name
      )
    )
  })

 shiny::tags$div(
   class = NA,
   role = "tabpanel",
   `data-example-id` = "togglable-tabs",
   shiny::tags$ul(
     class = tabSetCl,
     id = id,
     role = "tablist",
     tabMenu
   ),
   shiny::tags$div(
     class = "tab-content",
     id = paste0(id, "Content"),
     ...
   )
 )
}



#' Create a gentelella tabPanel
#'
#' To be included in a gentelellaTabSetPanel
#'
#' @param ... Tab content
#' @param tabName Tab name: it will be also passed as the id argument. Should be unique.
#' @param active Whether the tab is active or not. FALSE bu default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaTabPanel <- function(..., tabName, active = FALSE) {

  tabCl <- if (active) "tab-pane fade active in" else "tab-pane fade"

  shiny::tags$div(
    class = tabCl,
    role = "tabpanel",
    id = tabName,
    `aria-labelledby` = tabName,
    ...
  )
}
