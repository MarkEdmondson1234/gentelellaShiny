# #' A wizard container
# #'
# #' @param ... slot for \link{wizardItem}
# #' @param orientation wizard orientation: "horizontal" or "verticle"
# #'
# #' @examples
# #' if (interactive()) {
# #'  library(shiny)
# #'  library(gentelellaShiny)
# #'  shinyApp(
# #'   ui = gentelellaPageCustom(
# #'    gentelellaBody(
# #'     wizard(
# #'      wizardItem(1, description = "blabla"),
# #'      wizardItem(2, description = "blabla")
# #'     )
# #'    )
# #'   ),
# #'   server = function(input, output, session) {}
# #'  )
# #' }
# #'
# #' @export
# wizard <- function(..., orientation = "horizontal") {
#
#   items <- list(...)
#   len_items <- length(items)
#
#   # add the proper number to each item
#   items <- lapply(X = 1:len_items, FUN = function(i) {
#     current_item <- items[[i]][["tag"]]
#     if (i == 1) current_item$attribs$style <- "display: block;"
#     htmltools::tagAppendAttributes(current_item, id = paste0("step-", i))
#   })
#
#   # create the corresponding menu
#   itemsMenu <- lapply(X = 1:len_items, FUN = function(i) {
#
#     current_item_desc <- items[["desc"]][[i]]
#
#     shiny::tags$li(
#       shiny::a(
#         href = paste0("#step-", i),
#         class = if (i == 1) "selected" else "disabled",
#         isdone = "1", # always 1 by default
#         rel = i,
#         shiny::span(class = "step_no", i),
#         shiny::span(
#           class = "step_descr",
#           paste("Step", i),
#           shiny::br(),
#           shiny::tags$small(current_item_desc)
#         )
#       )
#     )
#   })
#
#  # main tag
#  htmltools::withTags({
#    shiny::div(
#      id = if (orientation == "vertical") "wizard_verticle" else "wizard",
#      class = paste0("form_wizard wizard_", orientation),
#      shiny::tags$ul(
#        class = if (orientation == "vertical") {
#          "list-unstyled wizard_steps anchor"
#        } else {
#          "wizard_steps anchor"
#        },
#        # insert setp items
#        itemsMenu
#      ),
#      shiny::div(
#        class = "stepContainer",
#        style = "height: 154px;",
#        lapply(X = 1:len_items, function(i) { items[[i]] })
#      )#,
#      # action bar
#      #shiny::div(
#      #  class = "actionBar",
#      #  shiny::div(
#      #    class = "msgBox",
#      #    shiny::div(class = "content"),
#      #    shiny::a(href = "#", class = "close", "X")
#      #  ),
#      #  shiny::div(class = "loader", "Loading"),
#      #  shiny::a(href = "#", class = "buttonNext btn btn-success", "Next"),
#      #  shiny::a(href = "#", class = "buttonPrevious btn btn-primary", "Previous")
#      #)
#    )
#  })
# }
#
#
# #' A wizard item
# #'
# #' @param ... any UI element
# #' @param display Whether to diplay if or not. "none" by default.
# #' @param description Item description, if any.
# #'
# #' @export
# wizardItem <- function(..., display = "none", description = NULL) {
#   list(
#     tag = shiny::div(
#       class = "content",
#       style = paste0("display: ", display, ";"),
#       ...
#     ),
#     desc = description
#   )
# }



#' A social stats container
#'
#' @param ... slot for socialStatsItem
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
socialStats <- function(...) {
  shiny::tags$ul(class = "list-inline count2", ...)
}


#' A social stats item to insert in a socialStats container
#'
#' @param value Item value
#' @param name Item name
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
socialStatsItem <- function(value = NULL, name = NULL) {
 shiny::tags$li(
   shiny::tags$h3(value),
   shiny::tags$span(name)
 )
}


#' A simple circular diagram item
#'
#' https://github.com/rendro/easy-pie-chart
#'
#' @param id Unique id.
#' @param value Item value
#' @param height Canvas height. 220 px by default.
#' @param width Canvas width. 220 px by default.
#' @param barColor Default: #ef1e25.	The color of the curcular bar. You can either pass a valid css color string, or a function that takes the current percentage as a value and returns a valid css color string.
#' @param trackColor Default: #f2f2f2.	The color of the track, or false to disable rendering.
#' @param scaleColor Default: #dfe0e0.	The color of the scale lines, false to disable rendering.
#' @param scaleLength Default: 5.	Length of the scale lines (reduces the radius of the chart).
#' @param lineCap Default: 'round'.	Defines how the ending of the bar line looks like. Possible values are: butt, round and square.
#' @param lineWidth Default: 3.	Width of the chart line in px.
#' @param rotate Default: 0.	Rotation of the complete chart in degrees.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     box(
#'       title = "pieChart",
#'       "If you've decided to go in development mode and
#'       tweak all of this a bit, there are few things
#'       you should do.",
#'       pieChart(id = "chart1", value = 10),
#'       pieChart(
#'        id = "chart2",
#'        value = 20,
#'        barColor = "#0000FF",
#'        trackColor = "#FFA500",
#'        scaleColor = "#dfe0e0",
#'        scaleLength = 10,
#'        lineCap = "square",
#'        lineWidth = 6,
#'        rotate = 180
#'       )
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
pieChart <- function(id, value, height = 220, width = 220,
                     barColor = "#ef1e25", trackColor = "#f2f2f2",
                     scaleColor = "#dfe0e0", scaleLength = 5,
                     lineCap = "round", lineWidth = 3, rotate = 0) {
  pieChartTag <- shiny::div(
    style = "text-align: center; margin-bottom: 17px;",
    shiny::span(
      class = "chart",
      id = id,
      `data-percent` = value,
      shiny::span(
        class = "percent",
        value
      ),
      shiny::tags$canvas(height = height, width = width)
    )
  )

  # initialisation of the chart
  shiny::tagList(
    shiny::singleton(
      shiny::tags$head(
        shiny::tags$script(
          paste0(
            "$(function() {
              $('#", id, "').easyPieChart({
               //your options goes here
               barColor:'", barColor,"',
               trackColor:'", trackColor,"',
               scaleColor:'", scaleColor,"',
               scaleLength:", scaleLength,",
               lineCap:'", lineCap,"',
               lineWidth:", lineWidth,",
               rotate:", rotate,"
              });
            });
            "
          )
        )
      )
    ),
    pieChartTag
  )
}



#' A timeline block
#'
#' @param ... slot for \link{timelineItem}
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     box(
#'      width = 4,
#'      title = "Timeline",
#'      timeline(
#'       timelineItem(
#'        title = "Who Needs Sundance When You’ve Got Crowdfunding?",
#'        url = NULL,
#'        date = "13 hours ago",
#'        author = "Jane Smith",
#'        "Film festivals used to be do-or-die moments for movie makers.
#'        They were where you met the producers that could fund your
#'        project, and if the buyers liked your flick, they’d pay to
#'        Fast-forward and ..."
#'       ),
#'       timelineItem(
#'        title = "Who needs Money",
#'        url = "http:://www.google.com",
#'        date = "Today",
#'        author = "John Doe",
#'        "Nobody need money!",
#'        tag = "My tag"
#'       )
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
timeline <- function(...) {
  shiny::tags$ul(
    class = "list-unstyled timeline",
    ...
  )
}


#' A timeline item
#'
#' @param ... timeline item content, any element
#' @param title timeline item title
#' @param url timelime item external link
#' @param date timeline item date
#' @param author timeline item author, if any
#' @param tag timeline item tag
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
timelineItem <- function(..., title = NULL, url = NULL, date = NULL, author = NULL,
                                   tag = NULL) {
  shiny::tags$li(
    shiny::div(
      class = "block",
      if (!is.null(tag)) {
        shiny::div(
          class = "tags",
          shiny::a(
            href = NA,
            class = "tag",
            shiny::span(tag)
          )
        )
      },
      shiny::div(
        class = "block_content",
        shiny::h2(
          class = "title",
          shiny::a(
            title,
            href = url,
            target = "_blank"
          )
        ),
        shiny::div(
          class = "byline",
          shiny::span(date),
          "by",
          shiny::a(author)
        ),
        shiny::p(class = "excerpt", ...)
      )
    )
  )
}



#' A quick list container
#'
#' @param ... slot for \link{quickListItem}
#'
#'@examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     box(
#'      width = 4,
#'      title = "Quick Lists",
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
quickList <- function(...) {
 shiny::tags$ul(class = "quick-list", ...)
}


#' A quick list item
#'
#' @param icon item icon
#' @param name item name
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
quickListItem <- function(icon, name = NULL) {
 shiny::tags$li(icon, shiny::tags$a(name))
}




#' A box widget container
#'
#' @param ... slot for any widget
#' @param title widget title
#' @param width widget width
#'
#'@examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     box(
#'      width = 6,
#'      title = "Box Widget",
#'      fluidRow(
#'       column(
#'        width = 3,
#'        align = "center",
#'        sliderInput(
#'         "obs",
#'         "Number of observations:",
#'         min = 0,
#'         max = 1000,
#'         value = 500
#'        )
#'       ),
#'       column(
#'        width = 9,
#'        boxWidget(
#'         title = "Widget",
#'         plotOutput("distPlot")
#'        )
#'       )
#'      )
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {
#'    output$distPlot <- renderPlot({
#'     hist(rnorm(input$obs))
#'    })
#'   }
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
boxWidget <- function(..., title = NULL, width = NULL) {
  shiny::tags$div(
    class = "dashboard-widget-content",
    shiny::tags$div(
      class = "sidebar-widget",
      style = paste0("width: ", width, "px;"),
      shiny::tags$h4(title),
      ...
    )
  )
}



#' A user list block
#'
#' @param ... slot for userListItem
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     box(
#'      width = 3,
#'      title = "user List",
#'      userList(
#'       userListItem(
#'        user_img = "https://image.flaticon.com/icons/svg/145/145862.svg",
#'        user_url = "http:://www.google.com",
#'        title = "user 1",
#'        subtitle = "2 Sales Today",
#'        "$2300. Agent Avarage Sales."
#'       ),
#'       userListItem(
#'        user_img = "https://image.flaticon.com/icons/svg/145/145864.svg",
#'        user_url = "http:://www.google.com",
#'        title = "user 2",
#'        subtitle = "4 Sales Today",
#'        "$4600. Agent Avarage Sales."
#'       )
#'      )
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
userList <- function(...) {
 shiny::tags$ul(class = "list-unstyled msg_list", ...)
}


#' A user list item
#'
#' @param ... Any content
#' @param user_img User image
#' @param title item title
#' @param subtitle item subtitle
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
userListItem <- function(..., user_img = NULL, title = NULL, subtitle = NULL) {
 shiny::tags$li(
   shiny::tags$a(
     shiny::tags$span(
       class = "image",
       shiny::img(src = user_img),
       shiny::tags$span(
         shiny::tags$span(title),
         shiny::tags$span(class = "time", subtitle)
       ),
       shiny::tags$span(
         class = "message",
         ...
       )
     )
   )
 )
}



#' Progress bar
#'
#' Progress bars are scaled from 0 to 100
#'
#' @param value progress value
#' @param side From which side the bar comes: "left" or "right". "left" by default.
#' @param status progress status: "danger", "warning", "info", "success" or "primary".
#' When status is not NULL, color is NULL
#' @param striped Whether the progress bar is striped or not. FALSE by default.
#' @param color Alternative to status: "red", "orange", "green", "blue", "purple".
#' When color is not NULL, status is NULL.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     box(
#'      width = 3,
#'      title = "Progress Bars",
#'      progressBar(
#'       20,
#'       side = "left",
#'       status = "danger",
#'       striped = FALSE
#'      ),
#'      progressBar(
#'       70,
#'       side = "right",
#'       color = "purple",
#'       striped = TRUE
#'      )
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
progressBar <- function(value, side = "left", status = NULL, striped = FALSE,
                        color = NULL){

  progressBarCl <- "progress-bar"

  if (!is.null(status)) {
    if (!is.null(color)) color <- NULL
    progressBarCl <- paste0(progressBarCl, " progress-bar-", status)
  }
  if (!is.null(color)) {
    status <- NULL
    progressBarCl <- paste0(progressBarCl, " bg-", color)
  }

  if (side == "left") progressCl <- "progress" else progressCl <- "progress right"
  if (striped) progressCl <- paste0(progressCl, " progress-striped")

  shiny::tags$div(
    class = progressCl,
    shiny::tags$div(
      class = progressBarCl,
      `data-transitiongoal` = value,
      `aria-valuenow` = value,
      style = "width: 25%;"
    )
  )
}




#' A jumbotron
#'
#' @param ... Any UI element or text
#' @param title jumbotron title
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     jumbotron(
#'      title = "Hello, world!",
#'      "This is a simple hero unit, a simple jumbotron-style
#'      component for calling extra attention to featured
#'      content or information."
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
jumbotron <- function(..., title = NULL) {
 shiny::tags$div(
   class = "bs-example",
   `data-example-id` = "simple-jumbotron",
   shiny::tags$div(
     class = "jumbotron",
     shiny::tags$h1(title),
     shiny::tags$p(...)
   )
 )
}


#' An alert
#'
#' @param ... Alert text
#' @param title Alert title
#' @param status Alert status: "danger", "warning", "info", "success" or "primary"
#' @param dismissible Whether the alert is closable or not. TRUE by default.
#' @param width Alert width. 3 by default.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     alert(
#'      status = "warning",
#'      title = "An alert",
#'      "Best check yo self,
#'      you're not looking too good."
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
alert <- function(..., title = NULL, status = "primary", dismissible = TRUE, width = 3) {

  alertCl <- "alert fade in"
  if (!is.null(status)) alertCl <- paste0(alertCl, " alert-", status)
  if (dismissible) alertCl <- paste0(alertCl, " alert-dismissible")

  shiny::column(
    width = width,
    shiny::tags$div(
      class = alertCl,
      role = "alert",
      if (dismissible) shiny::tags$button(
        type = "button",
        class = "close",
        `data-dismiss` = "alert",
        `aria-label` = "Close",
        shiny::tags$span(`aria-hidden` = "true", "x")
      ),
      shiny::tags$strong(title),
      shiny::br(),
      ...
    )
  )
}



# #' A rating tag
# #'
# #' @param value value between 0 and 5
# #'
# #' @examples
# #' if (interactive()) {
# #'  library(shiny)
# #'  library(gentelellaShiny)
# #'  shinyApp(
# #'   ui = gentelellaPageCustom(
# #'    gentelellaBody(
# #'     stars(value = 4)
# #'    )
# #'   ),
# #'   server <- function(input, output) {}
# #'  )
# #' }
# #'
# #' @export
# stars <- function(value) {
#
#   stop_val <- 5 - value
#
#  shiny::tags$p(
#    class = "ratings",
#    shiny::tags$a(value),
#    if (value >= 1) {
#      # full stars
#      shiny::tagList(
#        lapply(X = 1:value, FUN = function(i) {
#          shiny::tags$a(shiny::tags$span(class = "fa fa-star"))
#        }),
#        # empty stars
#        lapply(X = 1:stop_val, FUN = function(i) {
#          shiny::tags$a(shiny::tags$span(class = "fa fa-star-o"))
#        })
#      )
#    } else {
#      lapply(X = 1:5, FUN = function(i) {
#        shiny::tags$a(shiny::tags$span(class = "fa fa-star-o"))
#      })
#    }
#  )
# }




#' An activity list
#'
#' @param ... Slot for activityItem
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     box(
#'      title = "Activity List",
#'      activityList(
#'       lapply(X = 1:3, FUN = function(i) {
#'        activityItem(
#'         title = "Desmond Davison",
#'         img = paste0("https://image.flaticon.com/icons/svg/1087/108783", i,".svg"),
#'         day = 13,
#'         month = "june",
#'         url = "http://www.google.com",
#'         "Raw denim you probably haven't heard of them jean shorts Austin.
#'         Nesciunt tofu stumptown aliqua butcher retro keffiyeh
#'         dreamcatcher synth."
#'        )
#'       })
#'      )
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
activityList <- function(...) {
 shiny::tags$ul(class = "messages", ...)
}



#' An activity item
#'
#' @param ... item content
#' @param title item title
#' @param img img path or url
#' @param day day of publication
#' @param month month of publication
#' @param url external link
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
activityItem <- function(..., title = NULL, img = NULL,
                         day = NULL, month = NULL, url = NULL) {
  shiny::tags$li(
    shiny::tags$img(src = img, class = "avatar"),
    shiny::tags$div(
      class = "message_date",
      shiny::tags$h3(class = "date text-info", day),
      shiny::tags$p(class = "month", month)
    ),
    shiny::tags$div(
      class = "message_wrapper",
      shiny::tags$h4(class = "heading", title),
      shiny::tags$blockquote(class = "message", ...),
      shiny::br(),
      shiny::tags$p(
        class = "url",
        shiny::tags$a(
          href = url,
          target = "_blank",
          shiny::tags$span(shiny::icon("info"), " More")
        )
      )
    )
  )
}




#' tileCountRow
#'
#' @param ... tileCountElements
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     tileCountRow(
#'       lapply(1:4, tileCountElement)
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @author Mark Edmondson, \email{m@@sunholo.com}
#'
#' @export
tileCountRow <- function(...){
  shiny::tags$div(class = "row tile_count", shiny::tagList(...))
}

#' tileCountRow Element
#'
#' @param value Count value
#' @param change_value Change value
#' @param going_well If TRUE then change_value is green, else red
#' @param tile_title Title text
#' @param width Width of tile in bootstrap
#' @param icon_in Icon to show
#' @param from_text Change text
#' @param highlight color to highlight value
#'
#' @return a tileCountRow for use within \link{tileCountRow}
#'
#' @author Mark Edmondson, \email{m@@sunholo.com}
#'
#' @export
tileCountElement <- function(value = 2500, change_value = "4%", going_well = TRUE,
                             tile_title = " Total Users", width = 3,
                             icon_in = shiny::icon("user"), from_text = " From last Week",
                             highlight = NULL){
  if (going_well) {
    bottom_icon <- shiny::tags$i(class = "green", shiny::icon("sort-asc"), change_value)
  } else {
    bottom_icon <- shiny::tags$i(class = "red", shiny::icon("sort-desc"), change_value)
  }

  htmltools::withTags({
    shiny::div(
      class = paste0("col-md-",width," col-sm-4 col-xs-6 tile_stats_count"),
      shiny::span(class = "count_top", icon_in, tile_title),
      shiny::div(class = paste("count", highlight), value),
      shiny::span(class = "count_bottom", bottom_icon, from_text)
    )
  })
}

#' tileCount UI
#'
#' Shiny Module for use with \link{tileCountElement}
#'
#' @param id Shiny id
#'
#' @return Shiny UI
#'
#' @author Mark Edmondson, \email{m@@sunholo.com}
#'
#' @export
tileCountUI <- function(id){
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("tile_count"))
}

#' updateTileCount
#'
#' Shiny Module for use with \link{tileCountUI}
#'
#' Call via \code{shiny::callModule(updateTileCount, "your_id")}
#'
#' @param input shiny input
#' @param output shiny output
#' @param session shiny session
#' @param value [reactive] Count value
#' @param change_value [reactive] Change value
#' @param going_well [reactive] If TRUE then change_value is green, else red
#' @param tile_title Title text
#' @param width Width of tile in bootstrap
#' @param icon_in Icon to show
#' @param from_text Change text
#' @param highlight [reactive] color to highlight value
#'
#' @return NULL
#'
#' @author Mark Edmondson, \email{m@@sunholo.com}
#'
#' @export
updateTileCount <- function(input, output, session, value, change_value,
                            going_well, tile_title = " Total Users",
                            width = 2, icon_in = shiny::icon("user"),
                            from_text = " From last Week", highlight = shiny::reactive(NULL)){

  ns <- session$ns

  output$tile_count <- shiny::renderUI({
    tileCountElement(
      value = value(),
      change_value = change_value(),
      going_well = going_well(),
      tile_title = tile_title,
      width = width,
      icon_in = icon_in,
      from_text = from_text,
      highlight = highlight()
    )
  })
}



#' Create a label or badge
#'
#' @param name label name
#' @param status label status: "danger", "warning", "info", "success" or "primary".
#' If mode is "badge" statuses are "red", "orange", "green", "blue", "purple".
#' @param position label position: NULL by default, "pull-right" or "pull-left".
#' If mode is "badge", position is NULL
#' @param mode Either "label" or "badge". "label" by default.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    gentelellaBody(
#'     box(
#'      title = "Labels",
#'      label(name = "David", status = "warning", mode = "badge"),
#'      br(), br(), br(),
#'      label(name = "Mark", position = "pull-right"),
#'      label(name = "Isabella", status = "danger", position = "pull-left")
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
label <- function(name = NULL, status = "primary",
                            position = NULL, mode = "label") {

  if (mode == "badge") position <- NULL
  if (mode == "badge") status <- "green"
  mode_switch <- switch(mode,
    "label" = "label",
    "badge" = "badge"
  )

  labelCl <- paste0(mode_switch, " ", if (mode_switch == "badge") "bg" else mode_switch, "-", status)
  if (!is.null(position)) labelCl <- paste0(labelCl, " ", position)
  shiny::tags$span(class = labelCl, name)
}

