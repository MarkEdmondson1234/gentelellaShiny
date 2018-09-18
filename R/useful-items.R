#' A wizard container
#'
#' @param ... slot for wizardItem
#' @param orientation wizard orientation: "horizontal" or "vertical"
#'
#' @export
wizard <- function(..., orientation = "horizontal") {

  items <- list(...)
  len_items <- length(items)

  # add the proper number to each item
  items <- lapply(X = 1:len_items, FUN = function(i) {
    current_item <- items[[i]][["tag"]]
    if (i == 1) current_item$attribs$style <- "display: block;"
    tagAppendAttributes(current_item, id = paste0("step-", i))
  })

  # create the corresponding menu
  itemsMenu <- lapply(X = 1:len_items, FUN = function(i) {

    current_item_desc <- items[["desc"]][[i]]

    shiny::tags$li(
      shiny::a(
        href = paste0("#step-", i),
        class = if (i == 1) "selected" else "done",
        isdone = "1", # always 1 by default
        rel = i,
        shiny::span(class = "step_no", i),
        shiny::span(
          class = "step_descr",
          paste("Step", i),
          shiny::br(),
          shiny::tags$small(current_item_desc)
        )
      )
    )
  })

 # main tag
 htmltools::withTags({
   shiny::div(
     id = if (orientation == "vertical") "wizard_vertical" else "wizard",
     class = paste0("form_wizard wizard_", orientation),
     shiny::tags$ul(
       class = if (orientation == "vertical") {
         "list-unstyled wizard_steps anchor"
       } else {
         "wizard_steps anchor"
       },
       # insert setp items
       itemsMenu
     ),
     shiny::div(
       class = "stepContainer",
       style = "height: 154px;",
       lapply(X = 1:len_items, function(i) { items[[i]] })
     ),
     # action bar
     shiny::div(
       class = "actionBar",
       shiny::div(
         class = "msgBox",
         shiny::div(class = "content"),
         shiny::a(href = "#", class = "close", "X")
       ),
       shiny::div(class = "loader", "Loading"),
       shiny::a(href = "#", class = "buttonPrevious btn btn-primary", "<-"),
       shiny::a(href = "#", class = "buttonNext btn btn-success", "->")
     )
   )
 })
}


#' A wizard item
#'
#' @param ... any UI element
#' @param display Whether to diplay if or not. "none" by default.
#' @param description Item description, if any.
#'
#' @export
wizardItem <- function(..., display = "none", description = NULL) {
  list(
    tag = shiny::div(
      class = "content",
      style = paste0("display: ", display, ";"),
      ...
    ),
    desc = description
  )

}



#' A social stats container
#'
#' @param ... slot for socialStatsItem
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
#' @export
socialStatsItem <- function(value = NULL, name = NULL) {
 shiny::tags$li(
   shiny::tags$h3(value),
   shiny::tags$span(name)
 )
}


#' A simple circular diagram item
#'
#' @param value Item value
#' @param height Canvas height. 220 px by default.
#' @param width Canvas width. 220 px by default.
#'
#' @export
gentelellaGauge <- function(value, height = 220, width = 220) {
  shiny::div(
    style = "text-align: center; margin-bottom: 17px;",
    shiny::span(
      class = "chart",
      `data-percent` = value,
      shiny::span(
        class = "percent",
        value
      ),
      shiny::tags$canvas(height = height, width = width)
    )
  )
}



#' A timeline block
#'
#' @param ... slot for gentelellaTimelineItem
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaBox(
#'      width = 4,
#'      title = "Timeline",
#'      menuItems = list(
#'        a(class = "collapse-link", icon("chevron-up")),
#'        list(
#'          a(href = "http://www.google.com", "Test", target = "_blank"),
#'          a(href = "#", "Test2")
#'        ),
#'        a(class = "close-link", icon("close"))
#'      ),
#'      gentelellaTimeline(
#'       gentelellaTimelineItem(
#'        title = "Who Needs Sundance When You’ve Got Crowdfunding?",
#'        url = NULL,
#'        date = "13 hours ago",
#'        author = "Jane Smith",
#'        "Film festivals used to be do-or-die moments for movie makers.
#'        They were where you met the producers that could fund your
#'        project, and if the buyers liked your flick, they’d pay to
#'        Fast-forward and ..."
#'       ),
#'       gentelellaTimelineItem(
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
#' @export
gentelellaTimeline <- function(...) {
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
#' @export
gentelellaTimelineItem <- function(..., title = NULL, url = NULL, date = NULL, author = NULL,
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
#' @param ... slot for quickListItem
#'
#'@examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaBox(
#'      width = 4,
#'      title = "Quick Lists",
#'      menuItems = list(
#'        a(class = "collapse-link", icon("chevron-up")),
#'        list(
#'          a(href = "http://www.google.com", "Test", target = "_blank"),
#'          a(href = "#", "Test2")
#'        ),
#'        a(class = "close-link", icon("close"))
#'      ),
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
quickList <- function(...) {
 shiny::tags$ul(class = "quick-list", ...)
}


#' A quick list item
#'
#' @param icon item icon
#' @param name item name
#'
#' @export
quickListItem <- function(icon, name = NULL) {
 shiny::tags$li(shiny::icon(icon), shiny::tags$a(name))
}




#' A box widget container
#'
#' @param ... slot for any widget
#' @param width widget width
#'
#'@examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaBox(
#'      width = 6,
#'      title = "Box Widget",
#'      menuItems = list(
#'        a(class = "collapse-link", icon("chevron-up")),
#'        list(
#'          a(href = "http://www.google.com", "Test", target = "_blank"),
#'          a(href = "#", "Test2")
#'        ),
#'        a(class = "close-link", icon("close"))
#'      ),
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
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaBox(
#'      width = 3,
#'      title = "user List",
#'      menuItems = list(
#'        a(class = "collapse-link", icon("chevron-up")),
#'        list(
#'          a(href = "http://www.google.com", "Test", target = "_blank"),
#'          a(href = "#", "Test2")
#'        ),
#'        a(class = "close-link", icon("close"))
#'      ),
#'      userList(
#'       userListItem(
#'        user_img = NULL,
#'        user_url = "http:://www.google.com",
#'        title = "user 1",
#'        footer = "2 Sales Today",
#'        "$2300. Agent Avarage Sales."
#'       )
#'      )
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @export
userList <- function(...) {
 shiny::tags$ul(class = "list-unstyled top_profiles scroll-view", ...)
}


#' A user list item
#'
#' @param ... Any content
#' @param user_img User image
#' @param title item title
#' @param footer footer content
#'
#' @export
userListItem <- function(..., user_img = NULL, title = NULL, footer = NULL) {
 shiny::tags$li(
   class = "media_event",
   shiny::tags$a(
     class = "pull-left border-aero profile_thumb",
     if (!is.null(user_img)) {
       shiny::img(href = user_img)
     } else {
       shiny::icon("user", class = "aero")
     }
   ),
   shiny::tags$div(
     class = "media-body",
     shiny::tags$a(class = "title", title),
     shiny::tags$p(...),
     shiny::tags$p(shiny::tags$small(footer))
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
#' @param striped Whether the progress bar is striped or not. FALSE by default.
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaBox(
#'      width = 3,
#'      title = "Progress Bars",
#'      menuItems = list(
#'        a(class = "collapse-link", icon("chevron-up")),
#'        list(
#'          a(href = "http://www.google.com", "Test", target = "_blank"),
#'          a(href = "#", "Test2")
#'        ),
#'        a(class = "close-link", icon("close"))
#'      ),
#'      progressBar(
#'       20,
#'       side = "left",
#'       status = "primary",
#'       striped = FALSE
#'      ),
#'      progressBar(
#'       70,
#'       side = "right",
#'       status = "danger",
#'       striped = TRUE
#'      )
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @export
progressBar <- function(value, side = "left", status = "primary", striped = FALSE, width = 6){

  progressCl <- paste0("progress-bar progress-bar-", status)
  if (striped) progressCl <- paste0(progressCl, " progress-striped")

  shiny::tags$div(
    class = if (side == "left") "progress" else "progress right",
    shiny::tags$div(
      class = progressCl,
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
#'   ui = gentelellaPage(
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
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaAlert(
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
#' @export
gentelellaAlert <- function(..., title = NULL, status = "primary", dismissible = TRUE, width = 3) {

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
      br(),
      ...
    )
  )
}



#' A rating tag
#'
#' @param value value between 0 and 5
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaStars(value = 4)
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
#'
#' @export
gentelellaStars <- function(value) {

  stop_val <- 5 - value

 shiny::tags$p(
   class = "ratings",
   shiny::tags$a(value),
   if (value >= 1) {
     # full stars
     tagList(
       lapply(X = 1:value, FUN = function(i) {
         shiny::tags$a(shiny::tags$span(class = "fa fa-star"))
       }),
       # empty stars
       lapply(X = 1:stop_val, FUN = function(i) {
         shiny::tags$a(shiny::tags$span(class = "fa fa-star-o"))
       })
     )
   } else {
     lapply(X = 1:5, FUN = function(i) {
       shiny::tags$a(shiny::tags$span(class = "fa fa-star-o"))
     })
   }
 )
}




#' An activity list
#'
#' @param ... Slot for activityItem
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    gentelellaBody(
#'     gentelellaBox(
#'      title = "Activity List",
#'      activityList(
#'      lapply(X = 1:3, FUN = function(i) {
#'       activityItem(
#'        title = "Desmond Davison",
#'        img = paste0("https://image.flaticon.com/icons/svg/1087/108783", i,".svg"),
#'        day = 13,
#'        month = "june",
#'        url = "http://www.google.com",
#'        "Raw denim you probably haven't heard of them jean shorts Austin.
#'        Nesciunt tofu stumptown aliqua butcher retro keffiyeh
#'        dreamcatcher synth."
#'       )
#'      })
#'      )
#'     )
#'    )
#'   ),
#'   server <- function(input, output) {}
#'  )
#' }
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
      br(),
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
