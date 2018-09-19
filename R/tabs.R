#' Create a gentelella tabSetPanel
#'
#'
#' @param ... Slot for \link{tabPanel}.
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
#'     tabSetPanel(
#'      id = "tabset1",
#'      tabPanel(
#'        tabName = "Home",
#'        active = TRUE,
#'        "Raw denim you probably haven't heard of
#'        them jean shorts Austin. Nesciunt tofu stumptown
#'        aliqua, retro synth master cleanse. Mustache
#'        cliche tempor, williamsburg carles vegan helvetica.
#'        Reprehenderit butcher retro keffiyeh dreamcatcher synth.
#'        Cosby sweater eu banh mi, qui irure terr."
#'      ),
#'      tabPanel(
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
tabSetPanel <- function(..., id, right = FALSE) {

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
#' To be included in a \link{tabSetPanel}
#'
#' @note tabName must be unique even between \link{tabSetPanel} or \link{tabSetPill}
#'
#' @param ... Tab content
#' @param tabName Tab name: it will be also passed as the id argument. Should be unique.
#' @param active Whether the tab is active or not. FALSE bu default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
tabPanel <- function(..., tabName, active = FALSE) {

  tabCl <- if (active) "tab-pane fade active in" else "tab-pane fade"

  shiny::tags$div(
    class = tabCl,
    role = "tabpanel",
    id = tabName,
    `aria-labelledby` = tabName,
    ...
  )
}



#' Create a gentelella tabSetPill
#'
#'
#' @param ... Slot for \link{tabPill}.
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
#'     tabSetPill(
#'      tabPill(
#'        tabName = "Home",
#'        active = TRUE,
#'        "Raw denim you probably haven't heard of
#'        them jean shorts Austin. Nesciunt tofu stumptown
#'        aliqua, retro synth master cleanse. Mustache
#'        cliche tempor, williamsburg carles vegan helvetica.
#'        Reprehenderit butcher retro keffiyeh dreamcatcher synth.
#'        Cosby sweater eu banh mi, qui irure terr."
#'      ),
#'      tabPill(
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
tabSetPill <- function(..., right = FALSE) {

  tabSetCl <- if (right) {
    "nav nav-tabs tabs-right"
  } else {
    "nav nav-tabs tabs-left"
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
      shiny::tags$a(
        href = paste0("#", current_item_name),
        `data-toggle` = "tab",
        `aria-expanded` = if (active == 1) "true" else "false",
        current_item_name
      )
    )
  })

  menuTag <- shiny::tags$ul(
    class = tabSetCl,
    tabMenu
  )

  contentTag <- shiny::tags$div(
    class = "tab-content",
    ...
  )

  mainTag <- if (right) {
    shiny::tagList(
      shiny::tags$div(class = "col-xs-9", contentTag),
      shiny::tags$div(class = "col-xs-3", menuTag)
    )
  } else {
    shiny::tagList(
      shiny::tags$div(class = "col-xs-3", menuTag),
      shiny::tags$div(class = "col-xs-9", contentTag)
    )
  }
  mainTag
}



#' Create a gentelella tabPill
#'
#' To be included in a \link{tabSetPill}
#'
#' @note tabName must be unique even between \link{tabSetPanel} or \link{tabSetPill}
#'
#' @param ... Tab content
#' @param tabName Tab name: it will be also passed as the id argument. Should be unique.
#' @param active Whether the tab is active or not. FALSE bu default.
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
tabPill <- function(..., tabName, active = FALSE) {

  tabCl <- if (active) "tab-pane active" else "tab-pane"

  shiny::tags$div(
    class = tabCl,
    id = tabName,
    ...
  )
}
