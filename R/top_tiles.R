#' tileCountRow
#'
#' @param ... tileCountElements
#'
#' @return Tile Count
#' @export
tileCountRow <- function(...){

  tags$div(class = "row tile_count",
        tagList(...)
        )

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
#' @export
tileCountElement <- function(value = 2500,
                             change_value = "4%",
                             going_well = TRUE,
                             tile_title = " Total Users",
                             width = 2,
                             icon_in = icon("user"),
                             from_text = " From last Week",
                             highlight = NULL){
  if(going_well){
    bottom_icon <- tags$i(class = "green", icon("sort-asc"), change_value)
  } else {
    bottom_icon <- tags$i(class = "red", icon("sort-desc"), change_value)
  }

  withTags({
    div(class = paste0("col-md-",width," col-sm-4 col-xs-6 tile_stats_count"),
        span(class = "count_top", icon_in, tile_title),
        div(class = paste("count", highlight), value),
        span(class = "count_bottom", bottom_icon, from_text)
        )

  })

}

#' tileCount UI
#'
#' Shiny Module for use with \link{tileCount}
#'
#' @param id Shiny id
#'
#' @return Shiny UI
#' @export
tileCountUI <- function(id){

  ns <- shiny::NS(id)

  uiOutput(ns("tile_count"))

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
#' @export
updateTileCount <- function(input, output, session,
                            value,
                            change_value,
                            going_well,
                            tile_title = " Total Users",
                            width = 2,
                            icon_in = icon("user"),
                            from_text = " From last Week",
                            highlight = reactive(NULL)){

    ns <- session$ns

    output$tile_count <- renderUI({

      tileCountElement(value = value(),
                       change_value = change_value(),
                       going_well = going_well(),
                       tile_title = tile_title,
                       width = width,
                       icon_in = icon_in,
                       from_text = from_text,
                       highlight = highlight())

    })

}
