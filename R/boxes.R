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
graph_box <- function(..., width = 12, boxtitle = "Impressive Title", subtitle = "sub-title",
                      datepicker = dateRangeInput("datepicker_id", NULL)){

   htmltools::withTags({
    div(class = paste(c("col-md","col-sm","col-xs"), width, sep = "-", collapse = " "),
        div(class = "dashboard_graph",
            div(class = "row x_title",
                div(class = "col-md-6",
                    h3(boxtitle,
                       small(subtitle)
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
#' @param ... elements to put in the box
#' @param width Box width
#' @param height Box height
#' @param box_title Title above
#' @param menuItems A list of other things to appear in top menu
#'
#' @return A box to put elements in
#'
#' @examples
#' if (interactive()) {
#'  library(shiny)
#'  library(gentelellaShiny)
#'  shinyApp(
#'   ui = gentelellaPage(
#'    dashboard_box(
#'      "test",
#'      width = 4,
#'      height = NULL,
#'      box_title = "Box title",
#'      menuItems = list(
#'        a(class = "collapse-link", icon("chevron-up")),
#'        list(
#'          a(href = "http://www.google.com", "Test", target = "_blank"),
#'          a(href = "#", "Test2")
#'        ),
#'        a(class = "close-link", icon("close"))
#'      )
#'    ),
#'    title_tag = "Shiny HTML Template",
#'    site_title = a(class="site_title", icon("eye"), span("Shiny HTML"))
#'   ),
#'   server = function(input, output, session) {}
#'  )
#' }
#'
#' @export
dashboard_box <- function(..., width = 4, height = NULL, box_title = "Box title",
                          menuItems = list(a(class = "collapse-link", icon("chevron-up")))){

  htmltools::withTags({
    div(
      class = paste0(paste(c("col-md","col-sm"), width, sep = "-", collapse = " "), " col-xs-12"),
      div(
        class = "x_panel tile",
        style = paste0("height: ", height, "px;"),
        div(
          class = "x_title",
          h2(box_title),
          ul(
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
                    ul(
                      class = "dropdown-menu",
                      role = "menu",
                      lapply(current_items, li)
                    )
                  )
                )
              } else {
                tags$li(menuItems[[i]])
              }
            })
            )
          ),
          div(class="clearfix")
        ),
        div(
          class = "x_content",
          tagList(...)
        )
      )
    )
  })
}
