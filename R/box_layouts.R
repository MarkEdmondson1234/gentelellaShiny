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
#' Its 320 pixels high by default
#' 
#' @param ... elements to put in the box
#' @param width Width
#' @param box_title Title above
#' @param menuItems A list of other things to appear in top menu
#' 
#' @return A box to put elements in
#' @export
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
              ul(class = "nav navbar-right panel_toolbox",
                 ## add more items to li menu if passed.
                 tagList(lapply(menuItems, li))
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