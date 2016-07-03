#' Progress bars
#' 
#' Progress bars are scaled from 0 to 100
#' 
#' @param values A named vector of values
#' @param display_totals A named vector same length as values showing totals
#' @param small If there should be small bars or big bars
#' @param bar_class The class of the bars themselves
#' 
#' @return a stack of progress bars
#' @export
progress_stack <- function(values,
                           display_totals = NULL,
                           small = TRUE,
                           text_pos = c("top","side"),
                           bar_class = "progress-bar bg-green"){
  text_pos <- match.arg(text_pos) 
  
  if(small){
    prog_class = "progress progress_sm"
  } else {
    prog_class = "progress"
  }
 
  make_progress_top <- function(x){
    val <- values[[x]]
    name <- x
    withTags({
    div(class = "col-md-12 col-sm-12 col-xs-6",
        div(
          p(paste(name, display_totals[[x]])),
          div(class = prog_class, style="width: 76%;",
              div(class=bar_class, role="progressbar", style=paste0("width: ",val,"%"))
              )
          )
        )
    })
  }
  
  make_progress_side <- function(x){
    val <- values[[x]]
    name <- x
    withTags({
      div(class = "widget_summary",
          div(class = "w_left w_25",
              span(name)
          ),
          div(class = "w_center w_55",
              div(class="progress",
                  div(class=bar_class, role = "progressbar", style=paste0("width: ",val,"%"))
                  )
          ),
          div(class = "w_right w_20",
              span(display_totals[[x]])
          ),
          div(class = "clearfix")
      )
    })
  }
  
  switch(text_pos,
         top = lapply(names(values), make_progress_top),
         side = lapply(names(values), make_progress_side)
         )
  
}