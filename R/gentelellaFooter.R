#' Create a Gentelella dashboard footer
#'
#' @param leftText Any left text
#' @param rightText Any right text
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
gentelellaFooter <- function(leftText = "Mark Edmondson and David Granjon",
                             rightText = "2018"){
  htmltools::withTags({
    shiny::tagList(
      shiny::tags$footer(
        shiny::div(class = "pull-left", leftText),
        shiny::div(class = "pull-right", rightText),
        shiny::div(class = "clearfix")
      )
    )
  })
}
