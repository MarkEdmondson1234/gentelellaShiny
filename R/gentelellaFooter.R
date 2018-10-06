#' Create a Gentelella dashboard footer
#'
#' @param leftText Any left text
#' @param rightText Any right text
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @importFrom htmltools withTags
#' @importFrom shiny tagList tags div
gentelellaFooter <- function(leftText = "Mark Edmondson and David Granjon",
                             rightText = "2018"){
  withTags({
    tagList(
      tags$footer(
        div(class = "pull-left", leftText),
        div(class = "pull-right", rightText),
        div(class = "clearfix")
      )
    )
  })
}
