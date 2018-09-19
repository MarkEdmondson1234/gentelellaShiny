#' Launch the gentelellaGallery
#'
#' @examples
#' if (interactive()) {
#'
#'  gentelellaGallery()
#'
#' }
#'
#' @export
gentelellaGallery <- function() {
  if (!requireNamespace(package = "gentelellaShiny"))
    message("Package 'gentelellaShiny' is required to run this function")

  options(shiny.port = 1221)
  #browseURL(system.file("www", "login.html", package = "gentelellaShiny"))
  shiny::shinyAppFile(
    system.file(
      paste0("example/app.R"),
      package = 'gentelellaShiny',
      mustWork = TRUE
    )
  )

}
