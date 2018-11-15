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
#' @importFrom shiny shinyAppFile
gentelellaGallery <- function() {
  if (!requireNamespace(package = "gentelellaShiny"))
    message("Package 'gentelellaShiny' is required to run this function")

  shinyAppFile(
    system.file(
      paste0("example/app.R"),
      package = 'gentelellaShiny',
      mustWork = TRUE
    )
  )

}
