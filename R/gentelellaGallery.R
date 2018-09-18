#' Launch the gentelellaGallery
#'
#' @export
gentelellaGallery <- function() {
  appDir <- system.file("demo", package = "gentelellaShiny")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  options(shiny.port = 1221)
  #browseURL(system.file("www", "login.html", package = "gentelellaShiny"))
  shiny::runApp(appDir, display.mode = "normal")

}
