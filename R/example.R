#' @export
runExample <- function() {
  appDir <- system.file("demo", "gaDashboard", package = "gentelellaShiny")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  browseURL(system.file("www", "login.html", package = "gentelellaShiny"))
  shiny::runApp(appDir, display.mode = "normal")

}
