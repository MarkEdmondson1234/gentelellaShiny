#' render a gentellaPage in ui.R
#'
#' Use tagList() on elements you want in the same row
#'
#' @param ... ui elements that will be rendered into a fluidRow per entry
#' @param title_tag Title tag passed to headerBoilerPlate
#' @param site_title Website title passed to sideBarBoilerPlate
#' @param menuItems Sidebar menu items created with \link{sideBarElement}
#' @param footer Footer message
#' @param tracking If not NULL, pass in \link{trackingTags} to enable web analytics
#'
#' @return a function suitable for a ui.R page
#' @import shiny
#' @export
gentelellaPage <- function(...,
                           title_tag = "Gentelella Shiny",
                           site_title = a(class="site_title", icon("paw"), span("Shiny HTML")),
                           menuItems = NULL,
                           footer = NULL,
                           tracking = NULL){

  main_body <- lapply(list(...), function(x) tagList(fluidRow(x), br()))

  htmlTemplate(system.file("templates", "main.html", package = "gentelellaShiny"),
               headerBoilerPlate = headerBoilerPlate(title_tag),
               sideBarBoilerPlate = sideBarBoilerPlate(site_title = site_title,
                                                       menuItems = menuItems),
               navbarBoilerPlate = navbarBoilerPlate(),
               footerBoilerPlate = footerBoilerPlate(footer),
               trackingTags = tracking,
               main_body = main_body)
}
