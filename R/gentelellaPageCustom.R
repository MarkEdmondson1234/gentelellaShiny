#' Create a Gentelella dashboard page
#'
#' This lets you customise the Gentelella dashboard to a much higher degree than \link{gentelellaPage}
#'
#' @param navbar Gentelella dashboard navbar.
#' @param sidebar Gentelella dashboard main sidebar.
#' @param body Gentelella dashboard body wrapper.
#' @param footer Gentelella dashboard footer.
#' @param title App title.
#' @param sidebar_collapsed Whether the sidebar is collapsed of not at start. TRUE by default.
#' @param footer_fixed Whether the footer is fixed or not. FALSE by default.
#'
#' @examples
#' if(interactive()){
#'  library(shiny)
#'  library(gentelellaShiny)
#'
#'  shinyApp(
#'   ui = gentelellaPageCustom(
#'    title = "Shiny Gentelella",
#'    navbar = gentelellaNavbar(
#'     navbarItems = notif(
#'      id = "menunotif",
#'      icon = "envelope-o",
#'      status = "primary",
#'      expanded = FALSE,
#'      lapply(X = 1:5, FUN = function(i) {
#'       notifItem(
#'        title = "John Doe",
#'        date = "3 min ago",
#'        img = paste0("https://image.flaticon.com/icons/svg/163/16382", i,".svg"),
#'        "Film festivals used to be do-or-die moments
#'        for movie makers. They were where..."
#'       )
#'      })
#'     )
#'    ),
#'    sidebar = gentelellaSidebar(
#'     sidebarProfile(
#'      name = "Mark",
#'      img = "https://image.flaticon.com/icons/svg/236/236831.svg"
#'     ),
#'     sidebarDate(),
#'     sidebarMenu()
#'    ),
#'    body = gentelellaBody(
#'     fluidRow(
#'      column(
#'       width = 4,
#'       align = "center",
#'       sliderInput(
#'        "obs",
#'        "Number of observations:",
#'        min = 0,
#'        max = 1000,
#'        value = 500
#'       )
#'      ),
#'      column(
#'       width = 8,
#'       align = "center",
#'       plotOutput("distPlot")
#'      )
#'     )
#'    ),
#'    footer = gentelellaFooter()
#'   ),
#'   server = function(input, output) {
#'    output$distPlot <- renderPlot({
#'     hist(rnorm(input$obs))
#'    })
#'   }
#'  )
#' }
#'
#' @author David Granjon, \email{dgranjon@@ymail.com}
#'
#' @export
#' @import shiny
gentelellaPageCustom <- function(navbar = NULL, sidebar = NULL, body = NULL,
                                 footer = NULL, title = NULL, sidebar_collapsed = TRUE,
                                 footer_fixed = FALSE){

  pageCl <- if (sidebar_collapsed) {
    if (footer_fixed) "nav-sm footer_fixed" else "nav-sm"
  } else {
    if (footer_fixed) "nav-md footer_fixed" else "nav-md"
  }

  tags$html(
    # Head
    headerBoilerPlate(title),
    # Body
    addDeps(
      tags$body(
        class = pageCl,
        tags$div(
          class = "container body",
          tags$div(
            class = "main_container",
            sidebar,
            navbar,
            # page content
            body,
            footer,
            includeScript(system.file("easypiechart-2.1.6/easypiechart.min.js",
                                      package = "gentelellaShiny")),
            includeScript(system.file("gentelella-1.5.0/custom.min.js",
                                      package = "gentelellaShiny"))
          )
        )
      )
    )
  )
}
