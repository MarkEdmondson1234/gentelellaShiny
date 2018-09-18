#' Create a Gentelella dashboard page
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
#'  shiny::shinyApp(
#'   ui = gentelellaPage(
#'    title = "Shiny Gentelella",
#'    navbar = gentelellaNavbar(
#'     navbarItems = gentelellaNotif(
#'      id = "menunotif",
#'      icon = "envelope-o",
#'      status = "primary",
#'      expanded = FALSE,
#'      lapply(X = 1:5, FUN = function(i) {
#'       gentelellaNotifItem(
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
#'    gentelellaSidebarProfile(
#'     name = "Mark",
#'     img = "https://image.flaticon.com/icons/svg/236/236831.svg"
#'    ),
#'     gentelellaSidebarMenu()
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
gentelellaPage <- function(navbar = NULL, sidebar = NULL, body = NULL,
                           footer = NULL, title = NULL, sidebar_collapsed = TRUE,
                           footer_fixed = FALSE){

  pageCl <- if (sidebar_collapsed) {
    if (footer_fixed) "nav-sm footer_fixed" else "nav-sm"
  } else {
    if (footer_fixed) "nav-md footer_fixed" else "nav-md"
  }

    footer_fixed

  shiny::tags$html(
    # Head
    shiny::tags$head(
      shiny::tags$meta(
        charset = "UTF-8",
        content = "text/html",
        `http-equiv` = "Content-Type"
      ),
      shiny::tags$meta(charset = "utf-8"),
      shiny::tags$meta(
        `http-equiv` = "X-UA-Compatible",
        content = "IE=edge"
      ),
      shiny::tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      shiny::tags$title(title),
      shiny::includeCSS(path = "inst/gentelella-1.5.0/custom.min.css")
    ),
    # Body
    addDeps(
      shiny::tags$body(
        class = pageCl,
        shiny::tags$div(
          class = "container body",
          shiny::tags$div(
            class = "main_container",
            sidebar,
            navbar,
            # page content
            body,
            footer,
            shiny::includeScript(path = "inst/smartwizard-3.3.1/wizard.js"),
            shiny::includeScript(path = "inst/gentelella-1.5.0/custom.min.js")
          )
        )
      )
    )
  )
}
