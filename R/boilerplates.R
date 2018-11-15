#' @import shiny
headerBoilerPlate <- function(title_tag = "Gentelella Shiny"){
    # Head
    tags$head(
      tags$meta(
        charset = "UTF-8",
        content = "text/html",
        `http-equiv` = "Content-Type"
      ),
      tags$meta(charset = "utf-8"),
      tags$meta(
        `http-equiv` = "X-UA-Compatible",
        content = "IE=edge"
      ),
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$title(title_tag),
      includeCSS(system.file("gentelella-1.5.0/custom.min.css", package = "gentelellaShiny"))
    )
}

#' @import shiny
sideBarBoilerPlate <-
  function(site_title = a(class="site_title", icon("paw"), span("Shiny HTML")),
           menuItems = list(
             sideBarElement(" Home ",
                            icon = icon("home"),
                            list(a(href="index.html", "Dashboard"),
                                 a(href="index2.html", "Dashboard2"),
                                 a(href="index3.html", "Dashboard3"))
             ),
             sideBarElement(" Contact ",
                            icon = icon("envelope"),
                            list(a(href="http://twitter.com/HoloMarkeD",
                                   HTML(paste(icon("twitter"), "@HoloMarkeD"))),
                                 a(href="http://code.markedmondson.me",
                                   HTML(paste(icon("rss"), " Blog"))),
                                 a(href="https://github.com/MarkEdmondson1234/gentelellaShiny",
                                   HTML(paste(icon("github"), " Github"))))
             ),
             sideBarElement(column(width = 12, googleAuthR::googleAuthUI("auth"),
                                   icon = NULL)
             ))){

    withTags({
      div(class="col-md-3 left_col",
          div(class="left_col scroll-view",
              div(class="navbar nav_title", style="border: 0;",
                  site_title
              ),
              div(class = "clearfix"),
              uiOutput("profile"),
              br(),
              div(id="sidebar-menu", class="main_menu_side hidden-print main_menu",
                  div(class = "menu_section",
                      h3(format(Sys.Date(), format = "%a - %d %B, %Y")),
                      tags$ul(class = "nav side-menu",
                         tagList(lapply(menuItems, tags$li))
                      )
                  )
              )


          ))
    })
  }

#' sideBarLinks
#'
#' Things put in li()
#'
#' Use HTML() wrapped around icon() if neccessary
#' @param element Element.
#' @param icon Icon.
#' @param nested_element Nested items if any.
#' @import shiny
#' @export
sideBarElement <- function(element,
                           icon = NULL,
                           nested_element=NULL
){

  tagList(
    tags$a(icon, element, if(!is.null(nested_element)) span(class="fa fa-chevron-down")),
    tags$ul(class="nav child_menu",
            tagList(lapply(nested_element, tags$li)
            )
    ))

}

#' Top Navbar
#' @import shiny
navbarBoilerPlate <- function(){

  withTags({
    div(class="top_nav",
        div(class="nav_menu",
            tags$nav(
              div(class="nav toggle",
                  a(id="menu_toggle", icon("bars"))
              ),
              uiOutput("profile_nav")

            )
        )
    )

  })

}

#' @import shiny
footerBoilerPlate <- function(message = NULL){

  if(is.null(message)){
    message <- tagList(p('Gentelella Shiny - Bootstrap Admin Template by',
                         a(href="https://colorlib.com", 'Colorlib'),
                         '. Shiny template by ',
                         a(href="http://markedmondson.me", 'Mark Edmondson')))
  }

  js_file <- paste(readLines(system.file("www", "custom.min.js", package = "gentelellaShiny"),
                             warn = FALSE),
                   collapse=" \n")

  HTML(sprintf('
               <!-- footer content -->
               <footer>
               <div class="pull-right">
               %s
               </div>
               <div class="clearfix"></div>
               </footer>
               <!-- /footer content -->
               <!-- Custom Theme Scripts -->
               <script>%s</script>
               ', message, js_file))


}
