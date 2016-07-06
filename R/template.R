#' Tracking tags for your app
#'
#' @param tag_template Tracking tag template
#' @param tag_code If GA, the UA code.  If GTM, the GTM code.
#'
#' tag_code depends on what tag_template is:
#'
#' \itemize{
#'   \item ga: The UA code (UA-xxxxxx-x)
#'   \item gtm The GTM code (GTM-xxxxx)
#'   \item freeform The JS code
#'  }
#' If you select a tag template, you can put UA-xxxx-x as tag_code
#'
#' @return JavaScript for tracking
#' @export
trackingTags <- function(tag_template = c("ga","gtm","freeform"),
                         tag_code){

  tag_template <- match.arg(tag_template)

  out <- switch(tag_template,
                ga = sprintf("<script>
  (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
                             (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
                             m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
                             })(window,document,'script','https://www.google-analytics.com/analytics.js','ga');

                             ga('create', '%s', 'auto');
                             ga('send', 'pageview');

                             </script>", tag_code),
                gtm = sprintf("<!-- Google Tag Manager -->
<noscript><iframe src=\"//www.googletagmanager.com/ns.html?id=%s\"
                height=\"0\" width=\"0\" style=\"display:none;visibility:hidden\"></iframe></noscript>
                <script>(function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
                new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
                j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
                '//www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
                })(window,document,'script','dataLayer','%s');</script>
                <!-- End Google Tag Manager -->", tag_code, tag_code),
                freeform = tag_code



                )

  HTML(out)

}


#' @import shiny
headerBoilerPlate <- function(title_tag = "Shiny HTML"){

  css_file <- paste(readLines(system.file("www", "custom.min.css", package = "gentelellaShiny"),
                              warn = FALSE),
                    collapse=" \n")

  HTML(sprintf('
    <meta http-equiv="Content-Type" content="text/html; charset=UTF-8">
    <!-- Meta, title, CSS, favicons, etc. -->
       <meta charset="utf-8">
       <meta http-equiv="X-UA-Compatible" content="IE=edge">
       <meta name="viewport" content="width=device-width, initial-scale=1">

       <title>%s</title>
       <!-- Custom Theme Style -->
       <style>%s</style>', title_tag, css_file))

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
                ul(class = "nav side-menu",
                   tagList(lapply(menuItems, li))
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
            nav(
              div(class="nav toggle",
                  a(id="menu_toggle", icon("bars"))
              ),
              uiOutput("profile_nav")

            )
        )
    )

  })

}
