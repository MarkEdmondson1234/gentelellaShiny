# gentelellaShiny

[![Travis-CI Build Status](https://travis-ci.org/MarkEdmondson1234/gentelellaShiny.svg?branch=master)](https://travis-ci.org/MarkEdmondson1234/gentelellaShiny)

> This is an R version of the gentelella admin template
[gentelella bootstrap theme](https://github.com/puikinsh/gentelella).

## Install

```r
devtools::install_github("MarkEdmondson1234/gentelellaShiny")
```

## Features

See a gallery for an overview of all working elements

* A unique looking Shiny dashboard
* Dynamic headline tiles
* Custom boxes: closable, collapsible, dropdown menus, ribbons, social boxes, ...
* tabsetpanels or pills
* pieChart widget (easypiechart)
* Timelines, alerts, lists, sidebarProfile, sidebarDate, jumbotrons, ...
* navbar notifications
* custom footer

A demo app is available by running `gentelellaShiny::gentelellaGallery()`

## Set up a basic page

### To use

A special function `gentelellaPage()` has been created that you pass normal ui.R elements to, that will then render each parameter in order to each row of content on the page.

If you want more items to appear in one row, then pass them in a `shiny::tagList`

Example:

#### In ui.R

```r
## to appear in one row
boxRow <- tagList(
  dashboard_box(uiOutput("progress_bar2"), box_title = "Targets"),
  dashboard_box(gaugeOutput("gauge1"), box_title = "Flexdashboard Gauge", menuItems = NULL),
  dashboard_box(p("More interesting stuff"))
)

## each parameter holds UI elements that are rendered per row in order down the content page
gentelellaPage(
  column(width = 12, googleAnalyticsR::authDropdownUI("auth_dropdown")),
  tileCountRow(tileCountUI("e1"), tileCountUI("e2"), tileCountUI("e3"),
               tileCountUI("e4"), tileCountUI("e5"), tileCountUI("e6")),
  graph_box(dygraphOutput("trend_plot"),
            datepicker = dateRangeInput("datepicker_id", NULL, start = Sys.Date() - 300)),
  boxRow
)

```

`gentelellaPage()` also includes some named parameters to set options such as sidebar menu items, title tag and footer images.

```r
## create menu items for sidebar using sidebarElement
menuItems <- list(
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
  ))

gentelellaPage(
  tileCountRow(tileCountUI("e1"), tileCountUI("e2"), tileCountUI("e3"),
               tileCountUI("e4"), tileCountUI("e5"), tileCountUI("e6")),
  
  ## start named parameters:
  menuItems = menuItems,
  title_tag = "Shiny HTML Template",
  site_title = a(class="site_title", icon("eye"), span("Shiny HTML")),
  footer = "Made in Denmark"
)
```

## Set up an advanced custom page

For a lot more control over all the special elments, use the function `gentelellaPageCustom()` created by @DivadNojnarg

```r
 library(shiny)
 library(gentelellaShiny)
 library(shinyWidgets)

shinyApp(
  ui = gentelellaPageCustom(
   title = "Shiny Gentelella",
   navbar = gentelellaNavbar(
    navbarItems = notif(
     id = "menunotif",
     icon = icon("envelope-o"),
     status = "primary",
     expanded = FALSE,
     lapply(X = 1:5, FUN = function(i) {
      notifItem(
       title = "John Doe",
       date = "3 min ago",
       img = paste0("https://image.flaticon.com/icons/svg/163/16382", i,".svg"),
       "Film festivals used to be do-or-die moments
       for movie makers. They were where..."
      )
     })
    )
   ),
   sidebar = gentelellaSidebar(
    uiOutput("profile"),
    sidebarDate(),
    sidebarMenu(
     sidebarItem(
      "Tab 1",
      tabName = "tab1", 
      icon = tags$i(class = "fas fa-chart-bar"), 
      badgeName = "new",
      badgeStatus = "danger"
     ),
     sidebarItem(
      "Tab 2",
      tabName = "tab2", 
      icon = tags$i(class = "fas fa-info")
     )
    )
   ),
   body = gentelellaBody(
    tabItems(
     tabItem(
      tabName = "tab1",
      fluidRow(
       column(
        width = 4,
        align = "center",
        sliderInput(
         "obs",
         "Number of observations:",
         min = 0,
         max = 1000,
         value = 500
        )
      ),
      column(
       width = 8,
       align = "center",
       plotOutput("distPlot")
      )
     )
     ),
     tabItem(
      tabName = "tab2",
       jumbotron(
        title = "Hello, world!",
        "This is a simple hero unit, a simple jumbotron-style
        component for calling extra attention to featured
        content or information."
       )
      )
     )
   ),
   footer = gentelellaFooter()
  ),
  server = function(input, output, session) {
   output$distPlot <- renderPlot({
    hist(rnorm(input$obs))
   })
   
   counter <- reactiveValues(connect = 0)
   
   observeEvent(counter$connect == 0, {
    inputSweetAlert(
      session = session, 
      inputId = "name",
      title = "What's your name ?"
    )
   })
   
   output$profile <- renderUI({
    sidebarProfile(
     name = input$name,
     img = "https://image.flaticon.com/icons/svg/236/236831.svg"
    )
   })
  }
 )

```

## Limitations
Currently sidebar subitems are **not supported**.
