library(shiny)
library(gentelellaShiny)
library(googleAnalyticsR)
library(dygraphs)
library(flexdashboard)
# library(radarchart)

options(shiny.port = 1221)
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile",
                                        "https://www.googleapis.com/auth/analytics.readonly"))
options(googleAuthR.securitycode = "gentelellafkjfs24j234123")
options(googleAuthR.webapp.client_id = "201908948134-cjjs89cffh3k429vi7943ftpk3jg36ed.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "mE7rHl0-iNtzyI1MQia-mg1o")


boxRow <- tagList(
  dashboard_box(uiOutput("progress_bar2"), box_title = "Targets"),
  dashboard_box(gaugeOutput("gauge1"), p("We are doing great!"),
                box_title = "Flexdashboard Gauge", menuItems = NULL),
  # dashboard_box(  chartJSRadarOutput("ID"))
  dashboard_box(column(width = 12,
                       d3heatmap::d3heatmapOutput("heatmap", width = 300, height = 250)
                       ),
                box_title = "Heatmap")
)

menuItems <- list(
  sideBarElement(" Resources ",
                 icon = icon("book"),
                 list(a(href="http://code.markedmondson.me/gentelellaShiny/",
                        HTML(paste(icon("github"), "GentellaShiny"))),
                      a(href="http://code.markedmondson.me/googleAnalyticsR",
                        HTML(paste(icon("line-chart"), "googleAnalyticsR")))
                      )
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
  column(width = 12, googleAnalyticsR::authDropdownUI("auth_dropdown", inColumns = TRUE)),
  tileCountRow(tileCountUI("e1"),
               tileCountUI("e2"),
               tileCountUI("e3"),
               tileCountUI("e4"),
               tileCountUI("e5"),
               tileCountUI("e6")
               ),
  graph_box(dygraphOutput("trend_plot"),
            datepicker = dateRangeInput("datepicker_id", NULL, start = Sys.Date() - 300)),
  boxRow,
  menuItems = menuItems,
  title_tag = "Shiny HTML Template",
  site_title = a(class="site_title", icon("eye"), span("Shiny HTML"))
)
