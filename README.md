# gentelellaShiny

[![Travis-CI Build Status](https://travis-ci.org/MarkEdmondson1234/gentelellaShiny.svg?branch=master)](https://travis-ci.org/MarkEdmondson1234/gentelellaShiny)

This is an R [Shiny HTML Template](http://shiny.rstudio.com/articles/templates.html) version of the [gentelella bootstrap theme](https://github.com/puikinsh/gentelella).

## Features

* A unique looking Shiny dashboard
* Login authentication page (requires [`googleAuthR`](https://github.com/MarkEdmondson1234/googleAuthR))
* Profile name and picture pulled from G+ (requires [`googleID`](https://github.com/MarkEdmondson1234/googleID))
* Dynamic headline tiles
* Dynamic progress bar visualisation
* Custom box for plots including date picker in header
* Custom collapsible box for dashboard elements

A demo app for viewing GoogleAnalytics data is available by running `gentelellaShiny::runExample()`

## Screenshot after logging in

The demo uses `googleAnalyticsR` to download data, so won't look like much if you login with an account with no GA :)  Replace with your own data source and everything else will work though. 

![](https://github.com/MarkEdmondson1234/gentelellaShiny_nopackage/raw/master/gentellelaShinydemo.png)

## Install

```r
devtools::install_github("MarkEdmondson1234/gentelellaShiny")
```

For logins you also need `googleID`

```r
devtools::install_github("MarkEdmondson1234/googleID")
```

For the demo to work it uses `googleAnalyticsR`

```r
install.packages("googleAnalyticsR")
```

## Elements


### Top tiles

A tile to show headline metrics

![](https://github.com/MarkEdmondson1234/gentelellaShiny_nopackage/raw/master/top_tiles.png)

### Plot Box

A box with datepicker for plots

![](https://github.com/MarkEdmondson1234/gentelellaShiny_nopackage/blob/master/plot_box.png)

### Collapsible box

A box to display other dashboard content

![](https://github.com/MarkEdmondson1234/gentelellaShiny_nopackage/blob/master/dash_box.png)


### Progress bars

For showing things like performance against target

![](https://github.com/MarkEdmondson1234/gentelellaShiny_nopackage/blob/master/progress_bars.png)

### Other Shiny elements

Any other Shiny UI elements can also be used, including `htmlwidgets`.  A couple of examples are shown below:

#### Google Analytics dropdown

From `googleAnalyticsR`, a UI to selet GA view:

![](https://github.com/MarkEdmondson1234/gentelellaShiny_nopackage/blob/master/ga_dropdown.png)

#### Gauge

From `flexdashboards`:

![](https://github.com/MarkEdmondson1234/gentelellaShiny_nopackage/blob/master/guage_demo.png)

## To use

A special function `gentelellaPage()` has been created that you pass normal ui.R elements to, that will then render each parameter in order to each row of content on the page.

If you want more items to appear in one row, then pass them in a `shiny::tagList`

Example:

### In ui.R

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

There is no support for different layouts other than the defaults in `gentelellaPage()`, in that case use `shiny::htmlTemplate` directly and edit `index.html` to include your R code blocks in `{{ brackets }}`

### Example files

A demo app for viewing GoogleAnalytics data is available by running `runExample()`.  

It will start up a login page and a (logged out) Shiny dashboard.  Use the login page or the login button on the Shiny app to start. 

In production the Shiny app would be running on a server and user's start at the login page.

### Login page

`googleAuthUI` creates a login URL that includes a security code.  

Set the security code to be static (default it changes each launch of `googleAuthR`) via options, e.g. `options(googleAuthR.securitycode = "gentelellXXXXXXX")` in the top of `server.R`

Generate the login URL with the security code, and use as the login link in the `login.html` file.

![](https://github.com/MarkEdmondson1234/gentelellaShiny_nopackage/blob/master/googleLogin.png)

The `login.html` file can then be used for logins, with the Shiny app holding logic to not show content without logging in (e.g. `req(auth())`)
