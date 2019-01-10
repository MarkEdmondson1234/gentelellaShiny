library(shiny)
library(gentelellaShiny)

lorem_ipsum <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit.
                Sed non risus. Suspendisse lectus tortor, dignissim sit
                amet, adipiscing nec, ultricies sed, dolor. Cras
                elementum ultrices diam. Maecenas ligula massa,
                varius a, semper congue, euismod non, mi."

shinyApp(
  ui = gentelellaPageCustom(
    title = "Shiny Gentelella",
    navbar = gentelellaNavbar(
      navbarItems = notif(
        id = "menunotif",
        icon = icon("envelope-o"),
        status = "danger",
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
      sidebarProfile(
        name = "Mark",
        img = "https://image.flaticon.com/icons/svg/236/236831.svg"
      ),
      sidebarDate(),
      sidebarMenu(
        #title = "test",
        sidebarItem("Boxes", tabName = "boxes", icon = icon("clone"), badgeName = "New"),
        sidebarItem("Tabs", tabName = "tabs", icon = icon("tasks")),
        sidebarItem("Charts", tabName = "charts", icon = icon("bar-chart")),
        sidebarItem("Other Items", tabName = "others", icon = icon("bug"))
      )
    ),
    body = gentelellaBody(
      tabItems(
        tabItem(
          tabName = "boxes",
          fluidRow(
            box(
              width = 4,
              title = "Simple Box",
              subtitle = "Box subtitle",
              closable = TRUE,
              collapsible = TRUE,
              dropdownMenu = list(
                a(href = "http://www.google.com", "Link 1", target = "_blank"),
                a(href = "http://www.google.com", "Link 2", target = "_blank")
              ),
              lorem_ipsum
            ),
            ribbonBox(
              width = 4,
              ribbon_text = "A ribbon",
              ribbon_color = "red",
              title = "Ribbon Box",
              lorem_ipsum
            ),
            socialBox(
              width = 4,
              title = "Social Box",
              url_1 = "https://www.facebook.com",
              url_2 = "https://twitter.com",
              media_1 = "facebook",
              media_2 = "twitter",
              profile_img = "https://image.flaticon.com/icons/svg/17/17004.svg",
              footer = "If you\'ve decided to go in development
              mode and tweak all of this a bit, there are few
              things you should do.",
              socialStats(
                socialStatsItem(value = 123, name = "Articles"),
                socialStatsItem(value = 1234, name = "Followers"),
                socialStatsItem(value = 435, name = "Following")
              )
            ),
            fluidRow(
              contactBox(
                width = 3,
                head_title = "Contact Box",
                main_title = "Nicole Pearson",
                img = "https://image.flaticon.com/icons/svg/145/145859.svg",
                footer_left = "Some text",
                footer_right = pieChart(value = 45, id = "contactChart"),
                quickList(
                  quickListItem(icon = icon("calendar-o"), name = "Settings"),
                  quickListItem(icon = icon("bars"), name = "Subscription")
                )
              )
            ),
            fluidRow(
              valueBox(
                value = 179,
                title = "New Sign ups",
                description = "Lorem ipsum psdea itgum rixt",
                icon = icon("caret-square-o-right")
              ),
              valueBox(
                value = 345,
                title = "New Customers",
                description = "Lorem ipsum psdea itgum rixt",
                icon = icon("comments-o")
              )
            )
          )
        ),
        tabItem(
          tabName = "tabs",
          column(
            width = 6,
            align = "center",
            h3("tabSetPanel"),
            tabSetPanel(
              id = "tabset1",
              tabPanel(
                tabName = "Home",
                active = TRUE,
                "Raw denim you probably haven't heard of
                them jean shorts Austin. Nesciunt tofu stumptown
                aliqua, retro synth master cleanse. Mustache
                cliche tempor, williamsburg carles vegan helvetica.
                Reprehenderit butcher retro keffiyeh dreamcatcher synth.
                Cosby sweater eu banh mi, qui irure terr."
              ),
              tabPanel(
                tabName = "Profile",
                active = FALSE,
                sliderInput(
                  "obs",
                  "Number of observations:",
                  min = 0,
                  max = 1000,
                  value = 500
                ),
                plotOutput("distPlot")
              )
            )
          ),
          column(
            width = 6,
            align = "center",
            h3("tabSetPill"),
            tabSetPill(
              tabPill(
                tabName = "Tab1",
                active = TRUE,
                "Raw denim you probably haven't heard of
                them jean shorts Austin. Nesciunt tofu stumptown
                aliqua, retro synth master cleanse. Mustache
                cliche tempor, williamsburg carles vegan helvetica.
                Reprehenderit butcher retro keffiyeh dreamcatcher synth.
                Cosby sweater eu banh mi, qui irure terr."
              ),
              tabPill(
                tabName = "Tab2",
                active = FALSE,
                radioButtons(
                  "dist",
                  "Distribution type:",
                  c("Normal" = "norm",
                    "Uniform" = "unif",
                    "Log-normal" = "lnorm",
                    "Exponential" = "exp")
                ),
                plotOutput("plot")
              )
            )
          )
        ),
        tabItem(
          tabName = "charts",
          box(
            title = "pieChart",
            "gentelellaShiny allows you to use the easypiechart library.
            See https://github.com/rendro/easy-pie-chart",
            pieChart(id = "chart1", value = 10),
            pieChart(
              id = "chart2",
              value = 20,
              barColor = "#0000FF",
              trackColor = "#FFA500",
              scaleColor = "#dfe0e0",
              scaleLength = 10,
              lineCap = "square",
              lineWidth = 6,
              rotate = 180
            )
          )
        ),
        tabItem(
          tabName = "others",
          fluidRow(
            alert(
              width = 3,
              status = "warning",
              title = "An alert",
              "Best check yo self,
              you're not looking too good."
            ),
            alert(
              width = 3,
              status = "danger",
              title = "An alert",
              "Best check yo self,
              you're not looking too good."
            ),
            alert(
              width = 3,
              status = "success",
              title = "An alert",
              "Best check yo self,
              you're not looking too good."
            ),
            alert(
              width = 3,
              status = "info",
              title = "An alert",
              "Best check yo self,
              you're not looking too good."
            )
          ),
          jumbotron(
            title = "Hello, world!",
            "This is a simple hero unit, a simple jumbotron-style
            component for calling extra attention to featured
            content or information."
          ),
          fluidRow(
            box(
              width = 12,
              title = "Activity List",
              activityList(
                lapply(X = 1:3, FUN = function(i) {
                  activityItem(
                    title = "Desmond Davison",
                    img = paste0("https://image.flaticon.com/icons/svg/1087/108783", i,".svg"),
                    day = 13,
                    month = "june",
                    url = "http://www.google.com",
                    "Raw denim you probably haven't heard of them jean shorts Austin.
                    Nesciunt tofu stumptown aliqua butcher retro keffiyeh
                    dreamcatcher synth."
                  )
                })
              )
            )
          ),
          fluidRow(
            box(
              width = 4,
              title = "Quick Lists",
              quickList(
                quickListItem(icon = icon("calendar-o"), name = "Settings"),
                quickListItem(icon = icon("bars"), name = "Subscription")
              )
            ),
            box(
              width = 4,
              title = "Timeline",
              timeline(
                timelineItem(
                  title = "Who Needs Sundance When You’ve Got Crowdfunding?",
                  url = NULL,
                  date = "13 hours ago",
                  author = "Jane Smith",
                  "Film festivals used to be do-or-die moments for movie makers.
                  They were where you met the producers that could fund your
                  project, and if the buyers liked your flick, they’d pay to
                  Fast-forward and ..."
                ),
                timelineItem(
                  title = "Who needs Money",
                  url = "http:://www.google.com",
                  date = "Today",
                  author = "John Doe",
                  "Nobody need money!",
                  tag = "My tag"
                )
                )
            ),
            box(
              width = 4,
              title = "user List",
              userList(
                userListItem(
                  user_img = "https://image.flaticon.com/icons/svg/145/145862.svg",
                  user_url = "http:://www.google.com",
                  title = "user 1",
                  subtitle = "2 Sales Today",
                  "$2300. Agent Avarage Sales."
                ),
                userListItem(
                  user_img = "https://image.flaticon.com/icons/svg/145/145864.svg",
                  user_url = "http:://www.google.com",
                  title = "user 2",
                  subtitle = "4 Sales Today",
                  "$4600. Agent Avarage Sales."
                )
              )
            )
          ),
          fluidRow(
            tileCountRow(
              lapply(1:4, tileCountElement)
            )
          ),
          fluidRow(
            box(
              title = "Labels",
              label(name = "David", status = "warning", mode = "badge"),
              br(), br(), br(),
              label(name = "Mark", position = "pull-right"),
              label(name = "Isabella", status = "danger", position = "pull-left")
            ),
            box(
              width = 6,
              title = "Progress Bars",
              column(
                width = 6,
                progressBar(
                  20,
                  side = "left",
                  status = "primary",
                  striped = TRUE
                ),
                progressBar(
                  70,
                  side = "right",
                  color = "purple",
                  striped = FALSE
                )
              )
            )
          )
        )
      )
    ),
    footer = gentelellaFooter()
  ),
  server = function(input, output) {
    output$distPlot <- renderPlot({
      hist(rnorm(input$obs))
    })
    output$plot <- renderPlot({
      dist <- switch(
        input$dist,
        norm = rnorm,
        unif = runif,
        lnorm = rlnorm,
        exp = rexp,
        rnorm
      )
      hist(dist(500))
    })
  }
)
