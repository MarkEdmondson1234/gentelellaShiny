library(shiny)
library(gentelellaShiny)
library(googleAuthR)
library(googleAnalyticsR)
library(googleID)
library(dygraphs)
library(zoo)
library(ggplot2)
library(flexdashboard)
library(d3heatmap)
library(tidyr)
library(dplyr)
library(testthat)

options(shiny.port = 1221)
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile",
                                        "https://www.googleapis.com/auth/analytics.readonly"))
options(googleAuthR.securitycode = "gentelellafkjfs24j234123")
options(googleAuthR.webapp.client_id = "201908948134-cjjs89cffh3k429vi7943ftpk3jg36ed.apps.googleusercontent.com")
options(googleAuthR.webapp.client_secret = "mE7rHl0-iNtzyI1MQia-mg1o")

function(input, output, session){

  access_token <- callModule(googleAuthR::googleAuth, "auth",
                             login_text = "Log in",
                             logout_text = "Log off")

  ga_tables <- reactive({

    req(access_token())


    with_shiny(google_analytics_account_list,
               shiny_access_token = access_token())

  })

  user_data <- reactive({

    req(access_token())
    ## user_data$emails$value
    ## user_data$displayName
    ## user_data$image$url
    with_shiny(get_user_info,
               shiny_access_token = access_token())

  })

  # output$ID <- radarchart::renderChartJSRadar({
  #
  #   # ssd <- session_data()
  #   # browser()
  #   # scores <- data.frame("Label"=ssd$medium,
  #   #                      "Users" = ssd$users.d1,
  #   #                      "Sessions" = ssd$sessions.d1)
  #
  #   scores <- data.frame("Label"=c("Communicator", "Data Wangler", "Programmer",
  #                                  "Technologist",  "Modeller", "Visualizer"),
  #                        "Rich" = c(9, 7, 4, 5, 3, 7),
  #                        "Andy" = c(7, 6, 6, 2, 6, 9),
  #                        "Aimee" = c(6, 5, 8, 4, 7, 6))
  #
  #   radarchart::chartJSRadar(scores, showToolTipLabel=TRUE)
  #
  #
  # })

  output$heatmap <- d3heatmap::renderD3heatmap({

    req(trend_data())
    trend_data <- trend_data()

    trend_data$weekdays <- ordered(weekdays(trend_data$date, abbreviate = TRUE),
                                   c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
    trend_data$weeks <- format(trend_data$date, format = "W%W")

    heatdata <- trend_data %>% select(weekdays, weeks, sessions) %>% spread(weekdays, sessions)
    row.names(heatdata) <- heatdata$weeks
    heatdata <- heatdata %>% select(-weeks) %>% tail

    d3heatmap::d3heatmap(heatdata, FALSE, FALSE,
                         dendrogram = "none", width = 300, height = 200, cexCol = 0.3)

  })
  output$profile <- renderUI({

    req(user_data())

    ud <- user_data()

    profile_box(ud$displayName, ud$image$url)


  })

  output$profile_nav <- renderUI({

    req(user_data())

    ud <- user_data()

    profile_nav(ud$displayName, ud$image$url, list(tags$li(tags$a(href="javascript:;", " Profile")),
                                                   tags$li(
                                                     tags$a(href="javascript:;", " Settings")
                                                   ),
                                                   tags$li(
                                                     tags$a(href="javascript:;", "Help")
                                                   ),
                                                   tags$li(
                                                     tags$a(href="/", tags$i(class="fa fa-sign-out pull-right"), "Log out"
                                                     ))))


  })

  selected_id <- callModule(authDropdown, "auth_dropdown", ga_tables)

  session_data <- reactive({

    req(access_token())
    req(selected_id())

    s1 <- Sys.Date() - 60
    e1 <- Sys.Date() - 30
    s2 <- Sys.Date() - 29
    e2 <- Sys.Date() - 1

    with_shiny(
      google_analytics_4,
      viewId = selected_id(),
      date_range = c(s1, e1, s2, e2),
      metrics = c("sessions","users"),
      dimensions = "medium",
      order = order_type("sessions", "DESCENDING", "DELTA"),
      shiny_access_token = access_token()
    )

  })

  ## trend plot

  trend_data <- reactive({

    req(access_token())
    req(input$datepicker_id)

    dates <- input$datepicker_id

    with_shiny(
      google_analytics_4,
      viewId = selected_id(),
      date_range = dates,
      metrics = c("sessions"),
      dimensions = "date",
      shiny_access_token = access_token()
    )

  })

  output$trend_plot <- renderDygraph({

    req(trend_data())
    trend_data <- trend_data()

    zz <- zoo(trend_data$sessions, order.by = trend_data$date)
    dygraph(zz, ylab = "sessions", main = "Sessions Trend")

  })

  ## Gauges

  output$gauge1 <- flexdashboard::renderGauge({

    req(session_data())
    ssd <- session_data()

    flexdashboard::gauge(ssd$sessions.d1[1], min = 0, max = ssd$sessions.d1[1] * 1.3,
                         sectors = gaugeSectors(c(ssd$sessions.d2[1], ssd$sessions.d1[1] * 1.3),
                                                c(10, ssd$sessions.d2[1]),
                                                c(0,10)))

  })

  ## progress bars

  output$progress_bar <- renderUI({

    req(session_data())
    sd <- session_data()

    values <- sd$sessions.d1
    names(values) <- sd$medium
    progress_stack(values,
                   display_totals = values)

  })

  output$progress_bar2 <- renderUI({
    validate(
      need(session_data(), "Fetching data...")
    )

    sd <- session_data()

    ## puts them on an index of 100 = maximum
    values <- round(sd$users.d1 * (100 / max(sd$users.d1)))
    names(values) <- sd$medium
    progress_stack(values,
                   small = FALSE,
                   text_pos = "top")

  })

  ### top tiles

  top_tile_data <- reactive({

    sd <- session_data()

    list(
      e1.value = sd[sd$medium == "(none)","sessions.d1"],
      e1.change_value = sd[sd$medium == "(none)","sessions.d1"] - sd[sd$medium == "(none)","sessions.d2"],
      e1.going_well = if(sd[sd$medium == "(none)","sessions.d1"] - sd[sd$medium == "(none)","sessions.d2"] > 0) TRUE else FALSE,
      e2.value = sd[sd$medium == "referral","sessions.d1"],
      e2.change_value = sd[sd$medium == "referral","sessions.d1"] - sd[sd$medium == "referral","sessions.d2"],
      e2.going_well = if(sd[sd$medium == "referral","sessions.d1"] - sd[sd$medium == "referral","sessions.d2"] > 0) TRUE else FALSE,
      e3.value = sd[sd$medium == "organic","sessions.d1"],
      e3.change_value = sd[sd$medium == "organic","sessions.d1"] - sd[sd$medium == "organic","sessions.d2"],
      e3.going_well = if(sd[sd$medium == "organic","sessions.d1"] - sd[sd$medium == "organic","sessions.d2"] > 0) TRUE else FALSE,
      e4.value = sd[sd$medium == "(none)","users.d1"],
      e4.change_value = sd[sd$medium == "(none)","users.d1"] - sd[sd$medium == "(none)","users.d2"],
      e4.going_well = if(sd[sd$medium == "(none)","users.d1"] - sd[sd$medium == "(none)","users.d2"] > 0) TRUE else FALSE,
      e5.value = sd[sd$medium == "referral","users.d1"],
      e5.change_value = sd[sd$medium == "referral","users.d1"] - sd[sd$medium == "referral","users.d2"],
      e5.going_well = if(sd[sd$medium == "referral","users.d1"] - sd[sd$medium == "referral","users.d2"] > 0) TRUE else FALSE,
      e6.value = sd[sd$medium == "organic","users.d1"],
      e6.change_value = sd[sd$medium == "organic","users.d1"] - sd[sd$medium == "organic","users.d2"],
      e6.going_well = if(sd[sd$medium == "organic","users.d1"] - sd[sd$medium == "organic","users.d2"] > 0) TRUE else FALSE
    )
  })

  ## top tiles
  shiny::callModule(updateTileCount, "e1",
                    value = reactive(top_tile_data()$e1.value),
                    change_value = reactive(top_tile_data()$e1.change_value),
                    going_well = reactive(top_tile_data()$e1.going_well),
                    tile_title = " Direct Sessions",
                    width = 2,
                    icon_in = icon("eye"),
                    from_text = " From last Week")

  shiny::callModule(updateTileCount, "e2",
                    value = reactive(top_tile_data()$e2.value),
                    change_value = reactive(top_tile_data()$e2.change_value),
                    going_well = reactive(top_tile_data()$e2.going_well),
                    tile_title = " Referral Sessions",
                    width = 2,
                    icon_in = icon("eye"),
                    from_text = " From last Week")

  shiny::callModule(updateTileCount, "e3",
                    value = reactive(top_tile_data()$e3.value),
                    change_value = reactive(top_tile_data()$e3.change_value),
                    going_well = reactive(top_tile_data()$e3.going_well),
                    tile_title = " Organic Sessions",
                    width = 2,
                    icon_in = icon("eye"),
                    from_text = " From last Week",
                    highlight = reactive("green"))

  shiny::callModule(updateTileCount, "e4",
                    value = reactive(top_tile_data()$e4.value),
                    change_value = reactive(top_tile_data()$e4.change_value),
                    going_well = reactive(top_tile_data()$e4.going_well),
                    tile_title = " Direct Users",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week")

  shiny::callModule(updateTileCount, "e5",
                    value = reactive(top_tile_data()$e5.value),
                    change_value = reactive(top_tile_data()$e5.change_value),
                    going_well = reactive(top_tile_data()$e5.going_well),
                    tile_title = " Referral Users",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week")

  shiny::callModule(updateTileCount, "e6",
                    value = reactive(top_tile_data()$e6.value),
                    change_value = reactive(top_tile_data()$e6.change_value),
                    going_well = reactive(top_tile_data()$e6.going_well),
                    tile_title = " Organic Users",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week")

}
