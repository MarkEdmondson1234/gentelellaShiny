library(shiny)
library(gentelellaShinyTemplate)
library(googleAuthR)
library(googleAnalyticsR)
library(googleID)
library(dygraphs)
library(zoo)
library(ggplot2)
library(flexdashboard)
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

  output$delta <- renderDataTable({

    session_data()

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

  # output$trend_plot <- renderPlot({
  #
  #   req(trend_data())
  #   trend_data <- trend_data()
  #
  #   ggplot(trend_data, aes(x = date, y = sessions)) + geom_line() + theme_minimal()
  #
  # })

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
      e3.going_well = if(sd[sd$medium == "organic","sessions.d1"] - sd[sd$medium == "organic","sessions.d2"] > 0) TRUE else FALSE
    )
  })

  ## top tiles
  shiny::callModule(updateTileCount, "e1",
                    value = reactive(top_tile_data()$e1.value),
                    change_value = reactive(top_tile_data()$e1.change_value),
                    going_well = reactive(top_tile_data()$e1.going_well),
                    tile_title = " Direct Sessions",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week")

  shiny::callModule(updateTileCount, "e2",
                    value = reactive(top_tile_data()$e2.value),
                    change_value = reactive(top_tile_data()$e2.change_value),
                    going_well = reactive(top_tile_data()$e2.going_well),
                    tile_title = " Referral Sessions",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week")

  shiny::callModule(updateTileCount, "e3",
                    value = reactive(top_tile_data()$e3.value),
                    change_value = reactive(top_tile_data()$e3.change_value),
                    going_well = reactive(top_tile_data()$e3.going_well),
                    tile_title = " Organic Sessions",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week",
                    highlight = "green")

  shiny::callModule(updateTileCount, "e4",
                    value = reactive(top_tile_data()$e1.value),
                    change_value = reactive(top_tile_data()$e1.change_value),
                    going_well = reactive(top_tile_data()$e1.going_well),
                    tile_title = " Direct Sessions",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week")

  shiny::callModule(updateTileCount, "e5",
                    value = reactive(top_tile_data()$e2.value),
                    change_value = reactive(top_tile_data()$e2.change_value),
                    going_well = reactive(top_tile_data()$e2.going_well),
                    tile_title = " Referral Sessions",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week")

  shiny::callModule(updateTileCount, "e6",
                    value = reactive(top_tile_data()$e3.value),
                    change_value = reactive(top_tile_data()$e3.change_value),
                    going_well = reactive(top_tile_data()$e3.going_well),
                    tile_title = " Organic Sessions",
                    width = 2,
                    icon_in = icon("user"),
                    from_text = " From last Week")

}
