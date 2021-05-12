library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(zoo)
library(data.table)
library(leaflet)
library(scales)
library(plotly)
library(streamgraph)

options(stringsAsFactors = FALSE)

source("R/data_prep.R")

source("R/about_text.R")


### UI ###

# get a vector of issue tags
issue_tags <- sort(unique(unlist(str_split(ccc$issues, "; "))))


header <- dashboardHeader(title = "CCC Data Dashboard", titleWidth = 400)

sidebar <- dashboardSidebar(

  width = 400,

  sidebarMenu(
    menuItem("Map View", tabName = "map", icon = icon("map")),
    menuItem("Summary Plots", tabName = "plots", icon = icon("dashboard")),
    menuItem("About", tabName = "about", icon = icon("address-card"))
  ),

  fluidRow(column(width = 12,

    br(),

    br(),

    selectInput("time_selector", "Filter events by...",
                width = 375,
                choices = c("Date range", "Year"),
                selected = "Date range"),

    uiOutput("time_controls"),

    pickerInput(inputId = "issues",
                label = "Filter by political issue", 
                width = 350,
                choices = issue_tags,
                selected = issue_tags,
                options = list(`actions-box` = TRUE, size = 5), 
                multiple = TRUE),
    
    awesomeCheckboxGroup(inputId = "valence",
                         label = "Filter by political valence", 
                         choices = list("left wing" = 1, "right wing" = 2, "other" = 0),
                         selected = c(0,1,2),
                         inline = TRUE),

    awesomeCheckboxGroup(inputId = "violence",
                         label = "Only show events with...", 
                         choices = list("arrests" = "arrests_any",
                                        "property damage" = "property_damage_any",
                                        "protester injuries" = "injuries_crowd_any",
                                        "police injuries" = "injuries_police_any"),
                         inline = TRUE)

  ))
  
)

body <- dashboardBody(

  tabItems(

    tabItem(tabName = "map",

      fluidRow(

        leafletOutput("ccc_map", width = "100%", height = "500px")

      ),

      fluidRow(

        plotOutput("daily_event_tally_ribbon", width = "100%", height = "100px")

      )

    ),

    tabItem(tabName = "plots",

      fluidRow(
        column(width = 4,
          box(height = 200, background = "black", width = "100%", valueBoxOutput("event_tally"))
        ),
        column(width = 8,
          box(height = 200, background = "black", title = "Daily tallies of events", width = "100%",
            plotlyOutput("daily_event_tally_plot", height = "125px")
          )
        )
      ),

      fluidRow(
        column(width = 4,
          box(height = 200, background = "green", width = "100%", valueBoxOutput("participant_tally"))
        ),
        column(width = 8,
          box(height = 200, background = "green", title = "Daily tallies of participants (using mid-range estimates)", width = "100%",
            plotlyOutput("daily_participant_tally_plot", height = "125px")
          )
        )
      ),

      fluidRow(
        column(width = 4),
        column(width = 8,
          box(height = 300, background = "aqua", title = "Monthly tallies of events by issues (duplication allowed)", width = "100%",
            streamgraphOutput("issue_streamgraph", height = "250px", width = "100%")
          )
        )
      )

    ),

    tabItem(tabName = "about",

      formatted_about_text

    )

  )

)

ui <- dashboardPage(header, sidebar, body, skin = "black")


## SERVER ##

server <- function(input, output, session) {

  # generate conditional set of widgets for selecting time frame
  output$time_controls <- renderUI({

    switch(input$time_selector,

      "Date range" = dateRangeInput("daterange", label = "Choose a date range",
                                    width = 375,
                                    start = Sys.Date() - 366, end = Sys.Date() - 1,
                                    min = "2017-01-01", max = Sys.Date() - 1,
                                    format = "MM d, yyyy"),

      "Year" = selectInput("year", label = "Pick a year",
                           width = 375,
                           choices = rev(seq(2017,2021)),
                           selected = 2021)

    )

  })

  # generate filtered and tallied data based on user selections
  ccc_munged_list <- reactive({

    dat <- ccc

    # filter for political valence
    dat <- filter(dat, valence %in% as.numeric(input$valence))

    # filter for user-selected forms of violence
    for(j in input$violence) {

      dat <- dat[dat[,j] == 1,]

    }


    # filter for issue tags, defaulting to 'other/unknown' if none is selected
    if(length(input$issues) > 0) {

      dat <- as.data.table(dat)[issues %like% paste(input$issues, collapse = "|")]

    } else {

      dat <- as.data.table(dat)[issues %like% "other/unknown"]

    }

    # now filter for time frame, conditioning on status of input$time_selector
    if(input$time_selector == "Year") {

      req(input$year)

      dat <- filter(dat, lubridate::year(date) == input$year)

      grid <- data.frame(date = seq(date(sprintf("%s-01-01", input$year)), date(sprintf("%s-12-31", input$year)), by = "day"))

      tally_events <- dat %>%
        count(date) %>%
        left_join(grid, .) %>%
        mutate(count = ifelse(is.na(n), 0, n))

      tally_participants <- dat %>%
        group_by(date) %>%
        summarize(n = sum(size_mean, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(grid, .) %>%
        mutate(count = ifelse(is.na(n), 0, n))

    } else {

      req(input$daterange)

      dat <- filter(dat, date >= input$daterange[1] & date <= input$daterange[2])

      grid <- data.frame(date = seq(date(input$daterange[1]), date(input$daterange[2]), by = "day"))

      tally_events <- dat %>%
        count(date) %>%
        left_join(grid, .) %>%
        mutate(count = ifelse(is.na(n), 0, n))

      tally_participants <- dat %>%
        group_by(date) %>%
        summarize(n = sum(size_mean, na.rm = TRUE)) %>%
        ungroup() %>%
        left_join(grid, .) %>%
        mutate(count = ifelse(is.na(n), 0, n))
      
    }

    dat_list <- list("events" = dat, "event tally" = tally_events, "participant tally" = tally_participants)

    return(dat_list)

  })

  output$ccc_map <- renderLeaflet({

    req(input$issues)

    addLegendCustom <- function(map, colors, labels, sizes, opacity = 1, position = "topright", title = NULL){

      colorAdditions <- paste0(colors, "; border-radius: 50%; width:", sizes * 2, "px; height:", sizes * 2, "px")
      labelAdditions <- paste0("<div style='display: inline-block;height: ", sizes, "px;margin-top: 4px;line-height: ", sizes, "px;'>", labels, "</div>")
    
      return(addLegend(map, title = title, colors = colorAdditions, labels = labelAdditions, opacity = opacity, position = position))

    }

    leaflet() %>%
#     addProviderTiles("CartoDB.Positron") %>%
      addProviderTiles("Stamen.TonerLite") %>%
      # set view to geographic center of the united states
      setView(lat = 39.5, lng = -98.35, zoom = 4) %>%
      addCircleMarkers(data = ccc_munged_list()[['events']],
                       lat = ~lat, lng = ~lon,
                       radius = ~marker_radius, stroke = FALSE, fillColor = ~marker_color, opacity = 1/2, group = "circles",
                       popup = ~marker_label) %>%
      addLegendCustom(title = "Crowd size",
                      colors = c("gray", rep("#cc5500", 4)),
                      labels = c("unknown", "10s", "100s", "1,000s", "10,000s"),
                      sizes = c(5,5,10,15,20),
                      opacity = 1/2)

  })

  output$daily_event_tally_ribbon <- renderPlot({

    req(input$issues)

    ccc_munged_list()[['event tally']] %>%
      ggplot(aes(date, count)) +
        geom_col(fill = "gray60") +
        theme_minimal() +
        scale_y_continuous(position = "left", labels = comma) +
        scale_x_date(date_labels = "%b %d, %Y") +
        labs(caption = "daily event count") +
        theme(axis.title.x = element_blank(),
              axis.title.y = element_blank(),
              plot.caption = element_text(face = "bold", hjust = 0.5, size = 12))

  })

  output$event_tally <- renderValueBox({

    req(input$issues)

    box_sum = sum(ccc_munged_list()[['event tally']][,'count'], na.rm = TRUE)

    valueBox(value = format(box_sum, big.mark = ","),
             subtitle = "Events",
             color = "black")

  })


  output$participant_tally <- renderValueBox({

    req(input$issues)

    box_sum = sum(ccc_munged_list()[['participant tally']][,'count'], na.rm = TRUE)

    valueBox(value = format(box_sum, big.mark = ","),
             subtitle = "Participants",
             color = "green")

  })

  output$daily_event_tally_plot <- renderPlotly({

    req(input$issues)

    p <- ccc_munged_list()[['event tally']] %>%
      ggplot(aes(date, count)) +
        geom_col() +
        theme_minimal() +
        scale_y_continuous(position = "left", labels = comma) +
        scale_x_date(date_labels = "%b %d, %Y") +
        theme(title = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())

    ggplotly(p)

  })

  output$daily_participant_tally_plot <- renderPlotly({

    req(input$issues)

    p <- ccc_munged_list()[['participant tally']] %>%
      ggplot(aes(date, count)) +
        geom_col() +
        theme_minimal() +
        scale_y_continuous(position = "left", labels = comma) +
        scale_x_date(date_labels = "%b %d, %Y") +
        theme(title = element_blank(),
              axis.title.x = element_blank(),
              axis.title.y = element_blank())

    ggplotly(p)

  })

  output$issue_streamgraph <- renderStreamgraph({ 
    
    req(input$issues)

    dat <- ccc_munged_list()[['events']]

    # use data.table to reduce the db to rows with one or more of those issue tags
    issue_shares <- as.data.table(dat)[issues %like% paste(input$issues, collapse = "|")]

    # cut out any additional rows with missing data. in this
    # case, microbenchmark::microbenchmark indicates that dplyr is faster
    issue_shares <- filter(issue_shares, !is.na(date))

    # make a table with dummy variables for each issue tag for each event
    z <- setNames(map_dfc(input$issues, ~as.numeric(grepl(., issue_shares$issues))), input$issues)

    # bind that table of dummy variables with the date col from the original
    issue_shares <- bind_cols(transmute(issue_shares, yrmo = zoo::as.yearmon(date)), z)

    # pivot to tidy long format to allow easy tallying with multi-counting for events that have more 
    # than one issue tag attached
    issue_shares <- pivot_longer(issue_shares, -yrmo, names_to = "issue", values_to = "status")

    # use data.table for fast computation of monthly sums; here, data.table blows dplyr away
    issue_shares <- as.data.table(issue_shares)[, .(tally = sum(status)), by = .(yrmo, issue)]

    # now make the streamgraph with offset = 'zero' so we get a y-axis with a lower bound at 0
    streamgraph(issue_shares, "issue", "tally", "yrmo", interactive = TRUE, offset = "zero") %>%
      sg_axis_x(tick_interval = 1, tick_units = "month", "%b")

 })

}

## RUN ###

shinyApp(ui, server)
