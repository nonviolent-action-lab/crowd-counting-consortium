formatted_about_text <- 

      div(p("This dashboard presents data produced by the ",
               a("Crowd Counting Consortium", href = "https://sites.google.com/view/crowdcountingconsortium/home", target = "_blank"),
            " (CCC). CCC uses publicly available information to make data on political crowds in the United States,
             including marches, protests, strikes, demonstrations, riots, and other actions. CCC publishes its data via a series
             of monthly Google Sheets (",
               a("here", href = "https://sites.google.com/view/crowdcountingconsortium/view-download-the-data", target = "_blank", .noWS = "outside"),
            "). The dashboard runs on a compiled and cleaned version of those data that you can find in ",
              a("this GitHub repository", href = "https://github.com/nonviolent-action-lab/crowd-counting-consortium", target = "_blank", .noWS = "outside"),
            "."),
          p("Some notes on the data and dashboard:",
            tags$ul(
               tags$li("As the repository notes, while the raw data are updated on a rolling basis, there is inevitably some lag 
                       between the appearance of a news story about an event or a public submission to CCC on the one hand and 
                       the addition of a complete record to CCC's Google Sheets on the other. CCC strives to keep that interval 
                       as short as possible, but the project operates on a shoestring budget, and periods of higher protest activity 
                       usually make for longer delays."),
               tags$li("Events have been geolocated to the level of the city/town using the Google Maps Geolocation API and then
                       jittered slightly to make it easier to distinguish them when you zoom in on the map. Although the
                       database includes more detailed information on some events' locations, the location of points within
                       cities/towns on the map here is not meaningful."),
               tags$li("Events get tagged with as many issues as apply. In the dashboard, filtering by political issue uses 
                       'or' logic, so events tagged with any one of the selected issues are included in the results. In the
                       stream graph of issues on the 'Summary Plots' tab, it's the tags that are counted, not the events. So, 
                       for example, an event tagged with 'racism' and 'policing' adds 1 to that month's tally for both issues.")
            )
          ),
          p("The CCC emerged out of a collaborative effort by ",
               a("Jeremy Pressman", href = "https://jeremy-pressman.uconn.edu/", target = "_blank"),
            " and ",
               a("Erica Chenoweth", href = "https://www.ericachenoweth.com/", target = "_blank"),
            " to provide an accurate estimate of the number of people who participated in the Women's March on Washington
             (and its affiliated Sister Marchers worldwide) on January 21st, 2017. Several of their colleagues expressed an interest 
             in conducting similar kinds of efforts. Upon recognizing the growing public interest in up-to-date information on
             crowds---and in response to requests to continue the effort beyond the Women's March---they and their volunteer 
             colleagues established the CCC.  On April 22, 2017, the CCC began to collaborate with ",
               a("Count Love", href = "https://countlove.org/", target = "_blank", .noWS = "outside"),
             ", another volunteer group that developed a webcrawler that captures events data from local newspaper and television
              sites on a daily basis."),
          p("CCC collects these data in the public interest and to further scholarly research. CCC collaborates with Count Love but 
             is not formally affiliated with any other efforts to collect data on political crowds. Anyone who wishes
             to conduct research using the data is fully responsible for any necessary contact with their own Institutional
             Review Boards. If you wish to use these data, please include a citation to the \"Crowd Counting Consortium.\""),
          p("Read more about the origins of the project ",
               a("here", href = "https://www.theatlantic.com/technology/archive/2017/01/womens-march-protest-count/514166/", target = "_blank",  .noWS = "outside"),
            "."),
          p("Submit a record or propose a correction ",
               a("here", href = "https://sites.google.com/view/crowdcountingconsortium/submit-a-record", target = "_blank", .noWS = "outside"),
            "."),
          p("Follow us on Twitter at ",
               a("@crowdcounting", href = "https://twitter.com/crowdcounting", target = "_blank", .noWS = "outside"), "."),
          p("Email us at ",
               a("crowdcountingconsortium@gmail.com", href = "mailto:crowdcountingconsortium@gmail.com", target = "_blank", .noWS = "outside"), "."),
          br(),
          p(strong("Dashboard Designer:"), a("Jay Ulfelder", href = "https://github.com/ulfelder", target = "_blank")), 
          p(strong("R Packages:"), "shiny, shinydashboard, shinyWidgets, tidyverse, lubridate, zoo, data.table, leaflet, scales, plotly, streamgraph"))