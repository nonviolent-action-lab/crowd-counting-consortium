require(lubridate)
require(dplyr)

edge_date <- Sys.Date() - 1

ccc <- read.csv("c:/users/ulfel/documents/nval/ccc/data_clean/ccc_compiled.csv") %>%
  filter(date >= "2023-10-07" & date <= edge_date) %>%
  filter(grepl("for palestinian (?:liberation|rights)|free palestine(?! from hamas)|in solidarity with (?:palestin|gaza)|in remembrance of palestin|ceasefire in gaza|against apartheid in israel|(?:for ending|against) israel's occupation of palestin|genocide of palestin", claims, ignore.case = TRUE, perl = TRUE)) %>%
  filter(!grepl("in solidarity with Israel", claims, ignore.case = TRUE)) %>%
  filter(!(grepl("counter-protest", type) & !grepl("foreign", issues_major)))

# format date col and then get nicely formatted date string for use in label
ccc$date <- lubridate::date(ccc$date)
ccc$date_string <- lubridate::stamp_date("January 1, 2017")(ccc$date)
ccc$location <- with(ccc, paste0(resolved_locality, ", ", resolved_state))

ccc$size_label <- with(ccc, case_when(
  size_cat == 1 ~ "tens",
  size_cat == 2 ~ "hundreds",
  size_cat == 3 ~ "thousands",
  size_cat == 4 ~ "tens of thousands",
  TRUE ~ "unknown"
))

ccc$location <- paste(ccc$locality, ccc$state, sep = ", ")

# build the label that will appear when users click on an event marker, complete with style
ccc$marker_label <- with(ccc, paste(
  "<div>",
  "<p>",
    "<h4>", location, "</h4>",
    "<h5>", date_string, "</h5>",
  "<p> <ul>",
    "<li> <b> Location Detail: </b>", ifelse(is.na(location_detail), "none recorded", location_detail), "</li>",
    "</br>",  
    "<li> <b> Est. Crowd Size: </b>", size_label, " </li>",
    "</br>",
    "<li> <b> Claims: </b>", ifelse(is.na(claims), "none recorded", claims), "</li>",
    "</br>",
  "<p>",
    ifelse(grepl("^http|^www", source_1), sprintf("<a href=%s target='_blank'> source link </a>", source_1), sprintf("source: %s", source_1)),
    ifelse(!is.na(source_2) & grepl("^http|^www", source_2), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_2), ""), 
    ifelse(!is.na(source_3) & grepl("^http|^www", source_3), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_3), ""),
    ifelse(!is.na(source_3) & grepl("^http|^www", source_4), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_4), ""),
    ifelse(!is.na(source_3) & grepl("^http|^www", source_5), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_5), ""),
    ifelse(!is.na(source_3) & grepl("^http|^www", source_6), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_6), ""),
    ifelse(!is.na(source_3) & grepl("^http|^www", source_7), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_7), ""),
    ifelse(!is.na(source_3) & grepl("^http|^www", source_8), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_8), ""),
    ifelse(!is.na(source_3) & grepl("^http|^www", source_9), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_9), ""),
    ifelse(!is.na(source_3) & grepl("^http|^www", source_10), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_10), ""),
  "</p>",
  "</div>"))

# jitter the geocoordinates a bit so events in same location can be distinguished
ccc$lat <- jitter(ccc$lat, factor = 1)
ccc$lon <- jitter(ccc$lon, factor = 1)

# drop rows with no geocoords so tallies and map use same data
ccc <- filter(ccc, !is.na(lat))

# create cols for marker color and radius conditional on event size
ccc <- mutate(ccc, marker_color = ifelse(size_cat == 0, "gray75", "#cc5500"))
ccc <- mutate(ccc, marker_radius = case_when(
  size_cat == 2 ~ 10,
  size_cat == 3 ~ 15,
  size_cat == 4 ~ 20,
  TRUE ~ 5
))

# create other markers to use as filters
ccc$counter <- with(ccc, as.integer(grepl("counter-protest", type)))
ccc$countered <- with(ccc, ifelse(!is.na(macroevent), 1, 0))
ccc$directaction <-  with(ccc, ifelse(grepl("direct action", type, ignore.case = TRUE), 1, 0))
ccc$electeds <- with(ccc, ifelse(grepl("\\belected|lawmaker|legislator|council", participants, ignore.case = TRUE), 1, 0))
ccc$schools <- with(ccc, ifelse(grepl("college|university|school|institute of technology|\\bpoly(technic institute)?|\\bauraria|pentacrest", location_detail, ignore.case = TRUE), 1, 0))
ccc$casualties <- with(ccc, ifelse(injuries_crowd_any == 1 | !is.na(participant_deaths), 1, 0))
ccc$encampment <- with(ccc, ifelse(grepl("encamp", participant_measures, ignore.case = TRUE), 1, 0))

ccc$arrests <- as.integer(ccc$arrests)
ccc$arrests <- with(ccc, ifelse(is.na(arrests), 0, arrests))
ccc$injuries_crowd <- as.integer(ccc$injuries_crowd)
ccc$injuries_crowd <- with(ccc, ifelse(is.na(injuries_crowd), 0, injuries_crowd))
ccc$injuries_police <- as.integer(ccc$injuries_police)
ccc$injuries_police <- with(ccc, ifelse(is.na(injuries_police), 0, injuries_police))

# save version with only the cols req'd for the app
ccc <- select(ccc,
              date,
              location,
              location_detail,
              type,
              size_mean,
              organizations,
              arrests,
              arrests_any,
              property_damage_any,
              injuries_crowd,
              injuries_crowd_any,
              injuries_police,
              injuries_police_any,
              counter,
              countered,
              directaction,
              electeds,
              schools,
              casualties,
              encampment,
              claims,
              lat,
              lon,
              size_label,
              marker_label,
              marker_color,
              marker_radius,
              source_1:source_10)

write.csv(ccc, "c:/users/ulfel/documents/palestine-protest-dashboard/data/palestine_dashboard_data.csv", row.names = FALSE)