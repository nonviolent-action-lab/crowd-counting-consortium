ccc <- read.csv("c:/users/ulfel/documents/nval/ccc/data_clean/ccc_compiled.csv")

require(lubridate)
require(dplyr)


# format date col and then get nicely formatted date string for use in label
ccc$date <- lubridate::date(ccc$date)
ccc$date_string <- lubridate::stamp_date("January 1, 2017")(ccc$date)
ccc$location <- with(ccc, paste0(resolved_locality, ", ", resolved_state))

# drop rows identified as occurring online or as virtual events;
# events without dates; and events in the future
ccc <- ccc[ccc$state != "VIRTUAL" & 
           !grepl("online", ccc$locality, ignore.case = TRUE) &
           !is.na(ccc$date) &
            ccc$date < Sys.Date(),]

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
    "<li> <b> Event Type: </b>", ifelse(is.na(type), "none recorded", type), "</li>",
    "</br>",
    "<li> <b> Crowd Size: </b>", size_label, " </li>",
    "</br>",
    "<li> <b> Claims: </b>", ifelse(is.na(claims), "none recorded", claims), "</li>",
    "</br>",
    "<li> <b> Issue Tags: </b>", ifelse(is.na(issues) | issues == "", "none", issues), " </li> </ul> </p>",
  "<p>",
    ifelse(grepl("^http|^www", source_1), sprintf("<a href=%s target='_blank'> source link </a>", source_1), sprintf("source: %s", source_1)),
    ifelse(!is.na(source_2) & grepl("^http|^www", source_2), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_2), ""), 
    ifelse(!is.na(source_3) & grepl("^http|^www", source_3), sprintf("</br> <a href=%s target='_blank'> source link </a>", source_3), ""), 
  "</p>",
  "</div>"))

# jitter the geocoordinates a bit so events in same location can be distinguished
ccc$lat <- jitter(ccc$lat, factor = 0.5)
ccc$lon <- jitter(ccc$lon, factor = 0.5)

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

# replace empty issue tag field with 'other/unknown' to allow cleaner labeling and filtering
ccc$issues[ccc$issues == ""] <- "other/unknown"

# save version with only the cols req'd for the app
ccc <- select(ccc,
              date,
              location,
              type,
              online,
              issues,
              valence,
              macroevent,
              size_mean,
              arrests_any,
              property_damage_any,
              injuries_crowd_any,
              injuries_police_any,
              claims,
              lat,
              lon,
              size_label,
              marker_label,
              marker_color,
              marker_radius,
              source_1:source_3)

write.csv(ccc, "c:/users/ulfel/documents/ccc-data-dashboard/data/ccc_dashboard_data.csv", row.names = FALSE)