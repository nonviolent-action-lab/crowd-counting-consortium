library(tidyverse)
library(leaflet)
library(mapview)
library(lubridate)
library(zoo)
library(scales)
library(patchwork)
library(wesanderson)
library(ggmap)

options(stringsAsFactors = FALSE)

dat <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv")
dat$date <- date(dat$date)
dat <- filter(dat, date < "2021-04-01" & !is.na(date))


## TIME SERIES PLOT ##

dat_monthly <- dat %>%
  mutate(date = lubridate::date(date)) %>%
  mutate(yrmo = lubridate::date(zoo::as.yearmon(date))) %>%
  group_by(yrmo) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE))

png("ccc-event-counts-2021-03.png", res = 300, width = 20, height = 10, unit = "cm")
dat_monthly %>%
  filter(yrmo >= date("2019-03-01")) %>%
  ggplot(aes(yrmo, n)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(labels = comma) +
    labs(title = "Monthly counts of U.S. protest events, March 2019-March 2021",
         caption = "Data source: Crowd Counting Consortium")
dev.off()

### ISSUES ###

issues <- str_split(dat$issues, "; ", simplify = TRUE)

colnames(issues) <- paste("issue", seq(ncol(issues)), sep = "_")

dat_issues <- dat %>%
  select(date) %>%
  cbind(., issues) %>%
  filter(!is.na(date)) %>%
  pivot_longer(cols = contains("issue_")) %>%
  filter(value != "") %>%
  mutate(year = lubridate::year(date),
         month = lubridate::month(date)) %>%
  group_by(year, month, value) %>%
  tally()

png("ccc-issues-2021-03.png", res = 300, width = 20, height = 16, unit = "cm")
dat_issues %>%
  filter(year == 2021 & month == 3) %>%
  ggplot(aes(reorder(value, -n, sum), n)) + 
    geom_col(fill = wesanderson::wes_palette("Royal1")[1]) +
    theme_minimal() +
    labs(title = "Counts of U.S. protest events by political issue",
         subtitle = "March 2021",
         caption = "Data source: Crowd Counting Consortium") +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank())
dev.off()

## MAP ##

us <- c(left = -125, bottom = 24.5, right = -67, top = 49)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")

# filter data for mapping Stop Asian Hate events
dat_map_aapi <- dat %>%
  mutate(mapsize = ifelse(is.na(size_cat), 1, size_cat^2)) %>%
  mutate(lat = jitter(lat, factor = 0.5),
         lon = jitter(lon, factor = 0.5)) %>%
  filter(!is.na(lat) & !is.na(lon)) %>%
  filter(!grepl("online", location_detail, ignore.case = TRUE)) %>%
  filter(month(date) == 3 & year(date) == 2021) %>%
  filter(grepl("racism", issues) & grepl("asian", claims, ignore.case = TRUE))

png("ccc-map-2021-march-stop-asian-hate.png", width = 7, height = 5, units = "in", res = 300)
ggmap(us_map, extent = "device") +
  geom_point(aes(x = lon, y = lat, size = mapsize),
             data = dat_map_aapi,
             alpha = 1/2,
             shape = 21,
             fill = "darkorange",
             color = "black",
             stroke = 1,
             show.legend = FALSE) +
  labs(title = "Stop Asian Hate vigils and rallies",
       subtitle = "March 2021",
       caption = "Source: Crowd Counting Consortium")
dev.off()

## MISCELLANEOUS ##

dat_mo <- dat[dat$date >= "2021-03-01" & dat$date <= "2021-03-31",]

# event count and size range
nrow(dat_mo)
sum(dat_mo$size_low, na.rm = TRUE)
sum(dat_mo$size_high, na.rm = TRUE)

# how many events with no info on crowd size
sum(is.na(dat_mo$size_mean))
sum(!is.na(dat_mo$size_mean))/nrow(dat_mo)

# change from previous month
(dat_monthly$n[dat_monthly$yrmo == "2021-03-01"] - dat_monthly$n[dat_monthly$yrmo == "2021-02-01"])/dat_monthly$n[dat_monthly$yrmo == "2021-02-01"]

# change from same month a year earlier
(dat_monthly$n[dat_monthly$yrmo == "2021-03-01"] - dat_monthly$n[dat_monthly$yrmo == "2020-03-01"])/dat_monthly$n[dat_monthly$yrmo == "2020-03-01"]

# racism
sum(grepl("racism", dat_mo$issues))
sum(grepl("racism", dat_mo$issues))/nrow(dat_mo)

# asian hate
nrow(dat_mo[grepl("racism", dat_mo$issues) & grepl("asian|aapi", dat_mo$claims, ignore.case = TRUE),])

# how many related to breonna taylor
sum(grepl("breonna taylor", dat_mo$claims, ignore.case = TRUE))

# how many related to policing
sum(grepl("policing", dat_mo$issues))
nrow(dat_mo[grepl("policing", dat_mo$issues) & dat_mo$valence != 1,])

# strikes
dat_mo_strikes <- filter(dat_mo, grepl("labor", issues) & grepl("strike", type))
fix(dat_mo_strikes)

# right-wing events and worldwide rally for freedom
sum(dat_mo$valence == 2)
sum(dat_mo$valence == 2)/nrow(dat_mo)
sum(grepl("worldwide rally for freedom|world wide rally for freedom|freedom rally", dat_mo$misc, ignore.case = TRUE))


