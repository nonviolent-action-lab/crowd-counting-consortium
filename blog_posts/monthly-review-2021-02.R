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

dat <- ccc <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv")
dat$date <- date(dat$date)
dat <- filter(dat, date < "2021-03-01" & !is.na(date))


## TIME SERIES PLOT ##

dat_mo <- dat %>%
  mutate(date = lubridate::date(date)) %>%
  mutate(yrmo = lubridate::date(zoo::as.yearmon(date))) %>%
  group_by(yrmo) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE))

png("ccc-event-counts-2021-02.png", res = 300, width = 20, height = 10, unit = "cm")
dat_mo %>%
  filter(yrmo >= date("2019-01-01")) %>%
  ggplot(aes(yrmo, n)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank()) +
    scale_x_date(date_labels = "%b %Y") +
    scale_y_continuous(labels = comma) +
    labs(title = "Monthly counts of U.S. protest events",
         caption = "Data source: Crowd Counting Consortium")
dev.off()

# count of events
dat_mo$n[dat_mo$yrmo == "2021-02-01"]

# change from previous month
(dat_mo$n[dat_mo$yrmo == "2021-02-01"] - dat_mo$n[dat_mo$yrmo == "2021-01-01"])/dat_mo$n[dat_mo$yrmo == "2021-01-01"]

# change from same month a year earlier
(dat_mo$n[dat_mo$yrmo == "2021-02-01"] - dat_mo$n[dat_mo$yrmo == "2020-02-01"])/dat_mo$n[dat_mo$yrmo == "2020-02-01"]


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

png("ccc-issues-2021-02.png", res = 300, width = 20, height = 16, unit = "cm")
dat_issues %>%
  filter(year == 2021 & month == 2) %>%
  ggplot(aes(reorder(value, -n, sum), n)) + 
    geom_col(fill = wesanderson::wes_palette("Royal1")[1]) +
    theme_minimal() +
    labs(title = "Counts of U.S. protest events by political issue",
         subtitle = "February 2021",
         caption = "Data source: Crowd Counting Consortium") +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank())
dev.off()

## MAP ##

us <- c(left = -125, bottom = 24.5, right = -67, top = 49)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")

dat_map <- dat

# jitter the geocoordinates a bit so events in same location can be distinguished
dat_map$lat <- jitter(dat_map$lat, factor = 0.5)
dat_map$lon <- jitter(dat_map$lon, factor = 0.5)

# drop rows with no geocoords
dat_map <- filter(dat_map, !is.na(lat))

png("ccc-map-2021-feb-bamazon.png", width = 7, height = 5, units = "in", res = 300)
ggmap(us_map, extent = "device") +
  geom_point(aes(x = lon, y = lat, size = 1.5),
             data = filter(dat_map, month(date) == 2,
                                    year(date) == 2021,
                                    !grepl("online", location_detail, ignore.case = TRUE),
                                    grepl("National Day of Solidarity with Alabama Amazon Workers", misc)),
             alpha = 1/2,
             color = wes_palette("Royal1")[2],
             show.legend = FALSE) +
  labs(title = "Actions in solidarity with Alabama Amazon workers",
       subtitle = "February 19-21, 2021")
dev.off()

## MISCELLANEOUS ##

# how many events in February with no info on crowd size
sum(is.na(dat[dat$date >= "2021-02-01" & dat$date <= "2021-02-28",]$size_mean))

# looking for examples of protests about policing but not racism
feb_2020_policing <- filter(dat, date >= "2021-02-01" & date <= "2021-02-28" & grepl("policing", issues) & !grepl("racism", issues))

# how many Line 3 events?
with(dat, sum(date >= "2021-02-01" & date <= "2021-02-28" & grepl("Line 3", claims)))
with(dat, sum(date >= "2021-02-01" & date <= "2021-02-28" & grepl("environment", issues)))
with(dat, sum(date >= "2021-02-01" & date <= "2021-02-28" & grepl("environment", issues) & grepl("native", issues)))

# against anti-asian violence
with(dat, sum(date >= "2021-02-01" & date <= "2021-02-28" & grepl("anti-Asian", claims)))
with(dat, sum(date >= "2021-01-01" & date <= "2021-01-31" & grepl("anti-Asian", claims)))
sum(dat[dat$date >= "2021-02-01" & dat$date <= "2021-02-28" & grepl("anti-Asian", dat$claims),]$size_mean, na.rm = TRUE)
table(dat[dat$date >= "2021-02-01" & dat$date <= "2021-02-28" & grepl("anti-Asian", dat$claims),]$state)
