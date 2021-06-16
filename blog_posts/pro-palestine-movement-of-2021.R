library(tidyverse)
library(lubridate)
library(zoo)
library(patchwork)
library(scales)
library(wesanderson)
library(ggmap)
library(gganimate)
library(gifski)

options(stringsAsFactors = FALSE)

# read the data from the Github repo, set the date field to the correct class,
# and drop events with no date
ccc_path <- "https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv"
ccc <- read.csv(ccc_path)
ccc$date <- date(ccc$date)
ccc <- filter(ccc, !is.na(date))
ccc$yrmo <- zoo::as.yearmon(ccc$date)
ccc$yrmo_date <- date(ccc$yrmo)

max_date <- "2021-06-10"

# filter to post-Sheikh Jarrah events in support of Palestine
sj <- ccc %>%
  filter(str_detect(macroevent, "202105-sheikhjarrah")) %>%
  filter(date <= max_date)

sj$date <- lubridate::date(sj$date)

## SELECTED STATISTICS ##

# number of events
nrow(sj)

# scale of participation
sum(sj$size_low, na.rm = TRUE)
sum(sj$size_high, na.rm = TRUE)
sum(sj$size_mean, na.rm = TRUE)

# unique localities
length(unique(with(sj, paste(resolved_locality, resolved_state, sep = ", "))))

# peak days
sj %>%
  group_by(date) %>%
  tally() %>%
  arrange(-n)

## TIME SERIES CHARTS ##

# daily tallies of events, unique localities, and crowd sizes

p_sj_events_dd <- sj %>%
  group_by(date) %>%
  tally() %>%
  ungroup() %>%
  ggplot(aes(date, n)) + 
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_date(breaks = seq(from = date("2021-05-01"), to = date("2021-06-05"), by = 7),
                 date_labels = "%B %d") +
    labs(title = "Daily counts of pro-Palestine demonstrations since May 1, 2021")

p_sj_crowds_dd <- sj %>%
  group_by(date) %>%
  summarize(n = sum(size_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  ggplot(aes(date, n)) + 
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_y_continuous(labels = comma) +
    scale_x_date(breaks = seq(from = date("2021-05-01"), to = date("2021-06-05"), by = 7),
                 date_labels = "%B %d") +
    labs(title = "Daily sums of participants in pro-Palestine demonstrations since May 1, 2021",
       caption = "Source: Crowd Counting Consortium")

png("palestine-counts-daily.png", width = 7, height = 5, units = "in", res = 300)
p_sj_events_dd / p_sj_crowds_dd
dev.off()

## ANIMATED MAP ##

us <- c(left = -125, bottom = 24.5, right = -67, top = 49)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")

# This set of steps is required to add empty maps for dates with no events
df_dateseq <- seq(from = min(sj$date), to = max(sj$date), by = "day")
df_append <- as.data.frame(matrix(nrow = length(df_dateseq), ncol = ncol(sj)))
names(df_append) <- names(sj)
df_append$date <- df_dateseq
df_append$lat <- -90; df_append$lon <- -45; df_append$size_cat <- 99
df_append <- filter(df_append, !(date %in% sj$date))

sj_mapdat <- rbind(sj, df_append)

# create feature with dot sizes for map based on categorical measure of scale;
# had to tinker with these sizes and size of ouput for a bit to get version I liked
sj_mapdat$mapsize <- with(sj_mapdat, case_when(size_cat == 0 ~ 1/15,
                                               size_cat == 1 ~ 1/10,
                                               size_cat == 2 ~ 1/5,
                                               size_cat == 3 ~ 1/2,
                                               size_cat == 4 ~ 1,
                                               size_cat == 99 ~ 0))

sj_map <- ggmap(us_map, extent = "device") +
  geom_point(aes(x = lon, y = lat, size = mapsize),
             data = sj_mapdat,
             alpha = 0.5,
             color = "forestgreen",
             show.legend = FALSE)

sj_map_anim <- sj_map + transition_time(date) + labs(caption = 'Date: {frame_time}')

sj_map_anim_gif <- animate(sj_map_anim,
                           fps = 1,
                           duration = as.integer(max(sj$date) - min(sj$date) + 1), # manually set to show 1 map per second, duration = number of days
                           width = 5, height = 3, units = "in", res = 300,
                           renderer = gifski_renderer())

anim_save("ccc-palestine-2021-animated-map.gif", sj_map_anim_gif)

# counts of pro-Palestine and pro-Israel events earlier in 2021

pal_2021 <- ccc %>%
  filter(date >= "2021-01-01" & date < "2021-05-01") %>%
  filter(grepl("palestine|palestinian", claims, ignore.case = TRUE))

nrow(pal_2021)

isr_2021 <- ccc %>%
  filter(date >= "2021-01-01" & date < "2021-05-01") %>%
  filter(grepl("israel", claims, ignore.case = TRUE))

nrow(isr_2021)


# pro-Israel mobilization

isr <- ccc %>%
  filter(date >= "2021-05-01" & date <= max_date) %>%
  filter(grepl("in support of israel", claims, ignore.case = TRUE) & !grepl("counter-protest", type))

# number of events
nrow(isr)

# scale of participation
sum(isr$size_low, na.rm = TRUE)
sum(isr$size_high, na.rm = TRUE)
sum(isr$size_mean, na.rm = TRUE)

# unique localities
length(unique(with(isr, paste(resolved_locality, resolved_state, sep = ", "))))

