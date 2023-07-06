library(tidyverse)
library(zoo)
library(ggmap)
library(patchwork)


# max date
edge_date <- "2023-07-02"

ccc <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv") %>%
  mutate(date = lubridate::date(date),
         fips_code = ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code)) %>%
  filter(!is.na(date))

df_pride <- ccc %>%
  filter(grepl("lgbt", issues)) %>%
  filter(valence == 1) %>%
  filter(grepl("for (?:lgbt.{1,}|trans|gay|queer|lesbian|dyke) pride", claims, perl = TRUE, ignore.case = TRUE)) %>%
  filter(!grepl("counter-protest", type)) %>%
  filter(date >= "2023-05-01" & date <= edge_date)


## STATS ##

# total count
nrow(df_pride)

# crowd size
df_pride %>% summarize(across(size_low:size_mean, ~ sum(., na.rm = TRUE)))
sum(!is.na(df_pride$size_mean))
round(100 * sum(!is.na(df_pride$size_mean))/nrow(df_pride), 1)
select(df_pride, date, locality, state, size_mean)[which(df_pride$size_mean == max(df_pride$size_mean, na.rm = TRUE)),]
df_pride %>% select(date, locality, state, size_mean) %>% arrange(-size_mean) %>% slice(1:10)

# how many saw counter-protests?
pride_ids <- unique(df_pride$macroevent)
pride_ids <- pride_ids[!is.na(pride_ids)]
df_pride_counters <- df_pride %>% filter(macroevent %in% pride_ids)
nrow(df_pride_counters)
round(100 * nrow(df_pride_counters)/nrow(df_pride), 1)

# number of events with participant injuries
nrow(filter(df_pride, injuries_crowd_any == 1))
df_pride %>% filter(injuries_crowd_any == 1) %>%  # which ones
  select(date, locality, state, size_mean, injuries_crowd, macroevent) 

# how many of those counter-protests included far-right groups?
source("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/data_compilation/ccc_analysis_regex_list.R")
regex_farright <- paste(c(unlist(regex_groups_white), unlist(regex_groups_farright)), collapse = "|")
ccc %>%
  filter(macroevent %in% pride_ids) %>%
  group_by(macroevent) %>%
  summarize(farright = any(grepl(regex_farright, organizations, perl = TRUE, ignore.case = TRUE))) %>%
  summarize(farright = sum(farright))
ccc %>%
  filter(macroevent %in% pride_ids) %>%
  group_by(macroevent) %>%
  summarize(farright = any(grepl("proud boys", organizations, perl = TRUE, ignore.case = TRUE))) %>%
  summarize(farright = sum(farright))
ccc %>%
  filter(macroevent %in% pride_ids) %>%
  group_by(macroevent) %>%
  summarize(farright = any(grepl("patriot front", organizations, perl = TRUE, ignore.case = TRUE))) %>%
  summarize(farright = sum(farright))
ccc %>%
  filter(macroevent %in% pride_ids) %>%
  group_by(macroevent) %>%
  summarize(farright = any(grepl("white lives matter", organizations, perl = TRUE, ignore.case = TRUE))) %>%
  summarize(farright = sum(farright))
ccc %>%
  filter(macroevent %in% pride_ids) %>%
  group_by(macroevent) %>%
  summarize(farright = any(grepl("active club", organizations, perl = TRUE, ignore.case = TRUE))) %>%
  summarize(farright = sum(farright))
ccc %>%
  filter(macroevent %in% pride_ids) %>%
  group_by(macroevent) %>%
  summarize(farright = any(grepl("goyim|\\bgdl\\b", organizations, perl = TRUE, ignore.case = TRUE))) %>%
  summarize(farright = sum(farright))

# how many 1sts, 2nds, and 3rds
df_pride <- df_pride %>%
  mutate(annual = case_when(
    grepl("^1st|first", notes, perl = TRUE, ignore.case = TRUE) ~ "1",
    grepl("^2nd|second", notes, perl = TRUE, ignore.case = TRUE) ~ "2",
    grepl("^3rd|third", notes, perl = TRUE, ignore.case = TRUE) ~ "3",
    TRUE ~ "4+"
  ))
table(df_pride$annual)
mean(df_pride$size_mean[df_pride$annual == "1"], na.rm = TRUE)

## VISUALIZATIONS ##

png(sprintf("pride-2023-antilgbtq-monthly-counts-%s.png", gsub("-", "", Sys.Date())),
    res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
ccc %>%
  filter(grepl("lgbtq", issues) & valence == 2) %>%
  filter(date <= "2023-06-30" & date >= "2021-01-01") %>%
  # create year-month var to use for grouping
  mutate(yrmo = as.yearmon(date)) %>%
  group_by(yrmo) %>%
  tally() %>%
  mutate(date = as.Date(yrmo)) %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Monthly counts of anti-LGBTQ+ demonstrations in the U.S.",
         subtitle = "January 2021\u2013June 2023",
         caption = "Source: Crowd Counting Consortium") +
    scale_x_date(breaks = seq(date("2021-06-01"), date("2023-06-01"), by = "1 year"),
                 date_labels = "%b %Y")
dev.off()

# juxtaposition chart
chart_anti <- ccc %>%
  filter(grepl("lgbtq", issues) & valence == 2) %>%
  filter(date <= "2023-06-30" & date >= "2021-01-01") %>%
  # create year-month var to use for grouping
  mutate(yrmo = as.yearmon(date)) %>%
  group_by(yrmo) %>%
  tally() %>%
  mutate(date = as.Date(yrmo)) %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          title = element_blank()) +
    ylim(0,600) +
    scale_x_date(breaks = seq(date("2021-06-01"), date("2023-06-01"), by = "1 year"),
                 date_labels = "%b %Y")

chart_pro <- ccc %>%
  filter(grepl("lgbtq", issues) & valence == 1) %>%
  filter(date <= "2023-06-30" & date >= "2021-01-01") %>%
  # create year-month var to use for grouping
  mutate(yrmo = as.yearmon(date)) %>%
  group_by(yrmo) %>%
  tally() %>%
  mutate(date = as.Date(yrmo)) %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          title = element_blank()) +
    ylim(0,600) +
    scale_x_date(breaks = seq(date("2021-06-01"), date("2023-06-01"), by = "1 year"),
                 date_labels = "%b %Y")

png(sprintf("pride-2023-juxtaposition-monthly-counts-%s.png", gsub("-", "", Sys.Date())),
    res = 300, width = 16/1.67, height = 16/1.67, unit = "in")
chart_juxt <- chart_pro / chart_anti
chart_juxt + plot_annotation(
  title = "Monthly counts of U.S. protest events raising LGBTQ+ issues",
  subtitle = "January 2021\u2013June 2023",
  caption = "Source: Crowd Counting Consortium",
  tag_levels = list(c("pro-LGBTQ+", "anti-LGBTQ+"))
) & theme(
  plot.tag = element_text(size = 10),
  plot.tag.position = "right"
)
dev.off()

# map of US showing locations with size
us <- c(left = -125, bottom = 24.5, right = -67, top = 50)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")
map <- ggmap(us_map, extent = "panel")

df_pride <- df_pride %>%
  mutate(lat = jitter(lat, factor = 0.5),
         lon = jitter(lon, factor = 0.5))

png("pride-2023-map.png", width = 16/1.67, height = 9/1.67, units = "in", res = 300)
map +
  geom_point(aes(x = lon, y = lat),
             data = df_pride,
             shape = 21,
             color = "black",
             fill = "pink",
             alpha = 1/2,
             size = 3) +
  labs(title = "LGBTQ+ pride events in the U.S. from May 1 to July 2, 2023",
       subtitle = sprintf("%s events in %s cities and towns across all 50 states and DC",
                          nrow(df_pride),
                          length(unique(paste(df_pride$locality, df_pride$state, sep = ", ")))
                          ),
       caption = "Source: Crowd Counting Consortium") +
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())
dev.off()
