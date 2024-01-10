library(tidyverse)
library(scales)
library(wesanderson)
library(tidytext)
library(wordcloud)
library(zoo)
library(patchwork)
library(usmap)
library(tidycensus)


options(stringsAsFactors = FALSE)

# comment this out if you're not me
setwd('nval/blog-posts')

# point this at the raw pages on the github repo if you're not me
ccc_path <- "c:/users/ulfel/documents/nval/ccc/data_clean/ccc_compiled.csv"

ccc <- read.csv(ccc_path) %>%
  mutate(date = lubridate::date(date),
         fips_code = ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code),
         issues = ifelse(issues == "", "other", issues)) %>%
  filter(!is.na(date))

edge_date = "2023-12-31"

my_year = year(edge_date)


## OVERVIEW STATS ##

df_current <- filter(ccc, lubridate::year(date) == my_year & !grepl("strike|walkout|sickout", type))

df_prev <- filter(ccc, lubridate::year(date) == my_year - 1 & !grepl("strike|walkout|sickout", type))

# how many events?
nrow(df_current)
nrow(df_current)/nrow(df_prev)
nrow(df_prev)

# how many towns?
length(unique(with(df_current, paste(resolved_locality, state, sep = ", "))))
length(unique(with(df_current, paste(resolved_locality, state, sep = ", "))))/length(unique(with(df_prev, paste(resolved_locality, state, sep = ", "))))
length(unique(with(df_prev, paste(resolved_locality, state, sep = ", "))))

# how many people?
sum(df_current$size_mean, na.rm = TRUE)
sum(df_current$size_mean, na.rm = TRUE)/sum(df_prev$size_mean, na.rm = TRUE)
sum(df_prev$size_mean, na.rm = TRUE)
sum(df_current$size_low, na.rm = TRUE)
sum(df_current$size_high, na.rm = TRUE)
sum(!is.na(df_current$size_mean))
sum(!is.na(df_current$size_mean))/nrow(df_current)
mean(df_current$size_mean, na.rm = TRUE)
median(df_current$size_mean, na.rm = TRUE)

sum(df_prev$size_mean, na.rm = TRUE)
sum(df_prev$size_low, na.rm = TRUE)
sum(df_prev$size_high, na.rm = TRUE)
sum(!is.na(df_prev$size_mean))
sum(!is.na(df_prev$size_mean))/nrow(df_prev)
mean(df_prev$size_mean, na.rm = TRUE)
median(df_prev$size_mean, na.rm = TRUE)

# injuries?
sum(df_current$injuries_crowd_any)
sum(df_current$injuries_crowd_any)/nrow(df_current)
sum(df_prev$injuries_crowd_any)
sum(df_prev$injuries_crowd_any)/nrow(df_prev)
sum(ccc[year(ccc$date) == 2020,]$injuries_crowd_any)
sum(ccc[year(ccc$date) == 2021,]$injuries_crowd_any)

# deaths?
view(filter(df_current, !is.na(participant_deaths)))
view(filter(df_prev, !is.na(participant_deaths)))

# police injuries?
sum(df_current$injuries_police_any)
sum(df_current$injuries_police_any)/nrow(df_current)
sum(df_prev$injuries_police_any)
sum(df_prev$injuries_police_any)/nrow(df_prev)
sum(ccc[year(ccc$date) == 2020,]$injuries_police_any)
sum(ccc[year(ccc$date) == 2021,]$injuries_police_any)

# property damage?
sum(df_current$property_damage_any)
sum(df_current$property_damage_any)/nrow(df_current)
sum(df_prev$property_damage_any)
sum(df_prev$property_damage_any)/nrow(df_prev)
sum(ccc[year(ccc$date) == 2020,]$property_damage_any)
sum(ccc[year(ccc$date) == 2021,]$property_damage_any)

## ANTI-LGBTQ ##

df_antilgbtq <- ccc %>%
  filter(date <= edge_date) %>%
  filter(valence == 2 & grepl("lgbt", issues)) %>%
  mutate(yrmo = as.yearmon(date),
         drag = case_when(
           grepl("drag", claims, perl = TRUE, ignore.case = TRUE) & grepl("story (?:hour|time)|dqsh", claims, perl = TRUE, ignore.case = TRUE) ~ "Drag Story Hour",
           grepl("drag", claims, perl = TRUE, ignore.case = TRUE) ~ "other drag",
           TRUE ~ "other anti-LGBTQ+"
         ))

png("ccc-review-2024-antilgbtq-monthly-counts.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
df_antilgbtq %>%
  group_by(yrmo, drag) %>%
  tally() %>%
  ungroup() %>%
  mutate(date = as.Date(yrmo),
         drag = fct_relevel(drag, "other anti-LGBTQ+", "other drag", "Drag Story Hour")) %>%
  ggplot(aes(date, n, fill = drag)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Monthly counts of anti-LGBTQ+ demonstrations in the U.S.",
         subtitle = "January 2017\u2013December 2023",
         caption = "Source: Crowd Counting Consortium") +
    scale_fill_manual(values = wes_palette("Royal1")[c(1,4,2)],
                      name = NULL) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

df_prolgbtq <- ccc %>%
  filter(date <= edge_date) %>%
  filter(valence == 1 & grepl("lgbt", issues)) %>%
  mutate(yrmo = as.yearmon(date),
         laws = ifelse(grepl("legis", issues), "yes", "no"),
         counters = ifelse(grepl("counter.?protest", type), "yes", "no"))

png("ccc-review-2024-prolgbtq-monthly-counts.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
df_prolgbtq %>%
  group_by(yrmo, counters) %>%
  tally() %>%
  ungroup() %>%
  mutate(date = as.Date(yrmo)) %>%
  ggplot(aes(date, n, fill = counters)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Monthly counts of pro-LGBTQ+ protest events in the U.S.",
         subtitle = "January 2017\u2013December 2023",
         caption = "Source: Crowd Counting Consortium") +
    scale_fill_manual(values = wes_palette("Royal1")[c(1,2)],
                      name = "Counter-\nprotest\nto anti-\nLGBTQ+\naction?") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()


## ISRAEL-PALESTINE ##

# pro-Israel activity 

df_isr <- ccc %>%
  filter(date >= "2023-10-07" & date <= edge_date) %>%
  filter(grepl("against violent attack on israel|(?:in|for) (?:solidarity with|support of) israel|stand with israel", claims, perl = TRUE, ignore.case = TRUE)) %>%
  filter(!grepl("counter-protest", type)) %>%
  filter(!grepl("for palestinian liberation|palestine will be free|free free Palestine", claims, perl = TRUE, ignore.case = TRUE))

nrow(df_isr)
with(df_isr, length(unique(paste(locality, state, sep = ", "))))
with(df_isr, length(unique(state)))
sort(unique(df_isr$state))
sum(!is.na(df_isr$size_mean))
round(sum(!is.na(df_isr$size_mean)) / nrow(df_isr), 2)
sum(df_isr$size_mean, na.rm = TRUE)
sum(df_isr$size_low, na.rm = TRUE)
sum(df_isr$size_high, na.rm = TRUE)
median(df_isr$size_mean, na.rm = TRUE)

sum(as.numeric(df_isr$arrests), na.rm = TRUE)

sum(df_isr$property_damage_any)

png("ccc-review-2024-isr-daily-counts.png", res = 300, width = 7, height = 5, unit = "in")
isr_daily_events_chart <- df_isr %>%
  group_by(date) %>%
  tally() %>%
  ungroup() %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank())
isr_daily_crowds_chart <- df_isr %>%
  group_by(date) %>%
  summarize(n = sum(size_mean, na.rm = TRUE)) %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank())
isr_daily_events_chart / isr_daily_crowds_chart +
  plot_annotation(title = "Daily tallies for pro-Israel protest events in the U.S.",
                  subtitle = "October 7\u2013December 31, 2023",
                  caption = "Source: Crowd Counting Consortium",
                  tag_levels = list(c("count of\nevents",
                                      "estimated\nparticipants"
                                      ))
                  ) & theme(
                  plot.tag = element_text(size = 10),
                  plot.tag.position = "right"
                  )
dev.off()

# pro-Palestine data and stats

df_pal <- ccc %>%
  filter(date >= "2023-10-07" & date <= edge_date) %>%
  # this regex id's pro-palestine events in late 2023 by their claims
  filter(grepl("for palestinian liberation|free palestine(?! from hamas)|in solidarity with palestin|in remembrance of palestin|for ceasefire in gaza", claims, ignore.case = TRUE, perl = TRUE)) %>%
  filter(!grepl("counter-protest", type)) %>%
  # this catches a few pro-Israel events that include strings used in regex above
  filter(!grepl("in solidarity with Israel", claims, ignore.case = TRUE))

nrow(df_pal)
with(df_pal, length(unique(paste(locality, state, sep = ", "))))
with(df_pal, length(unique(state[state %in% state.abb])))
sort(unique(df_pal$state))
sum(!is.na(df_pal$size_mean))
round(sum(!is.na(df_pal$size_mean)) / nrow(df_pal), 2)
sum(df_pal$size_mean, na.rm = TRUE)
sum(df_pal$size_low, na.rm = TRUE)
sum(df_pal$size_high, na.rm = TRUE)
median(df_pal$size_mean, na.rm = TRUE)

sum(as.numeric(df_pal$arrests), na.rm = TRUE)

table(df_pal$electeds)["yes"]
round(table(df_pal$electeds)["yes"] / nrow(df_pal), 3)

sum(df_pal$property_damage_any)
round(sum(df_pal$property_damage_any) / nrow(df_pal), 2)
# print(unlist(df_pal[df_pal$property_damage_any == 1,]$property_damage))

sum(grepl("swastika", df_pal$claims, ignore.case = TRUE))
sum(grepl("nazi", df_pal$claims, ignore.case = TRUE))
sum(grepl("hitler", df_pal$claims, ignore.case = TRUE))

png("ccc-review-2024-pal-daily-counts.png", res = 300, width = 7, height = 5, unit = "in")
pal_daily_events_chart <- df_pal %>%
  group_by(date) %>%
  tally() %>%
  ungroup() %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank())
pal_daily_crowds_chart <- df_pal %>%
  group_by(date) %>%
  summarize(n = sum(size_mean, na.rm = TRUE)) %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank())
pal_daily_events_chart / pal_daily_crowds_chart +
  plot_annotation(title = "Daily tallies for pro-Palestine protest events in the U.S.",
                  subtitle = "October 7\u2013December 31, 2023",
                  caption = "Source: Crowd Counting Consortium",
                  tag_levels = list(c("count of\nevents",
                                      "estimated\nparticipants"
                                      ))
                  ) & theme(
                  plot.tag = element_text(size = 10),
                  plot.tag.position = "right"
                  )
dev.off()

# pro-Israel counter-protests chart

df_isr_expansive <- ccc %>%
  filter(date >= "2023-10-07" & date <= edge_date) %>%
  filter(grepl("against violent attack on israel|in (?:solidarity with|support of) israel|stand with israel", claims, perl = TRUE, ignore.case = TRUE)) %>%
  filter(!grepl("for palestinian liberation|palestine will be free|free free Palestine", claims, perl = TRUE, ignore.case = TRUE)) %>%
  mutate(acttype = ifelse(grepl("counter.?protest", type, ignore.case = TRUE), "counter-protest", "other"))

df_isr_expansive$week <- with(df_isr_expansive, case_when(

  date %in% seq(date("2023-10-07"), date("2023-10-15"), by = "1 day") ~ "Oct. 7-15",
  date %in% seq(date("2023-10-16"), date("2023-10-22"), by = "1 day") ~ "Oct. 16-22",
  date %in% seq(date("2023-10-23"), date("2023-10-29"), by = "1 day") ~ "Oct. 23-29",
  date %in% seq(date("2023-10-30"), date("2023-11-05"), by = "1 day") ~ "Oct. 30-Nov. 5",
  date %in% seq(date("2023-11-06"), date("2023-11-12"), by = "1 day") ~ "Nov. 6-12",
  date %in% seq(date("2023-11-13"), date("2023-11-19"), by = "1 day") ~ "Nov. 13-19",
  date %in% seq(date("2023-11-20"), date("2023-11-26"), by = "1 day") ~ "Nov. 20-26",
  date %in% seq(date("2023-11-27"), date("2023-12-03"), by = "1 day") ~ "Nov. 27-Dec. 3",
  date %in% seq(date("2023-12-04"), date("2023-12-10"), by = "1 day") ~ "Dec. 4-10",
  date %in% seq(date("2023-12-11"), date("2023-12-17"), by = "1 day") ~ "Dec. 11-17",
  date %in% seq(date("2023-12-18"), date("2023-12-24"), by = "1 day") ~ "Dec. 18-24",
  date %in% seq(date("2023-12-25"), date("2023-12-31"), by = "1 day") ~ "Dec. 25-31",
  TRUE ~ "2024"

))

df_isr_expansive <- mutate(df_isr_expansive, week = fct_relevel(week,
  "Oct. 7-15", "Oct. 16-22", "Oct. 23-29", "Oct. 30-Nov. 5",
  "Nov. 6-12", "Nov. 13-19", "Nov. 20-26", "Nov. 27-Dec. 3",
  "Dec. 4-10", "Dec. 11-17", "Dec. 18-24", "Dec. 25-31"))

png("ccc-review-2024-isr-weekly-counters.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
df_isr_expansive %>%
  filter(date <= edge_date) %>%
  group_by(week, acttype) %>%
  tally() %>%
  ungroup() %>%
  mutate(type = fct_relevel(acttype, "other", "counter-protest")) %>%
  ggplot(aes(week, n, fill = type)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = "bottom") +
    labs(title = "Weekly counts of pro-Israel events in U.S. by action type",
         subtitle = "October 7\u2013December 31, 2023",
         caption = "Source: Crowd Counting Consortium") +
    scale_fill_manual(values = c("gray60", "dodgerblue"),
                      name = NULL)
dev.off()


## ENVIRONMENT ##

df_eco <- ccc %>%
  filter(date <= edge_date) %>%
  filter(valence == 1 & grepl("enviro", issues)) %>%
  mutate(yrmo = as.yearmon(date),
         acttype = ifelse(grepl("direct action|civil dis|sit.?in|roadblock|blockade", type), "yes", "no"))

png("ccc-review-2024-environment-monthly-counts.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
df_eco %>%
  group_by(yrmo, acttype) %>%
  tally() %>%
  ungroup() %>%
  mutate(date = as.Date(yrmo)) %>%
  ggplot(aes(date, n, fill = acttype)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Monthly counts of U.S. protest events making environmental claims",
         subtitle = "January 2017\u2013December 2023",
         caption = "Source: Crowd Counting Consortium") +
    scale_fill_manual(values = wes_palette("Royal1")[c(1,2)],
                      name = "Did event involve\ncivil disobedience\nor direct action?") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()


## HOUSING ##

df_housing <- ccc %>%
  filter(date <= edge_date) %>%
  filter(grepl("housing", issues)) %>%
  mutate(yrmo = as.yearmon(date))

png("ccc-review-2024-housing-monthly-counts.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
df_housing %>%
  group_by(yrmo) %>%
  tally() %>%
  ungroup() %>%
  mutate(date = as.Date(yrmo)) %>%
  ggplot(aes(date, n)) +
    geom_col(fill = wes_palette("Royal1")[c(1)]) +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Monthly counts of U.S. protest events related to housing",
         subtitle = "January 2017\u2013December 2023",
         caption = "Source: Crowd Counting Consortium") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y")
dev.off()

df_housing %>%
  mutate(year = year(date)) %>%
  group_by(year, yrmo) %>%
  tally() %>%
  ungroup() %>%
  group_by(year) %>%
  summarize(avg = mean(n))


## PROUD BOYS ##

pb_apps <- ccc %>%
  filter(date <= edge_date) %>%  
  filter(grepl("proud boys", organizations, ignore.case = TRUE) | grepl("proud boys", actors, ignore.case = TRUE) | grepl("proud boys", participants, ignore.case = TRUE))

# yearly counts of appearances
pb_apps %>%
  mutate(year = year(date)) %>%
  group_by(year) %>%
  tally()

# map of appearances in last year
map <- plot_usmap(regions = "states")

pb_apps_latest <- pb_apps %>%
  filter(year(date) == my_year) %>%
  mutate(lat = jitter(lat, factor = 0.5), lon = jitter(lon, factor = 0.5)) %>%
  usmap_transform(.)

png("ccc-review-2024-proudboys-map.png", res = 300, width = 7, height = 5, unit = "in")
map +
  geom_point(aes(x = x, y = y),
             data = pb_apps_latest,
             shape = 21,
             colour = "black",
             fill = "gold",
             size = 3,
             alpha = 1/2,
             show.legend = FALSE) +
  theme(plot.margin = margin(1,1,1,1, unit = "cm"),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank()) +
  labs(title = sprintf("Reported Proud Boy appearances at U.S. protest events in %s", my_year),
       subtitle = sprintf("%s total appearances", nrow(pb_apps_latest)),
       caption = "Source: Crowd Counting Consortium")
dev.off()

# how many active club apps in 2023?
ccc |>
  filter(grepl("active club|northwest nationalist network", organizations, ignore.case = TRUE) | grepl("active club", actors, ignore.case = TRUE)) |>
  filter(valence == 2) |>
  filter(year(date) == 2023) |>
  summarize(n = n())


## ABORTION RIGHTS ##

df_repro <- ccc %>%
  filter(grepl("reproductive rights", issues_major) & valence == 1) |>
  filter(date >= "2022-01-01" & date <= edge_date) |>
  filter(!grepl("counter.?protest", type, ignore.case = TRUE))

png("ccc-review-2024-repro-monthly-counts.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
df_repro %>%
  mutate(yrmo = as.yearmon(date)) %>%
  group_by(yrmo) %>%
  tally() %>%
  ungroup() %>%
  mutate(date = as.Date(yrmo)) %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Monthly counts of U.S. protest events for reproductive rights",
         subtitle = "January 2022\u2013December 2023",
         caption = "Source: Crowd Counting Consortium") +
    scale_x_date(breaks = seq(from = date("2022-01-01"), to = date("2024-01-01"), by = "6 months"),
                 date_labels = "%b %Y") +
    scale_y_continuous(labels = comma)
dev.off()

# get and load Census API key
source("nal_census_api_key.r")
census_api_key(nal_census_api_key)
popdat <- get_acs(geography = "state",
                  variables = "B01003_001",
                  year = 2022,
                  output = "wide")
popdat <- transmute(popdat,
                    state = state.abb[match(NAME, state.name)],
                    statepop = B01003_001E)

repro_by_state <- df_repro %>%
  group_by(state) %>%
  tally() %>%
  merge(., popdat) %>%
  mutate(n_percap = n / (statepop / 1000000),
         n_peryear = n / 2) %>%
  arrange(state)

repro_by_state_predobbs <- ccc |>
  filter(grepl("reproductive rights", issues) & date < "2022-01-01" & valence == 1 & !grepl("counter.?protest", type, ignore.case = TRUE)) |>
  group_by(state) %>%
  tally() %>%
  merge(., popdat) %>%
  mutate(n_percap = n / (statepop / 1000000),
         n_peryear = n / 5) %>%
  arrange(state)

repro_by_state$n_peryear_diff <- repro_by_state$n_peryear - repro_by_state_predobbs$n_peryear
repro_by_state$n_peryear_diff_percap <- repro_by_state$n_peryear_diff / (repro_by_state$statepop / 1000000)

repro_by_state_year <- ccc |>
  filter(grepl("reproductive rights", issues) & date <= edge_date & valence == 1) |>  
  mutate(year = year(date)) |>
  group_by(state, year) |>
  tally() |>
  arrange(state)


