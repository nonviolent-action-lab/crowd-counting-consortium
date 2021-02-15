library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)
library(zoo)
library(ggmap)
library(gganimate)
library(gifski)

options(stringsAsFactors = FALSE)

# read the data from the Github repo, set the date field to the correct class, and drop events with no date
ccc <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv")
ccc$date <- date(ccc$date)
ccc <- filter(ccc, !is.na(date))

# create a year-month feature to use in time series, and a date version of this to play nicely with ggplot
ccc$yrmo <- zoo::as.yearmon(ccc$date)
ccc$yrmo_date <- date(ccc$yrmo)

# filter down to George Floyd uprising, defined as...
george_floyd <- ccc %>%
  # ...events with 'racism' as issue tag AND anti-Trump/left-wing valence...
  filter(grepl("racism", issues) & valence == 1) %>%
  # ...occurring after police kill George Floyd
  filter(date >= "2020-05-25")

## SELECTED STATISTICS ##

# number of events
nrow(george_floyd)

# as a fraction of all events in dataset
nrow(george_floyd)/nrow(ccc)

# scale of participation
sum(george_floyd$size_low, na.rm = TRUE)
sum(george_floyd$size_high, na.rm = TRUE)
sum(george_floyd$size_mean, na.rm = TRUE)

# unique localities
length(unique(with(george_floyd, paste(resolved_locality, resolved_state, sep = ", ")))) # whole period
length(unique(with(filter(george_floyd, date <= "2020-06-21"), paste(resolved_locality, resolved_state, sep = ", ")))) # may-june only

# peak day
gf_days <- george_floyd %>%
  group_by(date) %>%
  tally() %>%
  arrange(-n)

# frequency of property damage
table(george_floyd$property_damage_any)[2]/sum(table(george_floyd$property_damage_any))

# frequency of protester injuries
table(george_floyd$injuries_crowd_any)[2]/sum(table(george_floyd$injuries_crowd_any))

# frequency of police injuries
table(george_floyd$injuries_police_any)[2]/sum(table(george_floyd$injuries_police_any))
tinj <- with(george_floyd, table(injuries_crowd_any, injuries_police_any))

# frequency of any arrests
table(george_floyd$arrests_any)[2]/sum(table(george_floyd$arrests_any))

# police use of chemical irritants
sum(george_floyd$chemical_agents, na.rm = TRUE)
sum(george_floyd$chemical_agents, na.rm = TRUE)/nrow(george_floyd)
tgt <- with(george_floyd, table(chemical_agents, property_damage_any))
tgt[2,1]/sum(tgt[2,])
george_floyd$any_damage_or_hurt_cops <- with(george_floyd, ifelse(property_damage_any == 1 | injuries_police_any == 1, 1, 0))
tgt2 <- with(george_floyd, table(chemical_agents, any_damage_or_hurt_cops))
tgt2[2,2]/sum(tgt[2,])

# frequency of any arrests, conditional on property damage
z <- with(george_floyd, table(arrests_any, property_damage_any))
z[2,2]/sum(z[,2])

## TIME SERIES CHARTS ##

# daily tallies of events, unique localities, and crowd sizes

p_gf_events_dd <- george_floyd %>%
  group_by(date) %>%
  tally() %>%
  ggplot(aes(date, n)) + 
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    labs(title = "Daily counts of anti-racism protest events since May 25, 2020")

p_gf_towns_dd <- george_floyd %>%
  mutate(locale = paste(resolved_locality, resolved_state, sep = ", ")) %>%
  arrange(date, locale) %>%
  group_by(date, locale) %>%
  slice(1) %>%
  ungroup() %>%
  group_by(date) %>%
  tally() %>%
  ggplot(aes(date, n)) + 
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    labs(title = "Daily counts of localities with anti-racism protest events since May 25, 2020")

p_gf_crowds_dd <- george_floyd %>%
  group_by(date) %>%
  summarize(n = sum(size_mean, na.rm = TRUE)) %>%
  ggplot(aes(date, n)) + 
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    scale_y_continuous(labels = comma) +
    labs(title = "Daily sums of participants in anti-racism protest events since May 25, 2020",
       caption = "Source: Crowd Counting Consortium")

png("george-floyd-counts-daily.png", width = 7, height = 8, units = "in", res = 300)
p_gf_events_dd / p_gf_towns_dd / p_gf_crowds_dd
dev.off()

# incidence of property damage

png("george-floyd-damage-monthly.png", width = 7, height = 3.5, units = "in", res = 300)
george_floyd %>%
  mutate(property_damage_any = recode_factor(property_damage_any, `1` = "yes", `0` = "no", .ordered = TRUE)) %>%
  group_by(yrmo_date, property_damage_any) %>%
  tally() %>%
  ggplot(aes(yrmo_date, n, fill = property_damage_any)) + 
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(name = "any property\ndamage",
                      values = rev(wes_palette("Royal1")[1:2])) +
  labs(title = "Incidence of property damage at anti-racism events since May 25, 2020",
       caption = "Source: Crowd Counting Consortium")
dev.off()

# counter-protests

# filter down to anti-anti-racism events with pro-Trump/right-wing valence since George Floyd's death
anti_gf <- filter(ccc,
                  grepl("racism", issues),
                  date >= "2020-05-25",
                  grepl("counter.protest|counterprotest", type),
                  valence == 2)

# count these events
nrow(anti_gf)

png("george-floyd-counterprotests-monthly.png", width = 7, height = 3.5, units = "in", res = 300)
anti_gf %>%
  group_by(yrmo_date) %>%
  tally() %>%
  ggplot(aes(yrmo_date, n)) + 
    geom_col(fill = wes_palette("FantasticFox1")[1]) +
    theme_minimal() +
    theme(axis.title = element_blank()) +
    scale_y_continuous(labels = comma) +
    labs(title = "Monthly counts of counter-protests against anti-racism events since May 25, 2020",
         caption = "Source: Crowd Counting Consortium")
dev.off()

## ANIMATED MAP ##

us <- c(left = -125, bottom = 24.5, right = -67, top = 49)
us_map <- get_stamenmap(us, zoom = 5, maptype = "toner-lite")

# create feature with dot sizes for map based on categorical measure of scale;
# had to tinker with these sizes and size of ouput for a bit to get version I liked
george_floyd$mapsize <- with(george_floyd, case_when(size_cat == 0 ~ 1/10,
                                                     size_cat == 1 ~ 1/10,
                                                     size_cat == 2 ~ 1/5,
                                                     size_cat == 3 ~ 1/2,
                                                     size_cat == 4 ~ 1))

gf_map <- ggmap(us_map, extent = "device") +
  geom_point(aes(x = lon, y = lat, size = mapsize),
             data = filter(george_floyd, date < "2020-07-01"),
             alpha = 0.5,
             color = wes_palette("GrandBudapest1")[2],
             show.legend = FALSE)

gf_map_anim <- gf_map + transition_time(date) + labs(caption = 'Date: {frame_time}')

gf_map_anim_gif <- animate(gf_map_anim,
                           fps = 1, duration = 36, # manually set to show 1 map per second, duration = number of days
                           width = 5, height = 3, units = "in", res = 300,
                           renderer = gifski_renderer())

anim_save("ccc_george_floyd_uprising_animated_map.gif", gf_map_anim_gif)
