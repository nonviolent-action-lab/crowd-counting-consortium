## REPLICATION SCRIPT FOR PROTEST UNDER TRUMP, 2017-2021

library(tidyverse)
library(lubridate)
library(zoo)
library(scales)
library(ggmap)
library(data.table)
library(patchwork)
library(usmap)
library(tidycensus)


options(stringsAsFactors = FALSE)
getwd()
setwd("/Users/fifiteklemedhin/Desktop/Research Fall 2023/Fall 2023/crowd-counting-consortium/protest-under-trump-replication-materials")
# DATA INGESTION AND PREP

ccc <- read.csv("data/ccc_compiled_20211229.csv")
# fix class of date field
ccc$date <- lubridate::date(ccc$date)
# replace empty issue tag field with 'other' to allow cleaner labeling and filtering
ccc$issues[ccc$issues == ""] <- "other"
# replace leading 0 in FIPS codes
ccc$fips_code <- with(ccc, ifelse(nchar(fips_code) < 5, paste0("0", fips_code), fips_code))
# reduce to Trump presidency
ccc <- filter(ccc, date <= "2021-01-20")
# remove events with no date or no locality
ccc <- filter(ccc, !is.na(date) & !is.na(locality))
# remove online events
ccc <- filter(ccc, online != 1 & !grepl("online", location_detail) & !grepl("virtual", type))

# get tables with population estimates for counties and states
# ran the following on 2021-08-05 to create files stored in data directory
# source("r/nal_census_api_key.r")
# census_api_key(nal_census_api_key)
# county_pop <- get_acs(geography = "county",
#                       variables = "B01003_001",
#                       year = 2019,
#                       survey = "acs5")
# county_pop <- select(county_pop, fips_code = GEOID, popsize = estimate)
# state_pop <- get_acs(geography = "state",
#                      variables = "B01003_001",
#                      year = 2019,
#                      survey = "acs5")
# my.state.abb <- append(state.abb, c("DC","PR"))
# my.state.name <- append(state.name, c("District of Columbia", "Puerto Rico")) 
# state_pop <- state_pop %>%
#   mutate(state = my.state.abb[match(NAME, my.state.name)]) %>%
#   select(state, popsize = estimate)
# write.csv(county_pop, "data/county_pop.csv", row.names = FALSE)
# write.csv(state_pop, "data/state_pop.csv", row.names = FALSE)
county_pop <- read.csv("data/county_pop.csv")
county_pop$fips_code <- with(county_pop, ifelse(nchar(fips_code) < 5, paste0("0", fips_code), fips_code))
state_pop <- read.csv("data/state_pop.csv")


## STATS IN TEXT

### Sustained Protest in the Trump Years

# How many total events?
nrow(ccc)

# How many millions of participants in those events?
round(sum(ccc$size_low, na.rm = TRUE)/1000000, 1)
round(sum(ccc$size_high, na.rm = TRUE)/1000000, 1)

# Footnote: how many and what share of events have no info on crowd size?
summary(ccc$size_mean)["NA's"]
summary(ccc$size_mean)["NA's"]/nrow(ccc)

# How many and what share of events were anti-Trump?
round(table(ccc$valence)['1'], -2)
round(table(ccc$valence)['1']/nrow(ccc), 2) * 100

# How many participants in those anti-Trump events?
round(sum(ccc[ccc$valence == 1,]$size_low, na.rm = TRUE)/1000000, 1)
round(sum(ccc[ccc$valence == 1,]$size_high, na.rm = TRUE)/1000000, 1)

# How many and what share of events were pro-Trump?
table(ccc$valence)['2']
round(table(ccc$valence)['2']/nrow(ccc), 2) * 100

# How many participants in the pro-Trump events?
round(sum(ccc[ccc$valence == 2,]$size_low, na.rm = TRUE)/1000000, 1)
round(sum(ccc[ccc$valence == 2,]$size_high, na.rm = TRUE)/1000000, 1)

# Which months had the highest and lowest counts of events?
ccc_mo <- ccc %>%
  mutate(yrmo = as.yearmon(date)) %>%
  group_by(yrmo) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE)) %>%
  mutate(date = lubridate::date(yrmo))
sprintf("The peak month was %s, with %s events.",  with(ccc_mo, yrmo[which.max(n)]), max(ccc_mo$n))
sprintf("The lowest month was %s, with %s events.",  with(ccc_mo, yrmo[which.min(n)]), min(ccc_mo$n))

# Which months had the highest and lowest counts of participants?
sprintf("The month with the fewest protesters was %s, with %s.",
        label_date(format = "%B %Y")(with(ccc_mo, yrmo[order(-ccc_mo$size_mean)[nrow(ccc_mo)]])),
        ccc_mo$size_mean[order(-ccc_mo$size_mean)[nrow(ccc_mo)]])
sprintf("The month with the most protesters was %s, with more than %s million.",
        label_date(format = "%B %Y")(with(ccc_mo, yrmo[order(-ccc_mo$size_mean)[1]])),
        round(ccc_mo$size_mean[order(-ccc_mo$size_mean)[1]]/1000000, 1))

# Which months had the 2nd- and 3rd-highest counts of participants?
sprintf("The second-highest month was %s, with %s million participants.",
        label_date(format = "%B %Y")(with(ccc_mo, yrmo[order(-ccc_mo$size_mean)[2]])),
        round(ccc_mo$size_mean[order(-ccc_mo$size_mean)[2]]/1000000, 1))
sprintf("The third-highest month was %s, with %s participants.",
        label_date(format = "%B %Y")(with(ccc_mo, yrmo[order(-ccc_mo$size_mean)[3]])),
        ccc_mo$size_mean[order(-ccc_mo$size_mean)[3]])

# How many events and participants in the January 2017 Women's March?
wm2017 <- filter(ccc, macroevent == "20170121-womensmarch")
nrow(wm2017) # count of events
round(sum(wm2017$size_low, na.rm = TRUE)/1000000, 1) # sum of low ests, in millions
round(sum(wm2017$size_high, na.rm = TRUE)/1000000, 1) # sum of high ests, in millions
label_comma()(round(sum(wm2017$size_mean, na.rm = TRUE), -3)) # sum of mean ests, in millions
length(unique(wm2017$location)) # unique localities
round(sum(wm2017$size_mean < 1000 | is.na(wm2017$size_mean))/nrow(wm2017) * 100, 0) # percentage with < 1K participants

# 2017 WM participation by 2016 presidential election result
trump_states_2016 <- c("AL", "AK", "AZ", "AR", "FL",
                       "GA", "ID", "IN", "IA", "KS",
                       "KY", "LA", "MI", "MS", "MO",
                       "MT", "NE", "NC", "ND", "OH",
                       "OK", "PA", "SC", "SD", "TN",
                       "TX", "UT", "WV", "WI", "WY")
wm2017_trump <- filter(wm2017, state %in% trump_states_2016)
nrow(wm2017_trump)
label_comma()(round(sum(wm2017_trump$size_low, na.rm = TRUE), -3)) # sum of low ests to nearest 1000
label_comma()(round(sum(wm2017_trump$size_high, na.rm = TRUE), -3)) # sum of high ests to nearest 1000

# How many events and participants in some other biggies?
wm2018 <- ccc %>% filter(macroevent == "20180120-womensmarch")
nrow(wm2018)
round(sum(wm2018$size_low, na.rm = TRUE)/1000000, 1) # sum of low ests, in millions
round(sum(wm2018$size_high, na.rm = TRUE)/1000000, 1) # sum of high ests, in millions
m4ol <- ccc %>% filter(macroevent == "20180324-marchforourlives")
nrow(m4ol)
round(sum(m4ol$size_low, na.rm = TRUE)/1000000, 1) # sum of low ests, in millions
round(sum(m4ol$size_high, na.rm = TRUE)/1000000, 1) # sum of high ests, in millions
guns318 <- ccc %>% filter(macroevent == "20180314-nationalschoolwalkout")
label_comma()(nrow(guns318))
round(sum(guns318$size_low, na.rm = TRUE)/1000000, 1) # sum of low ests, in millions
round(sum(guns318$size_high, na.rm = TRUE)/1000000, 1) # sum of high ests, in millions

# How many participants in George Floyd uprising, May 26-July 2, 2020?
gf_nyt <- ccc %>%
  filter(grepl("racism", issues) & valence == 1) %>%
  filter(date >= "2020-05-26" & date <= "2020-07-02")
round(sum(gf_nyt$size_low, na.rm = TRUE)/1000000, 1) # sum of low ests, in millions
round(sum(gf_nyt$size_high, na.rm = TRUE)/1000000, 1) # sum of high ests, in millions

# Which states had the largest crowd totals?
ccc_state_size <- ccc %>%
  group_by(state) %>%
  summarize_at(vars(size_low:size_mean), sum, na.rm = TRUE) %>%
  arrange(-size_mean) %>%
  dplyr::select(state, size_mean) %>%
  filter(state %in% append(state.abb, "DC")) %>% # limit to states plus DC
  mutate(state_name = append(state.name, "District of Columbia")[match(state, append(state.abb, "DC"))])
head(ccc_state_size)
tail(ccc_state_size)

# How about per capita?
ccc_state_size_pop <- left_join(ccc_state_size, state_pop) %>%
  mutate(ratio = size_mean/popsize) %>%
  arrange(-ratio)
head(ccc_state_size_pop)
tail(ccc_state_size_pop)


# PROTEST CLAIMS

# generate data used in bar chart of counts of events by issue...
# get vector of issue labels
issues <- unique(str_trim(unlist(str_split(ccc$issues, ";"))))
# make a table with dummy variables for each issue tag for each event
dat_issues <- setNames(map_dfc(issues, ~as.numeric(grepl(., ccc$issues))), issues)
# bind that table of dummy variables with the valence col from the original
dat_issues <- ccc %>%
  dplyr::select(valence) %>%
  bind_cols(., dat_issues)
# pivot to tidy long format to allow easy tallying with multi-counting for events that have more 
# than one issue tag attached
dat_issues <- pivot_longer(dat_issues, -valence, names_to = "issue", values_to = "status")
# use data.table for fast computation of monthly sums; here, data.table blows dplyr away
dat_issues <- data.table::as.data.table(dat_issues)[, .(tally = sum(status)), by = .(valence, issue)]
# now summarize for whole period
issue_sums <- dat_issues %>%
  group_by(issue) %>%
  summarise(total = sum(tally, na.rm = TRUE))

# by state
dat_issues <- setNames(map_dfc(issues, ~as.numeric(grepl(., ccc$issues))), issues)
# bind that table of dummy variables with the state col from the original
dat_issues_state <- ccc %>%
  dplyr::select(state) %>%
  bind_cols(., dat_issues)
# pivot to tidy long format to allow easy tallying with multi-counting for events that have more 
# than one issue tag attached
dat_issues_state <- pivot_longer(dat_issues_state, -state, names_to = "issue", values_to = "status")
# use data.table for fast computation of monthly sums; here, data.table blows dplyr away
dat_issues_state <- data.table::as.data.table(dat_issues_state)[, .(tally = sum(status)), by = .(state, issue)] %>%
  group_by(state) %>%
  arrange(-tally, .by_group = TRUE)
# function for tallying states with specific issues in top 5
top_issue_tally <- function(issue, n = 5) {

  dat_issues_state %>%
    filter(state %in% state.abb) %>%
    group_by(state) %>%
    slice(1:n) %>%
    group_map(~any(grepl(issue, .$issue))) %>%
    unlist() %>%
    sum()

}
# generate table of counts of states in which each theme was a top-5 concern
issue_state_tally <- sapply(issues, function(x) { top_issue_tally(x) })
print(issue_state_tally)

### GEOGRAPHY OF PROTEST

# generate requisite data
ccc_fips <- ccc %>%
  group_by(fips_code) %>%
  tally() %>%
  left_join(county_pop, .) %>%
  mutate(n = ifelse(is.na(n), 0, n),
         events_per_100k = n/(popsize/100000)) %>%
  arrange(-n) %>%
  mutate(n_cat = case_when(
    n == 0 ~ "0",
    n < 10 ~ "1s",
    n < 100 ~ "10s",
    n < 1000 ~ "100s",
    TRUE ~ "1,000+" )) %>%
  mutate(n_cat = fct_relevel(n_cat, "0", "1s", "10s", "100s", "1,000+")) %>%
  mutate(n_pc_cat = case_when(
    events_per_100k == 0 ~ "0",
    events_per_100k < 10 ~ "< 10",
    events_per_100k >= 10 & events_per_100k < 20 ~ "10-20",
    events_per_100k >= 20 & events_per_100k < 30 ~ "20-30",
    TRUE ~ "30+" )) %>%
  mutate(n_pc_cat = fct_relevel(n_pc_cat, "0", "< 10", "10-20", "20-30", "30+"))

# How many and what share of counties had at least 1 event?
sum(ccc_fips$n > 0)
round(((sum(ccc_fips$n > 0)/3220) * 100), 0)

# Which five counties had the most events?
data(fips_codes)
fips_codes$fips_code <- with(fips_codes, paste0(state_code, county_code))
fips_codes$locality <- with(fips_codes, paste(county, state, sep = ", "))
paste(fips_codes$locality[match(ccc_fips$fips_code[1:5], fips_codes$fips_code)], collapse = "; ")

# How many and what share of counties had 0 events?
sum(ccc_fips$n == 0)
sum(ccc_fips$n == 0)/nrow(ccc_fips)

# How many and what share of counties had exactly 1 event?
sum(ccc_fips$n == 1)
round(100 * (sum(ccc_fips$n == 1)/nrow(ccc_fips)), 0)

# How many events occurred in top two counties?
ccc_fips$n[1:2]

# How many events, participants, and unique localities in March 14, 2018, school walkout?
ccc %>% filter(macroevent == "20180314-nationalschoolwalkout") %>% summarize_at(vars(size_low:size_mean), sum, na.rm = TRUE)
label_comma()(round(sum(ccc$size_mean[ccc$macroevent == "20180314-nationalschoolwalkout"], na.rm = TRUE), -4))
label_comma()(ccc %>% filter(macroevent == "20180314-nationalschoolwalkout") %>% nrow())
label_comma()(length(unique(ccc[ccc$macroevent == "20180314-nationalschoolwalkout",]$location)))

# How many unique localities for anti-racism protests during the period May 26-June 20, 2020?
label_comma()(length(unique(ccc[ccc$date >= "2020-05-26" & ccc$date <= "2020-06-20" & grepl("racism", ccc$issues) & ccc$valence == 1,]$location)))

# What about the time from Trump's inauguration until the murder of George Floyd?
label_comma()(length(unique(ccc[ccc$date < "2020-05-26" & grepl("racism", ccc$issues) & ccc$valence == 1,]$location)))


### PEACEFUL PROTESTS

# How many and what share of events involved any arrests?
label_comma()(with(ccc, sum(arrests_any == 1)))
round((100 * with(ccc, sum(arrests_any == 1))/nrow(ccc)), 1)

# How many and what share of events involved any property damage?
label_comma()(with(ccc, sum(property_damage_any == 1)))
round((100 * with(ccc, sum(property_damage_any == 1))/nrow(ccc)), 1)

# How many and what share of events involved any protester injuries?
label_comma()(with(ccc, sum(injuries_crowd_any == 1)))
round((100* with(ccc, sum(injuries_crowd_any == 1))/nrow(ccc)), 1)

# How many and what share of events involved any police injuries?
label_comma()(with(ccc, sum(injuries_police_any == 1)))
round((100 * with(ccc, sum(injuries_police_any == 1))/nrow(ccc)), 1)

# What about the George Floyd uprising in particular?
george_floyd <- ccc %>%
  # ...events with 'racism' as issue tag AND anti-Trump/left-wing valence...
  filter(grepl("racism", issues) & valence == 1) %>%
  # ...occurring after police kill George Floyd
  filter(date >= "2020-05-25")
# count of events
label_comma()(round(nrow(george_floyd), -3))
# frequency of property damage
table(george_floyd$property_damage_any)[2]
round((100 * table(george_floyd$property_damage_any)[2]/sum(table(george_floyd$property_damage_any))), 1)
# frequency of police injuries
table(george_floyd$injuries_police_any)[2]
round((100 * table(george_floyd$injuries_police_any)[2]/sum(table(george_floyd$injuries_police_any))), 1)
# frequency of protester injuries
table(george_floyd$injuries_crowd_any)[2]
round((100 * table(george_floyd$injuries_crowd_any)[2]/sum(table(george_floyd$injuries_crowd_any))), 1)
# frequency of any arrests
table(george_floyd$arrests_any)[2]
round((100 * table(george_floyd$arrests_any)[2]/sum(table(george_floyd$arrests_any))), 1)


### COUNTER-MOBILIZATION

# no stats, just the figure


## FIGURES ##

# journal specifies that TNR must be used in all figures
#windowsFonts(Times=windowsFont("Times New Roman"))

# Figure 1. Monthly counts of U.S. protest events, January 2017-January 2021
ccc_mo_count_text_dates <- lubridate::date(c("2018-03-01", "2018-04-01", "2020-06-01"))
ccc_mo_count_annotation <- data.frame(date = ccc_mo_count_text_dates,
                                      count = ccc_mo$n[ccc_mo$date %in% ccc_mo_count_text_dates],
                                      tag = c("March for Our Lives", "National School Walkout", "George Floyd Uprising"),
                                      hjust = c(-0.1, -0.1, 1.1),
                                      stringsAsFactors = FALSE)
moplo_counts <- ccc_mo %>%
  mutate(date = lubridate::date(yrmo)) %>%
  ggplot(aes(date, n)) + 
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          text=element_text(family="Times")) +
    scale_y_continuous(labels = comma)
png("figs/fig-1-monthy-event-counts.png", res = 300, width = 7, height = 3.5, units = "in")
moplo_counts + geom_text(data = ccc_mo_count_annotation,
                         aes(x = date, y = count, label = tag),
                         hjust = ccc_mo_count_annotation$hjust,
                         size = 3,
                         family = "Times")
dev.off()

# Figure 2. Monthly sums of participants in U.S. protest events, January 2017-January 2021
ccc_mo_size_text_dates <- lubridate::date(c("2017-01-01", "2018-01-01", "2018-03-01", "2018-06-01", "2020-06-01"))
ccc_mo_size_annotation <- data.frame(date = ccc_mo_size_text_dates,
                                     count = ccc_mo$size_mean[ccc_mo$date %in% ccc_mo_size_text_dates],
                                     tag = c("Women's March", "Women's March", "March for Our Lives", "Pride marches", "George Floyd Uprising"),
                                     hjust = c(-0.1, 1.15, 1.1, -0.15, 1.11),
                                     stringsAsFactors = FALSE)
moplo_size <- ccc_mo %>%
  ggplot(aes(date, size_mean)) + 
    geom_col(position = 'dodge') +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          text=element_text(family="Times")) +
    scale_y_continuous(labels = comma)
png("figs/fig-2-monthy-participant-counts.png", res = 300, width = 7, height = 3.5, units = "in")
moplo_size + geom_text(data = ccc_mo_size_annotation,
                       aes(x = date, y = count, label = tag),
                       hjust = ccc_mo_size_annotation$hjust,
                       size = 3,
                       family = "Times")
dev.off()

# Figure 3. Counts of protest events in the Trump era by political issue
png("figs/fig-3-issues-bar-chart.png", res = 300, width = 6, height = 7, units = "in")
issue_sums %>%
  ggplot(aes(reorder(issue, total, sum), total)) + 
    geom_col(fill = "gray60") +
    theme_minimal() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank(),
          text=element_text(family="Times"))
dev.off()

# Figure 4. Counts of Trump-era protest events by county
png("figs/fig-4-events-per-county-raw.png", res = 300, width = 7, height = 5, units = "in")
plot_usmap(data = dplyr::select(ccc_fips, fips = fips_code, n_cat),
           regions = "counties", 
           values = "n_cat",
           color = "gray75",
           size = 0.05) +
  theme(legend.position = "right",
        text=element_text(family="Times")) +
  scale_fill_brewer(palette = "Greys",
                    guide = "legend",
                    name = "total events")
dev.off()

# Figure 5. Population-weighted rates of Trump-era protest events by county
png("figs/fig-5-events-per-county-per-capita.png", res = 300, width = 7, height = 5, units = "in")
plot_usmap(data = dplyr::select(ccc_fips, fips = fips_code, n_pc_cat),
           regions = "counties", 
           values = "n_pc_cat",
           color = "gray75",
           size = 0.05) +
  theme(legend.position = "right",
        text=element_text(family="Times")) +
  scale_fill_brewer(palette = "Greys",
                    guide = "legend",
                    name = "events per\n100,000 pop.")
dev.off()



# *************************** TODO ********************************************************
# TODO: figure 5a; protests per capita on left and right; 'valence' is left v right
ccc_compiled_present = read_csv("data/ccc_compiled_2021-present.csv")
ccc_protests <- ccc %>% subset(grepl("protest", type, ignore.case = TRUE))

events_per_100k_by_fips_and_valence <- function(df, valence_num)
{
    df <- df %>% subset(valence == valence_num) %>%
    group_by(fips_code) %>%
    tally() %>% # tallies number of times each fip code (county code) shows up
    left_join(county_pop,.) %>% # joins with another table that gets population according to fip code
    mutate(n = ifelse(is.na(n), 0, n), # if count for fip code occurences is NA make it 0???
           events_per_100k = n/(popsize/100000)) %>% # calculates events per 100k ppl
    arrange(-n) %>%
    mutate(n_cat = case_when(
      n == 0 ~ "0",
      n < 10 ~ "1s",
      n < 100 ~ "10s",
      n < 1000 ~ "100s",
      TRUE ~ "1,000+" )) %>%
    mutate(n_cat = fct_relevel(n_cat, "0", "1s", "10s", "100s", "1,000+")) %>%
    mutate(n_pc_cat = case_when(
      events_per_100k == 0 ~ "0",
      events_per_100k < 10 ~ "< 10",
      events_per_100k >= 10 & events_per_100k < 20 ~ "10-20",
      events_per_100k >= 20 & events_per_100k < 30 ~ "20-30",
      TRUE ~ "30+" )) %>%
    mutate(n_pc_cat = fct_relevel(n_pc_cat, "0", "< 10", "10-20", "20-30", "30+"))
  
}


plot_flips_map <- function(df, file_name, map_title, color, palette)
{
 
  png(paste("figs/", file_name, ".png"), res = 300, width = 7, height = 5, units = "in") # specifies where to output file
  print(
    plot_usmap(data = dplyr::select(df, fips = fips_code, n_pc_cat), # selects fips and n_pc_cat data to plot on map
               regions = "counties",
               values = "n_pc_cat", # says that n_pc_cat denotes counties
               color = color,
               size = 0.05) +
      theme(legend.position = "right",
            text=element_text(family="Times")) +
      scale_fill_brewer(palette = palette,
                        guide = "legend",
                        name = paste(map_title)) # labels graph
  )
}



# ******************************** TODO generate graphs for each ideology and type of event
# TODO: get each unique type of event in the events column; can't find a designated list of types online

ccc_event <- ccc %>% subset(grepl("protest", type, ignore.case = TRUE))
events_nonunique_list = c()

append_delimited_events <- function(str){
  delimited <- unlist(strsplit(as.character(tolower(str)), split = "; "))#[[1]]
  print("delimited:" )
  print(delimited)
  print("")
  events_nonunique_list <<- c(events_nonunique_list, delimited)
}

event_types <- as.data.frame(ccc$type)
event_types_list <- apply(event_types, MARGIN = 2, FUN= append_delimited_events)
unique(event_types_list)
events_nonunique_list <- unique(events_nonunique_list)


index_1 <- events_nonunique_list[[1]]
c("hello", index_1)
index_1[[1]]
typeof(index_1)

left_protests_per_fips <- events_per_100k_by_fips_and_valence(df = ccc_protests, valence_num = 1)
plot_flips_map(left_protests_per_fips, file_name ="fig-5a-right-protests-per-county-per-capita", map_title = "Left/anti-Trump protests", color = "grey75", palette = "Blues")
dev.off()

right_protests_per_fips <- events_per_100k_by_fips_and_valence(df = ccc_protests, valence_num = 2)
plot_flips_map(right_protests_per_fips, file_name = "fig-5b-right-protests-per-county-per-capita", map_title = "Right/pro-Trump protests", "grey75", palette = "Reds")
dev.off()


# TODO # ********************************  Try with number of protesters instead of number of protests  # ********************************  

###### PASTED FROM CAT FOR THE PROTESTORS PER CAPITA COUNT


protestors_per_100k_by_fips_and_valence <- function(df, valence_num)
{
  df <- df %>% subset(valence==valence_num) %>%
    group_by(fips_code) %>%
    summarise(protestors = sum(size_mean, na.rm = TRUE)) %>%
    left_join(county_pop, .) %>%
    mutate(protestors = ifelse(is.na(protestors), 0, protestors),
           protestors_per_100k = protestors/(popsize/100000)) %>%
    arrange(-protestors_per_100k) %>%
    mutate(n_pc_cat = case_when(
      protestors_per_100k == 0 ~ "0",
      protestors_per_100k >= 0 & protestors_per_100k < 1 ~ "0-1",
      protestors_per_100k >= 1 & protestors_per_100k < 3 ~ "1-3",
      protestors_per_100k >= 3 & protestors_per_100k < 5 ~ "3-5",
      protestors_per_100k >= 5 & protestors_per_100k < 10 ~ "5-10",
      protestors_per_100k >= 10 & protestors_per_100k < 15 ~ "10-15",
      protestors_per_100k >= 15 & protestors_per_100k < 20 ~ "15-20",
      protestors_per_100k >= 20 & protestors_per_100k < 25 ~ "20-25",
      TRUE ~ "35+" )) %>%
    mutate(n_pc_cat = fct_relevel(n_pc_cat, "0", "0-1", "1-3", "3-5", "5-10", "10-15","15-20","20-25","25+"))
  
  
}


ccc_fips_right_protestors <- protestors_per_100k_by_fips_and_valence(df = ccc_protests, valence_num = 2)
ccc_fips_left_protestors <- protestors_per_100k_by_fips_and_valence(df = ccc_protests, valence_num = 1)

plot_flips_map(ccc_fips_left_protestors, file_name ="fig-5c-left-protestors-per-county", map_title = "Left/anti-Trump protestors per county", color = "grey75", palette = "Blues")
dev.off()
plot_flips_map(ccc_fips_right_protestors, file_name = "fig-5d-right-protestors-per-county", map_title = "Right/pro-Trump protests per county", "grey75", palette = "Reds")
dev.off()

tester <- ccc_fips_right_protestors %>% subset(protestors==0)
# TODO # ********************************  Plot raw_count_right_wing_protests/raw_count_left_wing_protests protests for each county # ******************************** 



national_total_left_protests <- sum(left_protests_per_fips$n)
national_total_right_protests <- sum(right_protests_per_fips$n)
national_percent_right_protests <- national_total_right_protests / (national_total_left_protests + national_total_right_protests) # national proportion of right-wing protests to total protests with valence 1 or 2
print(national_total_left_protests + national_total_right_protests)
print(national_percent_right_protests)

cc_fips_protests <- right_protests_per_fips %>%
  inner_join(left_protests_per_fips, by = "fips_code", suffix = c("_right", "_left"))%>%
  rename(protests_right = n_right,
         protests_left = n_left)%>%
  mutate(total_protests = protests_right + protests_left) %>%
  mutate(percent_right_protests = 100 * protests_right / total_protests) %>%
  mutate(percent_left_protests = 100 *protests_left / total_protests) 

cc_fips_protests <- cc_fips_protests %>%
  mutate(percent_right_protests = ifelse(is.na(percent_right_protests), 0, percent_right_protests)) %>%
  mutate(percent_left_protests = ifelse(is.na(percent_left_protests), 0, percent_left_protests)) 
  
cc_fips_protests <- cc_fips_protests %>%
  select(fips_code, percent_right_protests, protests_right, protests_left, total_protests, everything())



color_cases = c()
num_ranges = c("0")
gradient_requirements = c()
step = 5
for (i in seq(0, 95, by = step)) {
  new_num_range = paste0(i, "-", i+step)
  new_case = substitute(percent_right_protests >= i & percent_right_protests < i_plus_step ~ new_num_range, list(i=i, i_plus_step=i+step, new_num_range = new_num_range))
  
  num_ranges <- append(num_ranges, new_num_range)
  color_cases <- append(color_cases, new_case)
  
}
# Combine the formulas into a single formula string
final_color_cases <- as.formula((paste(color_cases, collapse = " + ")))

# used CHATGPT:
# Define the color range
colors <- c("#4230eb", "#A030A0", "#eb4255","#D40032","#A60027")  # Blue, Purple, Red in hex format, "#4230eb", "#A00180", 

# Create a function to generate the gradient colors
generate_gradient <- colorRampPalette(colors)

# Generate 10 colors in the gradient
gradient_colors <- c("#f7f8fa")
gradient_colors <- append(gradient_colors, generate_gradient(length(num_ranges)-1)) # did -1 since already predefined a color for 0, which is white


gradient_requirements <- setNames(gradient_colors, num_ranges)




plot_flips_map_gradient <- function(df, file_name, map_title, color, gradient_requirements)
{
  z1 = 10
  
  # Display the gradient colors
  gradient_colors
  png(paste("figs/", file_name, ".png"), res = 300, width = 7, height = 5, units = "in") # specifies where to output file
  print(
    plot_usmap(data = dplyr::select(df, fips = fips_code, n_pc_cat), # selects fips and n_pc_cat data to plot on map
               regions = "counties",
               values = "n_pc_cat", # says that n_pc_cat denotes counties
               color = color,
               size = 0.05) +
      theme(legend.position = "right",
            text=element_text(family="Times")) +
      
      scale_fill_manual(
        values = gradient_requirements,
        na.value = "grey50"
      )
  )
}




cc_fips_protests <- cc_fips_protests %>% 
  arrange(-percent_right_protests) %>%
  #mutate(n_pc_cat = case_when(percent_right_protests >= 0 & percent_right_protests < 110 ~ "0-10", final_color_cases, TRUE ~ "80+"  )) %>%
  mutate(n_pc_cat_right = case_when(
    percent_right_protests > 0 & percent_right_protests < 5 ~ "0-5" , 
      percent_right_protests >= 5 & percent_right_protests < 10 ~ 
      "5-10" , percent_right_protests >= 10 & percent_right_protests < 
      15 ~ "10-15" , percent_right_protests >= 15 & percent_right_protests < 
      20 ~ "15-20" , percent_right_protests >= 20 & percent_right_protests < 
      25 ~ "20-25" , percent_right_protests >= 25 & percent_right_protests < 
      30 ~ "25-30" , percent_right_protests >= 30 & percent_right_protests < 
      35 ~ "30-35" , percent_right_protests >= 35 & percent_right_protests < 
      40 ~ "35-40" , percent_right_protests >= 40 & percent_right_protests < 
      45 ~ "40-45" , percent_right_protests >= 45 & percent_right_protests < 
      50 ~ "45-50" , percent_right_protests >= 50 & percent_right_protests < 
      55 ~ "50-55" , percent_right_protests >= 55 & percent_right_protests < 
      60 ~ "55-60" , percent_right_protests >= 60 & percent_right_protests < 
      65 ~ "60-65" , percent_right_protests >= 65 & percent_right_protests < 
      70 ~ "65-70" , percent_right_protests >= 70 & percent_right_protests < 
      75 ~ "70-75" , percent_right_protests >= 75 & percent_right_protests < 
      80 ~ "75-80" , percent_right_protests >= 80 & percent_right_protests < 
      85 ~ "80-85" , percent_right_protests >= 85 & percent_right_protests < 
      90 ~ "85-90" , percent_right_protests >= 90 & percent_right_protests < 
      95 ~ "90-95" , percent_right_protests >= 95 & percent_right_protests <= 
      100 ~ "95-100",
    TRUE ~ "0"
  ))%>%
  mutate(n_pc_cat_right = fct_relevel(n_pc_cat_right, "0", num_ranges, "90+"))

  

percent_right_fips_protests <- cc_fips_protests %>% 
  select(fips_code, n_pc_cat_right) 
colnames(percent_right_fips_protests)[2] <- "n_pc_cat"

plot_flips_map_gradient(df = percent_right_fips_protests, file_name ="fig-5e", map_title = "national proportion of right-wing to total protests", color = "grey75", gradient_requirements = gradient_requirements)
dev.off()



has_protests <- cc_fips_protests %>%
  filter(total_protests > 0)
fivenum(has_protests$percent_left_protests)
fivenum(has_protests$percent_right_protests)
print(19897 + 2224) #

#right_protests <- 
print(2224/22121) # proportion of right-wing protests to total 





# if rawcountrightwingprotests/rawcountleftwing protests for each county
# national midpoint will be 2224/22121; proprotion of right-wing protests to total
# counties that are higher than national midpoint are right-wing

# for coloring use a gradient: get min non-0 proportion = blue and max proportion = red, national midpoint = purple, 0 is white 




# Figure 6. Incidences of arrests, protester injuries, police injuries, and property damage at U.S. protests by year, 2017-2020
p_arrests <- ccc %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(arrests_any = ifelse(arrests_any == 1, "yes", "no")) %>%
  group_by(year, arrests_any) %>%
  tally() %>%
  ggplot(aes(year, n, fill = arrests_any)) + 
    geom_col() +
    scale_fill_manual(values = c("yes" = "gray25",
                                 "no" = "gray85"),
                      name = element_blank()) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          text=element_text(family="Times")) +
    scale_y_continuous(labels = comma) +
    labs(title = "any arrests") +
    ylab("event count")
p_protinj <- ccc %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(injuries_crowd_any = ifelse(injuries_crowd_any == 1, "yes", "no")) %>%
  group_by(year, injuries_crowd_any) %>%
  tally() %>%
  ggplot(aes(year, n, fill = injuries_crowd_any)) + 
    geom_col() +
    scale_fill_manual(values = c("yes" = "gray25",
                                 "no" = "gray85"),
                      name = element_blank()) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          text=element_text(family="Times")) +
    scale_y_continuous(labels = comma) +
    labs(title = "any protester injuries") +
    ylab("event count")
p_copinj <- ccc %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(injuries_police_any = ifelse(injuries_police_any == 1, "yes", "no")) %>%
  group_by(year, injuries_police_any) %>%
  tally() %>%
  ggplot(aes(year, n, fill = injuries_police_any)) + 
    geom_col() +
    scale_fill_manual(values = c("yes" = "gray25",
                                 "no" = "gray85"),
                      name = element_blank()) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          text=element_text(family="Times")) +
    scale_y_continuous(labels = comma) +
    labs(title = "any police injuries") +
    ylab("event count")
p_propdam <- ccc %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(property_damage_any = ifelse(property_damage_any == 1, "yes", "no")) %>%
  group_by(year, property_damage_any) %>%
  tally() %>%
  ggplot(aes(year, n, fill = property_damage_any)) + 
    geom_col() +
    scale_fill_manual(values = c("yes" = "gray25",
                                 "no" = "gray85"),
                      name = element_blank()) +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(hjust = 0.5),
          panel.grid.minor = element_blank(),
          text=element_text(family="Times")) +
    scale_y_continuous(labels = comma) +
    labs(title = "any property damage") +
    ylab("event count")
png("figs/fig-6-incidence-of-arrests-and-such.png", res = 300, width = 6, height = 8, units = "in")
p_arrests / p_protinj / p_copinj / p_propdam
dev.off()

# Figure 7. Daily counts of right-wing protest events around three major themes, January 2020-January 2021
# data frame of relevant dates to use as scaffolding to get all the bits on a common scale for plotting
rw_dates <- data.frame(date = seq(from = lubridate::date("2020-01-01"), to = lubridate::date("2021-01-20"), by = "day"))
# Stop the Steal
sts <- ccc %>%
  filter(date > "2020-11-03") %>%
  filter(valence == 2) %>%
  filter(grepl("democracy", issues) & grepl("executive", issues)) %>%
  mutate(date = lubridate::date(date)) %>%
  group_by(date) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE)) %>%
  left_join(rw_dates, .)
sts[is.na(sts)] <- 0
# reopen and related actions against COVID public health measures
reopen <- ccc %>%
  filter(grepl("covid", issues) & valence == 2) %>%
  group_by(date) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE)) %>%
  left_join(rw_dates, .)
reopen[is.na(reopen)] <- 0
# Back the Blue actions
btb <- ccc %>%
  filter(grepl("policing", issues) & valence == 2) %>%
  group_by(date) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE)) %>%
  left_join(rw_dates, .)
btb[is.na(btb)] <- 0
p_reopen <- ggplot(reopen, aes(date, n)) + 
  geom_col() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Times")) +
  labs(title = "Against COVID mandates") +
  ylim(0,110)  
p_btb <- ggplot(btb, aes(date, n)) + 
  geom_col() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Times")) +
  labs(title = "For police and law enforcement") +
  ylim(0,110)  
p_sts <- ggplot(sts, aes(date, n)) + 
  geom_col() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Times")) +
  labs(title = "Against alleged election fraud") +
  ylim(0,110)  
png("figs/fig-7-right-wing-countermobilization-2020.png", res = 300, width = 6.5, height = 5, units = "in")
p_reopen / p_btb / p_sts
dev.off()
