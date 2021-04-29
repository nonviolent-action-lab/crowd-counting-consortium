library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)
library(zoo)
library(data.table)
library(wordcloud)
library(tidytext)
library(pluralize)
library(treemap)

options(stringsAsFactors = FALSE)

# read the data from the Github repo
ccc <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv")

# convert the date column to proper type
ccc$date <- lubridate::date(ccc$date)

# replace empty issue tag field with 'other' to allow cleaner labeling and filtering
ccc$issues[ccc$issues == ""] <- "other"

# create a year-month feature to use in time series, and then a date version of this to play nicely with ggplot
ccc$yrmo <- zoo::as.yearmon(ccc$date)
ccc$yrmo_date <- lubridate::date(ccc$yrmo)

# reduce to Biden administration
biden <- filter(ccc, date >= "2021-01-20")

## STATS ##

nrow(biden)
table(biden$valence)
table(biden$valence)/nrow(biden)

# occurence of racism or policing on left
biden %>% filter(valence == 1 & (grepl("racism", issues) | grepl("policing", issues))) %>% nrow()
(biden %>% filter(valence == 1 & (grepl("racism", issues) | grepl("policing", issues))) %>% nrow())/table(biden$valence)["1"]

# trump frequency
biden %>% filter(valence == 2 & grepl("Trump", claims)) %>% nrow()


## ISSUE TREE MAP ##

# get a vector of unique issue labels from the data
issues <- unique(str_trim(unlist(str_split(biden$issues, ";"))))

# make a table with dummy variables for each issue tag for each event
dat_issues <- setNames(map_dfc(issues, ~as.numeric(grepl(., biden$issues))), issues)

# bind that table of dummy variables with the yrmo_date col from the original
dat_issues <- bind_cols(select(biden, valence), dat_issues)

# pivot to tidy long format to allow easy tallying with multi-counting for events that have more 
# than one issue tag attached
dat_issues <- pivot_longer(dat_issues, -valence, names_to = "issue", values_to = "status")

# use data.table for fast computation of monthly sums; here, data.table blows dplyr away
dat_issues_valence <- as.data.table(dat_issues)[, .(tally = sum(status)), by = .(valence, issue)]

png("biden-100-treemap-right.png",
  res = 300, width = 6, height = 6, units = "in")
treemap(filter(dat_issues_valence, valence == 2),
               index = "issue",
               vSize = "tally",
               title = "Issues raised at right-wing events in Biden's first 100 days",
               algorithm = "squarified")
dev.off()

png("biden-100-treemap-left.png",
  res = 300, width = 6, height = 6, units = "in")
treemap(filter(dat_issues_valence, valence == 1),
               index = "issue",
               vSize = "tally",
               title = "Issues raised at left-wing events in Biden's first 100 days",
               algorithm = "squarified")
dev.off()

png("biden-100-treemap-other.png",
  res = 300, width = 6, height = 6, units = "in")
treemap(filter(dat_issues_valence, valence == 0),
               index = "issue",
               vSize = "tally",
               title = "Issues raised at nonpartisan events in Biden's first 100 days",
               algorithm = "squarified")
dev.off()

## WORD CLOUDS ##

cloud_dat_by_valence <- function(x = 1, my_data = biden) {

  my_data %>%
    filter(valence == x) %>%
    # grab the col with the summaries of protester claims
    select(claims) %>%
    # create tidy text version with one word per row (other cols preserved by default)
    tidytext::unnest_tokens(word, claims) %>%
    # remove stop words
    anti_join(stop_words) %>%
    # other cleanup
    mutate(word = gsub("\'s|\\banti|[[:punct:]]", "", word)) %>%
    filter(!(word %in% c("for", "against", "pro", "anti", "support", "rallying", "protest", "protesting", "during")),
           !(grepl("\\d", word)),
           nchar(word) > 2,
           !is.na(word)) %>%
    # convert any pluralized words to singular (this takes a bit to run)
    mutate(word = pluralize::singularize(word)) %>%
    group_by(word) %>%
    tally() %>%
    arrange(-n)

}

claims_left <- cloud_dat_by_valence(1)
claims_right <- cloud_dat_by_valence(2)
claims_other <- cloud_dat_by_valence(0)

png("biden-100-wordcloud-left.png",
  res = 300, width = 8, height = 8, units = "in")
with(claims_left, wordcloud(word, n,
                            min.freq = 5,
                            random.order = FALSE,
                            scale = c(8,0.5)))
dev.off()

png("biden-100-wordcloud-right.png",
  res = 300, width = 8, height = 8, units = "in")
with(claims_right, wordcloud(word, n,
                             min.freq = 5,
                             random.order = FALSE,
                             scale = c(8,0.5)))
dev.off()

png("biden-100-wordcloud-other.png",
  res = 300, width = 8, height = 8, units = "in")
with(claims_other, wordcloud(word, n,
                             min.freq = 5,
                             random.order = FALSE,
                             scale = c(8,0.5)))
dev.off()

