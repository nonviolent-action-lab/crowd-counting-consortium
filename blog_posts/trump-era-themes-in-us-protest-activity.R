library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)
library(zoo)
library(data.table)
library(wordcloud)
library(tidytext)
library(pluralize)

options(stringsAsFactors = FALSE)

# read the data from the Github repo, to ensure that everyone using this notebook is working from the same file
ccc <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv")

# convert the date column to proper type
ccc$date <- lubridate::date(ccc$date)

# replace empty issue tag field with 'other' to allow cleaner labeling and filtering
ccc$issues[ccc$issues == ""] <- "other"

# create a year-month feature to use in time series, and then a date version of this to play nicely with ggplot
ccc$yrmo <- zoo::as.yearmon(ccc$date)
ccc$yrmo_date <- lubridate::date(ccc$yrmo)

# reduce to Trump era for this analysis
ccc <- filter(ccc, date <= "2021-01-20")

## WORD CLOUD ##

claimwords <- ccc %>%
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
  mutate(word = pluralize::singularize(word))

claimwords_tally <- claimwords %>%
  group_by(word) %>%
  tally() %>%
  arrange(-n)

png("trump_era_claims_wordcloud.png", width = 6, height = 6, units = "in", res = 300)
with(claimwords_tally, wordcloud(word, n,
                                 min.freq = 10,
                                 random.order = FALSE,
                                 scale = c(8,0.5)))
dev.off()

## ISSUE TAG COLUMN CHART ##

# get a vector of unique issue labels from the data
issues <- unique(str_trim(unlist(str_split(ccc$issues, ";"))))

# make a table with dummy variables for each issue tag for each event
dat_issues <- setNames(map_dfc(issues, ~as.numeric(grepl(., ccc$issues))), issues)

# bind that table of dummy variables with the yrmo_date col from the original
dat_issues <- bind_cols(select(ccc, yrmo_date), dat_issues)

# pivot to tidy long format to allow easy tallying with multi-counting for events that have more 
# than one issue tag attached
dat_issues <- pivot_longer(dat_issues, -yrmo_date, names_to = "issue", values_to = "status")

# use data.table for fast computation of monthly sums; here, data.table blows dplyr away
dat_issues_mo <- as.data.table(dat_issues)[, .(tally = sum(status)), by = .(yrmo_date, issue)]

# now summarize for whole period
issue_sums <- dat_issues_mo %>%
  group_by(issue) %>%
  summarise(total = sum(tally, na.rm = TRUE))

png("trump_era_issues_barchart.png", width = 7, height = 5, units = "in", res = 300)
issue_sums %>%
  mutate(issue = fct_reorder(issue, -total)) %>%
  ggplot(aes(issue, total)) + 
    geom_col() +
    theme_minimal() +
    scale_y_continuous(labels = comma) +
    labs(title = "Counts of Trump-era protest events by political issue",
         caption = "Data source: Crowd Counting Consortium") +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1),
          panel.grid = element_blank())
dev.off()

## SMALL MULTIPLES BY ISSUE AND MONTH ##

png("trump_era_issues_small_multiples.png", width = 8, height = 10, units = "in", res = 300)
dat_issues_mo %>%
  ggplot(aes(yrmo_date, tally)) +
    geom_line() +
    scale_y_continuous(labels = comma) +
    facet_wrap(vars(issue),
               ncol = 4,
               scales = "free_y",
               strip.position = "top",
               as.table = TRUE) +
    theme_minimal() +
    theme(axis.title = element_blank())
dev.off()
