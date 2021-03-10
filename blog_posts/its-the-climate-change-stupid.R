library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)
library(zoo)

options(stringsAsFactors = FALSE)

data_url <- "https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv"

ccc <- read.csv(data_url)

eco <- filter(ccc, grepl("environment", issues))

## TIME SERIES CHARTS ##

eco_mo <- eco %>%
  mutate(yrmo = lubridate::date(as.yearmon(date))) %>%
  group_by(yrmo) %>%
  summarize(n_events = n(),
            sum_crowds = sum(size_mean, na.rm = TRUE)) %>%
  filter(yrmo <= "2021-02-01")

p_eco_mo_events <- eco_mo %>%
  ggplot(aes(yrmo, n_events)) + 
    geom_col(fill = "forestgreen") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank()) +
    scale_y_continuous(labels = comma) +
    labs(title = "Monthly counts of U.S. environmental protests",
         subtitle = "February 2017-February 2021",
         caption = "Source: Crowd Counting Consortium") +
    annotate("text",
             x = lubridate::date("2018-08-01"),
             y = eco_mo$n_events[eco_mo$yrmo == date("2018-09-01")][1],
             label = "Rise for Climate",
             hjust = "right",
             family = "mono",
             size = 3.5) +
    annotate("text",
             x = lubridate::date("2019-10-01"),
             y = eco_mo$n_events[eco_mo$yrmo == date("2019-09-01")][1],
             label = "Global Week for Future",
             hjust = "left",
             family = "mono",
             size = 3.5) +
    annotate("text",
             x = lubridate::date("2020-01-01"),
             y = eco_mo$n_events[eco_mo$yrmo == date("2019-12-01")][1],
             label = "Youth Climate Strike",
             hjust = "left",
             family = "mono",
             size = 3.5)

p_eco_mo_crowds <- eco_mo %>%
  ggplot(aes(yrmo, sum_crowds)) + 
    geom_col(fill = "forestgreen") +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid = element_blank()) +
    scale_y_continuous(labels = comma) +
    labs(title = "Monthly sums of crowd sizes at U.S. environmental protests",
         subtitle = "February 2017-February 2021",
         caption = "Source: Crowd Counting Consortium") +
    annotate("text",
             x = lubridate::date("2017-05-01"),
             y = eco_mo$sum_crowds[eco_mo$yrmo == date("2017-04-01")][1],
             label = "People's Climate March",
             hjust = "left",
             family = "mono",
             size = 3.5) +
    annotate("text",
             x = lubridate::date("2019-10-01"),
             y = eco_mo$sum_crowds[eco_mo$yrmo == date("2019-09-01")][1],
             label = "Global Week for Future",
             hjust = "left",
             family = "mono",
             size = 3.5)

png("ccc-environmental-protest-trends-by-month.png", width = 7, height = 7, units = "in", res = 300)
p_eco_mo_events / p_eco_mo_crowds
dev.off()

## WORD CLOUD OF CLAIMS ##

library(wordcloud)
library(tidytext)
library(pluralize)

claimwords <- eco %>%
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

png("environmental-protest-claims-wordcloud.png", width = 6, height = 6, units = "in", res = 300)
with(claimwords_tally, wordcloud(word, n,
                                 min.freq = 5,
                                 random.order = FALSE,
                                 scale = c(8,0.5)))
dev.off()
