library(tidyverse)
library(scales)
library(wesanderson)
library(tidytext)
library(pluralize)
library(wordcloud)
library(zoo)
library(patchwork)

options(stringsAsFactors = FALSE)

my_year <- 2022

my_path <- "nval/blog-posts"

chart_stump <- sprintf("%s/ccc-annual-%s-right-", my_path, my_year)

# regular expressions for selected types of protester actions; to apply to
# 'participant_measures' field from 2020 forward
regex_armed <- "(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm"
regex_scuffle <- "scuffle|push|shove|bump"
regex_assault <- "assault|punch|kick|attack|shot|stab|struck|brawl|fight|fought"

# regular expression for recognizing elements of comma-separated 'claims' field that are
# coder summaries rather than verbatim claims; for application to 'claims' field from 
# 2022 forward, usually to negate to get to verbatim claims like...
# claims <- str_trim(unlist(str_split(ccc[ccc$date >= "2022-01-01",]$claims, ",")))
# verbatim_claims <- claims[!grepl(regex_claimsum, claims, ignore.case = TRUE)]
regex_claimsum <- "^(?:for|against) |^in [[:alpha:]]{3,} (?:of|with)"

# data ingest

ccc_path <- "c:/users/ulfel/documents/github/crowd-counting-consortium/ccc_compiled.csv"

ccc <- read.csv(ccc_path) %>%
  mutate(date = lubridate::date(date),
         fips_code = ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code),
         issues = ifelse(issues == "", "other", issues)) %>%
  filter(!is.na(date))

df_current <- filter(ccc, valence == 2 & lubridate::year(date) == my_year)

df_prev <- filter(ccc, valence == 2 & lubridate::year(date) == my_year - 1)


## COUNTS ##

# how many events?
nrow(df_current)
nrow(df_current)/nrow(df_prev)
nrow(df_prev)

# how many towns?
length(unique(with(df_current, paste(resolved_locality, state, sep = ", "))))
length(unique(with(df_current, paste(resolved_locality, state, sep = ", "))))/length(unique(with(df_prev, paste(resolved_locality, state, sep = ", "))))

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

# which was the largest so far?
df_current[which.max(df_current$size_mean),][,c("date", "locality", "state", "location_detail", "size_text", "claims")]

# how many with arrests?
sum(df_current$arrests_any == 1)
sum(df_current$arrests_any == 1)/nrow(df_current)
sum(df_prev$arrests_any == 1)
sum(df_prev$arrests_any == 1)/nrow(df_prev)

# injuries?
sum(df_current$injuries_crowd_any)
sum(df_current$injuries_crowd_any)/nrow(df_current)
sum(df_prev$injuries_crowd_any)
sum(df_prev$injuries_crowd_any)/nrow(df_prev)

# how many with property damage?
sum(df_current$property_damage_any == 1)
sum(df_current$property_damage_any == 1)/nrow(df_current)
sum(df_prev$property_damage_any == 1)
sum(df_prev$property_damage_any == 1)/nrow(df_prev)

# armed?
nrow(filter(df_current, grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE)))
nrow(filter(df_current, grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE)))/nrow(df_current)
nrow(filter(df_prev, grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE)))
nrow(filter(df_prev, grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE)))/nrow(df_prev)

# what about trump rallies?
trumpus <- filter(df_current, grepl("feat. donald trump(?! jr)", notes, perl = T, ignore.case = T) & !grepl("virtual", type))
nrow(trumpus)
mean(trumpus$size_mean, na.rm = TRUE)
sum(is.na(trumpus$size_mean))
trumpus_2021 <- filter(df_prev, grepl("feat. (?:donald|president) trump(?! jr)", notes, perl = T, ignore.case = T) & !grepl("virtual", type))
nrow(trumpus_2021)
mean(trumpus_2021$size_mean, na.rm = TRUE)
sum(is.na(trumpus_2021$size_mean))
trumpus_2020 <- filter(ccc, lubridate::year(date) == 2020 & valence == 2 & grepl("feat. (?:donald|president) trump(?! jr)", notes, perl = T, ignore.case = T) & !grepl("virtual", type))
nrow(trumpus_2020)
mean(trumpus_2020$size_mean, na.rm = TRUE)
sum(is.na(trumpus_2020$size_mean))


## THEMES ##

# issue tag bar chart

# get vector of issue tags
issues <- sort(unique(str_trim(unlist(strsplit(ccc$issues, ";")))))

# generate and plot summary data
df_current_issue_counts <- map_dfr(issues, ~data.frame(issue = ., n = sum(grepl(., df_current$issues))) )

png(paste0(chart_stump, "issue-counts.png"), res = 300, width = 6, height = 7, units = "in")
df_current_issue_counts %>%
  arrange(-n) %>%
  ggplot(aes(reorder(issue, n, sum), n)) + 
    geom_col(fill = "gray60") +
    theme_minimal() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(title = "Counts of right-wing U.S. protest events in 2022\nby themes in protester claims",
         caption = "Source: Crowd Counting Consortium")
dev.off()
print(arrange(df_current_issue_counts, -n))

# word cloud of verbatim claims

# get vector of verbatim claims
claims <- str_trim(unlist(str_split(df_current[df_current$date >= "2022-01-01",]$claims, ",")))
verbatim_claims <- claims[!grepl(regex_claimsum, claims, ignore.case = TRUE)]

# generate counts for word cloud
df_words <- data.frame(claims = verbatim_claims) %>%
    unnest_tokens(word, claims) %>%
    # remove stop words
    anti_join(stop_words) %>%
    # other cleanup
    mutate(word = gsub("\'s|\\banti|[[:punct:]]", "", word)) %>%
    filter(word != "strikethrough") %>%  # used to interpret poster imagery, not in slogans
    filter(!is.na(word)) %>%
    filter(nchar(word) > 2) %>%
    # convert pluralized words to singular
    # mutate(word = singularize(word)) %>%
    group_by(word) %>%
    tally() %>%
    arrange(-n)

# create word cloud
png(paste0(chart_stump, "claims-wordcloud.png"), res = 300, width = 8, height = 8, units = "in")
layout(matrix(c(1, 2), nrow=2), heights=c(1, 7))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Relative frequencies of terms in claims made\nat right-wing protests in 2022", cex = 1.5)
with(df_words, wordcloud(word, n,
                         min.freq = 10,
                         random.order = FALSE,
                         scale = c(6,1/2)))
dev.off()
nrow(df_words)

# n-grams; see https://bookdown.org/Maxine/tidy-text-mining/tokenizing-by-n-gram.html

df_bigrams <- data.frame(claims = verbatim_claims) %>%
  unnest_tokens(bigram, claims, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  filter(!is.na(bigram)) %>%
  filter(!grepl("\\bto\\b|\\ba\\b|\\bthe\\b", bigram, ignore.case = TRUE))

df_trigrams <- data.frame(claims = verbatim_claims) %>%
  unnest_tokens(trigram, claims, token = "ngrams", n = 3) %>%
  count(trigram, sort = TRUE) %>%
  filter(!is.na(trigram)) %>%
  filter(!grepl("\\bto\\b|\\ba\\b|\\bthe\\b", bigram, ignore.case = TRUE))

df_phrases <- str_trim(unlist(str_split(verbatim_claims, " / "))) %>%
  data.frame(claims = .) %>%
  # some clean up to make phrases comparable
  mutate(claims = tolower(claims)) %>%  # all lowercase
  mutate(claims = gsub("\\&|\\+", "and", claims)) %>%  # make all forms of 'and' identical
  mutate(claims = gsub("[^[:alnum:][:space:]'-\\#]", "", claims)) %>%  # remove most punctuation
  mutate(claims = gsub("\\[.{1,}\\]", "", claims)) %>%  # remove coder representations of images
  mutate(claims = gsub("flags", "flag", claims)) %>%
  mutate(claims = gsub("fuck biden", "fuck joe biden", claims)) %>%
  mutate(claims = gsub("thin blue line", "thin blue line flag", claims)) %>%
  mutate(claims = gsub("^maga$", "make america great again", claims)) %>%
  count(claims, sort = TRUE) %>%
  filter(!is.na(claims) & claims != "") 

png(paste0(chart_stump, "top-20-claims.png"),
  res = 300, width = 6.5, height = 8, units = "in")
df_phrases %>%
 slice(1:20) %>%
  arrange(-n) %>%
  ggplot(aes(reorder(claims, n, sum), n)) + 
    geom_col(fill = "gray60") +
    theme_minimal() +
    coord_flip() +
    scale_y_continuous(labels = comma) +
    theme(axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(title = "20 most common phrases at right-wing protests in 2022",
         caption = "Source: Crowd Counting Consortium",
         y = "count of events at which phrase was seen")
dev.off()
print(df_phrases[1:20,])


# chart of trend in anti-lgbtq+ events

# make table of right-wing actions with anti-LGBTQ+ claims with categorical indicators for
# various conditions of interest and a year-month var for grouping
antiqueer <- ccc %>%
  filter(grepl("lgbtq", issues) & valence == 2) %>%
  # create year-month var to use for grouping
  mutate(yrmo = as.yearmon(date)) %>%
  # various categorical measures for crosstabbing
  mutate(guns = ifelse(grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         other_weapons = ifelse(grepl("kni(?:fe|ves)|mace|pepper spray|clubs?|\\bbats?\\b|baton|rocks", participant_measures, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         other_gear = ifelse(grepl("tactical gear|shield", participant_measures, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         verbal_threats = ifelse(grepl("threat(?:s|ened|\\b)", participant_measures, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         fighting = ifelse(grepl("scuffl|brawl|f(?:i|ou)ght|assault|attack(ed)?|punched|kicked", participant_measures, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         vehicle_ramming = ifelse(grepl("vehicle ramming", participant_measures), "yes", "no"),
         healthcare = ifelse(grepl("healthcare", issues), "yes", "no"),
         schools = ifelse(grepl("school|university|college|campus|student|teacher", claims), "yes", "no"),
         libraries = ifelse(grepl("librar(?:y|ies|ian)", claims, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         sports = ifelse(grepl("sports", issues), "yes", "no"),
         proud_boys = ifelse(grepl("proud boys", actors, perl = TRUE, ignore.case = TRUE) | grepl("proud boys", organizations, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         trans = ifelse(grepl("\\btrans(gender)?\\b|gender.affirming", claims, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         dq = ifelse(grepl("\\bdrag\\b", claims, perl = TRUE, ignore.case = TRUE), "yes", "no"),
         campaign_rally = ifelse(grepl("for (?:governor|office|(u\\.?s\\.? )?(?:senate|congress)|state legislature|(?:city|county) council|school board)", claims, perl = TRUE, ignore.case = TRUE) & grepl("rally", type), "yes", "no"))

png(paste0(chart_stump, "antilgbtq-monthly-counts-drag.png"), res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
antiqueer %>%
  filter(date <= "2022-12-31") %>%
  mutate(yrmo = as.yearmon(date)) %>%
  group_by(yrmo, dq) %>%
  tally() %>%
  mutate(date = as.Date(yrmo)) %>%
  ggplot(aes(date, n, fill = dq)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Monthly counts of anti-LGBTQ+ demonstrations in the U.S., 2017\u20132022",
         caption = "Source: Crowd Counting Consortium") +
    scale_fill_manual(values = wes_palette("Royal1"),
                      name = "targeting\ndrag show\nor Drag\nStory Hour?")
dev.off()
antiqueer %>% filter(date <= "2022-12-31") %>% mutate(yrmo = as.yearmon(date)) %>% group_by(yrmo, dq) %>% tally() %>% tail(.)


## DYNAMICS ##

# basic monthly count charts

png(paste0(chart_stump, "monthly-event-counts.png"), res = 300, width = 16/2, height = 9/2, units = "in")
df_current %>%
  mutate(yrmo = as.yearmon(date)) %>%
  group_by(yrmo) %>%
  tally() %>%
  mutate(date = lubridate::date(yrmo)) %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b") +
    scale_y_continuous(labels=comma) +
    theme(axis.title = element_blank(),
          panel.grid = element_blank()) +
    labs(title = "Monthly counts of right-wing U.S. protest events in 2022",
         caption = "Source: Crowd Counting Consortium")
dev.off()
df_current %>% mutate(yrmo = as.yearmon(date)) %>% group_by(yrmo) %>% tally() %>% print(.)

png(paste0(chart_stump, "monthly-crowd-sums.png"), res = 300, width = 16/2, height = 9/2, units = "in")
df_current %>%
  mutate(yrmo = as.yearmon(date)) %>%
  group_by(yrmo) %>%
  summarize(across(size_low:size_mean, ~ sum(.x, na.rm = TRUE))) %>%
  mutate(date = lubridate::date(yrmo)) %>%
  ggplot(aes(date, size_mean)) +
    geom_col() +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b") +
    scale_y_continuous(labels=comma) +
    theme(axis.title = element_blank(),
          panel.grid = element_blank()) +
    labs(title = "Monthly sums of obseXed crowd sizes at right-wing U.S. protest events in 2022",
         caption = "Source: Crowd Counting Consortium")
dev.off()
df_current %>% mutate(yrmo = as.yearmon(date)) %>% group_by(yrmo) %>% summarize(across(size_low:size_mean, ~ sum(.x, na.rm = TRUE))) %>% print(.)

# issue cycling chart

# data frame of months to use as scaffolding to get all the bits on a common scale for plotting
rw_dates <- data.frame(month = seq(from = lubridate::date("2020-01-01"), to = lubridate::date("2022-12-01"), by = "month"))

# Stop the Steal
sts <- ccc %>%
  filter(valence == 2) %>%
  filter(grepl("democracy", issues) & grepl("presiden", issues)) %>%
  mutate(yrmo = as.yearmon(date),
         month = lubridate::date(yrmo)) %>%
  group_by(month) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE)) %>%
  left_join(rw_dates, .)
sts[is.na(sts)] <- 0

# covid
covid <- ccc %>%
  filter(grepl("covid", issues) & valence == 2) %>%
  mutate(yrmo = as.yearmon(date),
         month = lubridate::date(yrmo)) %>%
  group_by(month) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE)) %>%
  left_join(rw_dates, .)
covid[is.na(covid)] <- 0

# cops and racism
cops <- ccc %>%
  filter((grepl("policing", issues) | grepl("racism", issues)) & valence == 2) %>%
  mutate(yrmo = as.yearmon(date),
         month = lubridate::date(yrmo)) %>%
  group_by(month) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE)) %>%
  left_join(rw_dates, .)
cops[is.na(cops)] <- 0

# lgbtq
lgbtq <- ccc %>%
  filter(grepl("lgbtq", issues) & valence == 2) %>%
  mutate(yrmo = as.yearmon(date),
         month = lubridate::date(yrmo)) %>%
  group_by(month) %>%
  summarize(n = n(),
            size_low = sum(size_low, na.rm = TRUE),
            size_high = sum(size_high, na.rm = TRUE),
            size_mean = sum(size_mean, na.rm = TRUE)) %>%
  left_join(rw_dates, .)
lgbtq[is.na(lgbtq)] <- 0

# plotting function
issue_plot <- function(df, my_ymax) {

  require(ggplot2)

  df %>%
    ggplot(aes(month, n)) + 
      geom_col() +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid = element_blank()) +
      ylim(0,my_ymax) +
      scale_x_date(date_labels = "%Y")

}

p_covid <- issue_plot(covid, 800)
p_cops <- issue_plot(cops, 800)
p_sts <- issue_plot(sts, 800)
p_lgbtq <- issue_plot(lgbtq, 800)

png(paste0(chart_stump, "issue-cycling.png"), res = 300, width = 6.5, height = 8, units = "in")
p_themes <- p_covid / p_cops / p_sts / p_lgbtq
p_themes + plot_annotation(
  title = "Monthly counts of right-wing U.S. protest events by selected themes",
  subtitle = "January 2020\u2013December 2022",
  caption = "Source: Crowd Counting Consortium",
  tag_levels = list(c("COVID", "Police or\nRacism", "Democracy and\nPresidency", "LGBTQ+"))
) & theme(
  plot.tag = element_text(size = 10),
  plot.tag.position = "right"
)
dev.off()


## EXTREMIST GROUPS ##

farright_regexes <- c(`Proud Boys` = "proud boys",
                      `Oath Keepers` = "oath keepers",
                      `Three Percenters` = "three percenters|\\biii|threeper",
                      `Boogaloo Bois` = "boog(aloo)?",
                      `Patriot Front` = "patriot front",
                      `White Lives Matter` = "white lives matter",
                      `Goyim Defense League` = "goyim defense league|gdl",
                      `NSC-131` = "national socialist club|nsc.131",
                      `Gays Against Groomers` = "gays against groomers",
                      `Protect Texas Kids` = "protect texas kids")

farright_counts <- map_dfr(farright_regexes, function(x) {

   years <- c(2020:2022)

   counts <- sapply(years, function(yr) {

     df_current <- filter(ccc, lubridate::year(date) == yr & (grepl(x, organizations, perl = TRUE, ignore.case = TRUE) | grepl(x, actors, perl = TRUE, ignore.case = TRUE)))

     nrow(df_current)

   })

  setNames(counts, years)
   
})

farright_counts <- farright_counts %>%
  mutate(group = names(farright_regexes)) %>%
  pivot_longer(!group, names_to = "year", values_to = "n")

# plotting function
groupcount_plot <- function(groupname, my_ymax) {

  require(ggplot2)

  farright_counts %>%
    filter(group == groupname) %>%
    ggplot(aes(year, n)) + 
      geom_col(width = 2/3) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            panel.grid = element_blank()) +
      scale_y_continuous(breaks = seq(0,200,100),
                         limits = c(0,my_ymax),
                         position = "right")

}

p_pb <- groupcount_plot("Proud Boys", 220)
p_ok <- groupcount_plot("Oath Keepers", 220)
p_iii <- groupcount_plot("Three Percenters", 220)
p_bb <- groupcount_plot("Boogaloo Bois", 220)
p_pf <- groupcount_plot("Patriot Front", 220)
p_wlm <- groupcount_plot("White Lives Matter", 220)
p_gdl <- groupcount_plot("Goyim Defense League", 220)
p_nsc <- groupcount_plot("NSC-131", 220)
p_gag <- groupcount_plot("Gays Against Groomers", 220)
p_ptk <- groupcount_plot("Protect Texas Kids", 220)

png(paste0(chart_stump, "farright-group-counts.png"), res = 300, width = 5, height = 8, units = "in")
p_groups <- p_pb / p_ok / p_iii / p_bb / p_pf / p_wlm / p_gdl / p_nsc / p_gag / p_ptk
p_groups + plot_annotation(
  title = "Annual counts of U.S. protest events involving\nselected far-right groups",
  caption = "Source: Crowd Counting Consortium",
  tag_levels = list(c("Proud Boys", "Oath Keepers", "Three Percenters", "Boogaloo Bois",
                      "Patriot Front", "White Lives Matter", "Goyim Defense League", "NSC-131",
                      "Gays Against Groomers", "Protect Texas Kids"))
) & theme(
  plot.tag.position = "left",
  plot.tag = element_text(size = 10, hjust = 0)
)
dev.off()


## COUNTER-PROTESTS ##

# how many right-wing actions in 2022 were counterprotests?
sum(grepl("counter.protest", df_current$type, perl = TRUE))
sum(grepl("counter.protest", df_current$type, perl = TRUE))/nrow(df_current)

# how many were counter-protested?
sum(!is.na(df_current$macroevent) & !grepl("counter.protest", df_current$type, perl = TRUE))
sum(!is.na(df_current$macroevent) & !grepl("counter.protest", df_current$type, perl = TRUE))/nrow(df_current)

# how many events in total were part of a p/cp set?
nrow(filter(df_current, !is.na(macroevent)))
nrow(filter(df_current, !is.na(macroevent)))/nrow(df_current)

# how do those stats compare to 2021?
sum(grepl("counter.protest", df_prev$type, perl = TRUE))
sum(grepl("counter.protest", df_prev$type, perl = TRUE))/nrow(df_prev)
sum(!is.na(df_prev$macroevent) & !grepl("counter.protest", df_prev$type, perl = TRUE))
sum(!is.na(df_prev$macroevent) & !grepl("counter.protest", df_prev$type, perl = TRUE))/nrow(df_prev)
nrow(filter(df_prev, !is.na(macroevent)))
nrow(filter(df_prev, !is.na(macroevent)))/nrow(df_prev)

# now for some stats about interactions
XX <- ccc[lubridate::year(ccc$date) == my_year & !is.na(ccc$macroevent),]
 
XXX <- split(XX, XX$macroevent)

Z <- map_dfr(XXX, function(dat) {

  # get subsets of protests and counter-protests, respectively
  dat_p <- dat[!grepl("counter.protest", dat$type, perl = T),]
  dat_cp <- dat[grepl("counter.protest", dat$type, perl = T),]

  # generate a one-row data frame containing various quantities of interest
  data.frame(

    id = dat$macroevent[1],

    date = min(dat$date),

    p_valence = max(dat_p$valence, na.rm = T),
    cp_valence = max(dat_cp$valence, na.rm = T),

    p_size = sum(dat_p$size_mean, na.rm = T),
    cp_size = sum(dat_cp$size_mean, na.rm = T),

    p_armed = any(grepl(regex_armed, dat_p$participant_measures, perl = T, ignore.case = T)),
    cp_armed = any(grepl(regex_armed, dat_cp$participant_measures, perl = T, ignore.case = T)),

    p_scuffle = any(grepl(regex_scuffle, dat_p$participant_measures, perl = T, ignore.case = T)),
    cp_scuffle = any(grepl(regex_scuffle, dat_cp$participant_measures, perl = T, ignore.case = T)),

    p_assault = any(grepl(regex_assault, dat_p$participant_measures, perl = T, ignore.case = T)),
    cp_assault = any(grepl(regex_assault, dat_cp$participant_measures, perl = T, ignore.case = T)),

    p_injuries = any(dat_p$injuries_crowd_any == 1),
    cp_injuries = any(dat_cp$injuries_crowd_any == 1),

    p_arrests = any(dat_p$arrests_any == 1),
    cp_arrests = any(dat_cp$arrests_any == 1),

    p_deaths = any(dat_p$participant_deaths > 0),
    cp_deaths = any(dat_cp$participant_deaths > 0),

    stringsAsFactors = FALSE

  )  

})

# now compute various quantities of interest

# count of sets
nrow(Z)

# armed
sum(with(Z, table(p_armed, cp_armed))[2:4]) # any armed (count)
sum(with(Z, table(p_armed, cp_armed))[2:4])/nrow(Z) # any armed (%)
with(Z, table(p_armed, cp_armed))[4] # both armed

# freq of injuries
ZZ <- Z %>% mutate(casualties = as.integer(p_injuries) + as.integer(cp_injuries))
table(ZZ$casualties)
sum(table(ZZ$casualties)[2:3])/sum(table(ZZ$casualties))

# freq of injuries at armed events
ZZ <- Z %>% filter(p_armed | cp_armed) %>% mutate(casualties = as.integer(p_injuries) + as.integer(cp_injuries))
table(ZZ$casualties)
sum(table(ZZ$casualties)[2:3])/sum(table(ZZ$casualties))

# freq of injuries at unarmed events
ZZ <- Z %>% filter(!p_armed & !cp_armed) %>% mutate(casualties = as.integer(p_injuries) + as.integer(cp_injuries))
table(ZZ$casualties)
sum(table(ZZ$casualties))
sum(table(ZZ$casualties)[2:3])/sum(table(ZZ$casualties))

# scuffling or fighting
nrow(filter(Z, p_scuffle | cp_scuffle | p_assault | cp_assault)) # any scuffles or fighting (count)
nrow(filter(Z, p_scuffle | cp_scuffle | p_assault | cp_assault))/nrow(Z) # any scuffles or fighting (%)

# any deaths
sum(Z$p_deaths == TRUE, na.rm = T)
sum(Z$cp_deaths == TRUE, na.rm = T)

# comparisons to 2021...

XX_prev <- ccc[lubridate::year(ccc$date) == my_year - 1 & !is.na(ccc$macroevent),]
 
XXX_prev <- split(XX_prev, XX_prev$macroevent)

Z_prev <- map_dfr(XXX_prev, function(dat) {

  # get subsets of protests and counter-protests, respectively
  dat_p <- dat[!grepl("counter.protest", dat$type, perl = T),]
  dat_cp <- dat[grepl("counter.protest", dat$type, perl = T),]

  # generate a one-row data frame containing various quantities of interest
  data.frame(

    id = dat$macroevent[1],

    date = min(dat$date),

    p_valence = max(dat_p$valence, na.rm = T),
    cp_valence = max(dat_cp$valence, na.rm = T),

    p_size = sum(dat_p$size_mean, na.rm = T),
    cp_size = sum(dat_cp$size_mean, na.rm = T),

    p_armed = any(grepl(regex_armed, dat_p$participant_measures, perl = T, ignore.case = T)),
    cp_armed = any(grepl(regex_armed, dat_cp$participant_measures, perl = T, ignore.case = T)),

    p_scuffle = any(grepl(regex_scuffle, dat_p$participant_measures, perl = T, ignore.case = T)),
    cp_scuffle = any(grepl(regex_scuffle, dat_cp$participant_measures, perl = T, ignore.case = T)),

    p_assault = any(grepl(regex_assault, dat_p$participant_measures, perl = T, ignore.case = T)),
    cp_assault = any(grepl(regex_assault, dat_cp$participant_measures, perl = T, ignore.case = T)),

    p_injuries = any(dat_p$injuries_crowd_any == 1),
    cp_injuries = any(dat_cp$injuries_crowd_any == 1),

    p_deaths = as.integer(any(dat_p$participant_deaths > 0)),
    cp_deaths = as.integer(any(dat_cp$participant_deaths > 0)),

    p_arrests = any(dat_p$arrests_any == 1),
    cp_arrests = any(dat_cp$arrests_any == 1),

    stringsAsFactors = FALSE

  )  

})

# now compute various quantities of interest

# count of sets
nrow(Z_prev)

# armed
sum(with(Z_prev, table(p_armed, cp_armed))[2:4]) # any armed (count)
sum(with(Z_prev, table(p_armed, cp_armed))[2:4])/nrow(Z_prev) # any armed (%)
with(Z_prev, table(p_armed, cp_armed))[4] # both armed

# freq of injuries
ZZ_prev <- Z_prev %>% mutate(casualties = as.integer(p_injuries) + as.integer(cp_injuries))
table(ZZ_prev$casualties)
sum(table(ZZ_prev$casualties)[2:3])/sum(table(ZZ_prev$casualties))

# freq of injuries at armed events
ZZ_prev <- Z_prev %>% filter(p_armed | cp_armed) %>% mutate(casualties = as.integer(p_injuries) + as.integer(cp_injuries))
table(ZZ_prev$casualties)
sum(table(ZZ_prev$casualties)[2:3])/sum(table(ZZ_prev$casualties))

# freq of injuries at unarmed events
ZZ <- Z_prev %>% filter(!p_armed & !cp_armed) %>% mutate(casualties = as.integer(p_injuries) + as.integer(cp_injuries))
table(ZZ_prev$casualties)
sum(table(ZZ_prev$casualties)[2:3])/sum(table(ZZ_prev$casualties))

# scuffling or fighting
nrow(filter(Z_prev, p_scuffle | cp_scuffle | p_assault | cp_assault)) # any scuffles or fighting (count)
nrow(filter(Z_prev, p_scuffle | cp_scuffle | p_assault | cp_assault))/nrow(Z_prev) # any scuffles or fighting (%)

# any deaths
sum(Z_prev$p_deaths == TRUE, na.rm = T)
sum(Z_prev$cp_deaths == TRUE, na.rm = T)

