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

chart_stump <- sprintf("%s/ccc-annual-%s-left-", my_path, my_year)

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

df_current <- filter(ccc, valence == 1 & lubridate::year(date) == my_year)

df_prev <- filter(ccc, valence == 1 & lubridate::year(date) == my_year - 1)

df_p2 <- filter(ccc, valence == 1 & lubridate::year(date) == my_year - 2)


## COUNTS ##

# how many events?
nrow(df_current)
nrow(df_current)/nrow(df_prev)
nrow(df_prev)
nrow(df_p2)

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

# which was the largest?
df_current[which.max(df_current$size_mean),][,c("date", "locality", "state", "location_detail", "size_text", "size_mean", "size_low", "size_high", "claims")]

# how many with arrests?
sum(df_current$arrests_any == 1)
sum(df_current$arrests_any == 1)/nrow(df_current)
sum(df_prev$arrests_any == 1)
sum(df_prev$arrests_any == 1)/nrow(df_prev)
sum(df_p2$arrests_any == 1)
sum(df_p2$arrests_any == 1)/nrow(df_p2)

# how many with property damage?
sum(df_current$property_damage_any == 1)
sum(df_current$property_damage_any == 1)/nrow(df_current)
sum(df_prev$property_damage_any == 1)
sum(df_prev$property_damage_any == 1)/nrow(df_prev)
sum(df_p2$property_damage_any == 1)
sum(df_p2$property_damage_any == 1)/nrow(df_p2)

# injuries?
sum(df_current$injuries_crowd_any)
sum(df_current$injuries_crowd_any)/nrow(df_current)
sum(df_prev$injuries_crowd_any)
sum(df_prev$injuries_crowd_any)/nrow(df_prev)
sum(df_p2$injuries_crowd_any)
sum(df_p2$injuries_crowd_any)/nrow(df_p2)

# deaths?
view(filter(df_current, !is.na(participant_deaths)))

# armed?
nrow(filter(df_current, grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE)))
nrow(filter(df_current, grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE)))/nrow(df_current)
nrow(filter(df_prev, grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE)))
nrow(filter(df_prev, grepl("(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm", participant_measures, perl = TRUE, ignore.case = TRUE)))/nrow(df_prev)


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


## THEMES ##

# issue tag bar chart

# get vector of issue tags
issues <- sort(unique(str_trim(unlist(strsplit(ccc$issues, ";")))))

# generate and plot summary data
df_current_issue_counts <- map_dfr(issues, ~data.frame(issue = ., n = sum(grepl(., df_current$issues))) )

png(paste0(chart_stump, "issue-counts.png"), res = 300, width = 5, height = 7, units = "in")
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
    labs(title = "Counts of left-wing protests in 2022\nby themes in protester claims",
         caption = "Source: Crowd Counting Consortium")
dev.off()

arrange(df_current_issue_counts, -n)

# abortion specifics

rv <- filter(df_current, grepl("reproductive rights", issues) & date >= "2022-05-02" & date <= "2022-11-08")

# how many events?
nrow(rv)

# how many towns?
length(unique(with(rv, paste(resolved_locality, state, sep = ", "))))

# how does that compare to GF Uprising?
gf <- filter(ccc, grepl("racism", issues) & date >= "2020-05-26" & date <= "2020-07-31" & valence == 1)
nrow(gf)
length(unique(with(gf, paste(resolved_locality, state, sep = ", "))))

png(paste0(chart_stump, "abortion-daily-counts.png"), res = 300, width = 16/2, height = 9/2, units = "in")
df_current %>%
  filter(grepl("reproductive", issues)) %>%
  group_by(date) %>%
  tally() %>%
  ggplot(aes(date, n)) +
    geom_col() +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month",
                 date_labels = "%b") +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Daily counts of U.S. protests for abortion access in 2022",
         caption = "Source: Crowd Counting Consortium")
dev.off()


## RHETORIC

# get vector of verbatim claims
claims <- str_trim(unlist(str_split(df_current$claims, ",")))
verbatim_claims <- claims[!grepl(regex_claimsum, claims, ignore.case = TRUE)]

# n-grams; see https://bookdown.org/Maxine/tidy-text-mining/tokenizing-by-n-gram.html

# df_bigrams <- data.frame(claims = verbatim_claims) %>%
#   unnest_tokens(bigram, claims, token = "ngrams", n = 2) %>%
#   count(bigram, sort = TRUE) %>%
#   filter(!is.na(bigram)) %>%
#   filter(!grepl("\\bto\\b|\\ba\\b|\\bthe\\b", bigram, ignore.case = TRUE))

df_phrases <- str_trim(unlist(str_split(verbatim_claims, " / "))) %>%
  data.frame(claims = .) %>%
  # some clean up to make phrases comparable
  mutate(claims = tolower(claims)) %>%  # all lowercase
  mutate(claims = gsub("\\&|\\+", "and", claims)) %>%  # make all forms of 'and' identical
  mutate(claims = gsub("[^[:alnum:][:space:]'-\\#]", "", claims)) %>%  # remove most punctuation
  mutate(claims = gsub("\\[.{1,}\\]?", "", claims)) %>%  # remove coder representations of images
  mutate(claims = gsub("flags", "flag", claims)) %>%
  mutate(claims = gsub("abortion is healthcare", "abortion is health care", claims)) %>%
  mutate(claims = gsub("we will not go back", "we won't go back", claims)) %>%
  mutate(claims = gsub("women's rights = human rights", "women's rights are human rights", claims)) %>%
  count(claims, sort = TRUE) %>%
  filter(!is.na(claims) & claims != "") 

png(paste0(chart_stump, "top-20-claims.png"),
  res = 300, width = 7, height = 8, units = "in")
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
    labs(title = "20 most common phrases at left-wing protests in 2022",
         caption = "Source: Crowd Counting Consortium",
         y = "count of events at which phrase was seen")
dev.off()

arrange(df_phrases, -n)[1:20,]

# word cloud

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

png(paste0(chart_stump, "claims-wordcloud.png"), res = 300, width = 9, height = 9, units = "in")
layout(matrix(c(1, 2), nrow=2), heights=c(1, 7))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Relative frequencies of terms in claims observed\nat left-wing protests in 2022", cex = 1.5)
with(df_words, wordcloud(word, n,
                         min.freq = 20,
                         random.order = FALSE,
                         scale = c(6,1/2)))
dev.off()


## COMMUNITY DEFENSE GROUPS ##

comdef_regexes <- c(`John Brown Gun Clubs` = "john brown gun club|jbgc",
                    `Socialist Rifle Associations` = "socialist rifle association|\\bsra\\b",
                    `Pride Was a Riot, Sacramento` = "lavender angel")

comdef_counts <- map_dfr(comdef_regexes, function(x) {

   years <- c(2020:2022)

   counts <- sapply(years, function(yr) {

     df_current <- filter(ccc, lubridate::year(date) == yr & (grepl(x, organizations, perl = TRUE, ignore.case = TRUE) | grepl(x, actors, perl = TRUE, ignore.case = TRUE) | grepl(x, participants, perl = TRUE, ignore.case = TRUE)))

     nrow(df_current)

   })

  setNames(counts, years)
   
})

comdef_counts <- comdef_counts %>%
  mutate(group = names(comdef_regexes)) %>%
  pivot_longer(!group, names_to = "year", values_to = "n")

nrow(filter(df_current, grepl("john brown gun club|jbgc", organizations, perl = TRUE, ignore.case = TRUE) & grepl("counter.?protest", type, perl = TRUE)))

nrow(filter(df_current, grepl("john brown gun club|jbgc", organizations, perl = TRUE, ignore.case = TRUE) & grepl("lgbtq", issues)))

nrow(filter(df_current, grepl("queers bash back", claims, ignore.case = TRUE)))

# how many different jbgcs?
orgs <- str_trim(unlist(str_split(df_current$organizations, ";")))
table(orgs[grepl("john brown gun club|jbgc", orgs, perl = T, ignore.case = T)])

orgs_prev <- str_trim(unlist(str_split(df_prev$organizations, ";")))
table(orgs_prev[grepl("john brown gun club|jbgc", orgs_prev, perl = T, ignore.case = T)])

orgs_p2 <- str_trim(unlist(str_split(df_p2$actors, ";")))
table(orgs_p2[grepl("john brown gun club|jbgc", orgs_p2, perl = T, ignore.case = T)])
