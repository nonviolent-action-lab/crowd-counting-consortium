library(tidyverse)
library(scales)
library(wesanderson)
library(tidytext)
library(wordcloud)
library(zoo)
library(patchwork)

options(stringsAsFactors = FALSE)

# comment this out if you're not me
setwd('nval/ad-hocs')

# NOTE: this is a local path and should be replaced with your own local path or 
# redirected to the relevant csv on GitHub:
# https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled_2021-present.csv
ccc_path <- "c:/users/ulfel/documents/nval/ccc/data_clean/ccc_compiled.csv"

ccc <- read.csv(ccc_path) %>%
  mutate(date = lubridate::date(date),
         fips_code = ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code),
         issues = ifelse(issues == "", "other", issues)) %>%
  filter(!is.na(date))

## PRO-PALESTINE STATS ##

df_pal <- ccc %>%
  filter(date >= "2023-10-07" & date <= "2023-11-26") %>%
  filter(grepl("for palestinian liberation|free palestine(?! from hamas)|in solidarity with palestin|in remembrance of palestin|for ceasefire in gaza", claims, ignore.case = TRUE, perl = TRUE)) %>%
  filter(!grepl("counter-protest", type)) %>%
  filter(!grepl("in solidarity with Israel", claims, ignore.case = TRUE)) %>%
  mutate(genocide = ifelse(grepl("genocid|holocaust|never again", claims, ignore.case = TRUE), "yes", "no"),
         apartheid = ifelse(grepl("apartheid", claims, ignore.case = TRUE), "yes", "no"),
         riversea = ifelse(grepl("from the river to the sea", claims, ignore.case = TRUE), "yes", "no"),
         ceasefire = ifelse(grepl("cease.?fire", claims, ignore.case = TRUE), "yes", "no"),
         taxes = ifelse(grepl("\\btax", claims, ignore.case = TRUE), "yes", "no"),
         kids = ifelse(grepl("\\bchild(ren)?|\\bkids|babies", claims, ignore.case = TRUE), "yes", "no"),
         resistance = ifelse(grepl("\\bresist", claims, ignore.case = TRUE), "yes", "no"),
         intifada = ifelse(grepl("intifada", claims, ignore.case = TRUE), "yes", "no"),
         biden = ifelse(grepl("\\bbiden|genocide joe", claims, ignore.case = TRUE, perl = TRUE), "yes", "no"),
         electeds = ifelse(grepl("elected official|lawmaker|legislator", participants), "yes", "no"),
         students = ifelse(grepl("student", participants) | grepl("student", actors, ignore.case = TRUE), "yes", "no"),
         campuses = ifelse(grepl("university|college|school|institute", location_detail, ignore.case = TRUE), "yes", "no"),
         acttype = case_when(grepl("direct action", type) & property_damage_any == 1 ~ "direct action",
                             grepl("direct action", type) ~ "civil disobedience",
                             TRUE ~ "other"))

df_pal$week <- with(df_pal, case_when(

  date %in% seq(date("2023-10-07"), date("2023-10-15"), by = "1 day") ~ "Oct. 7-15",
  date %in% seq(date("2023-10-16"), date("2023-10-22"), by = "1 day") ~ "Oct. 16-22",
  date %in% seq(date("2023-10-23"), date("2023-10-29"), by = "1 day") ~ "Oct. 23-29",
  date %in% seq(date("2023-10-30"), date("2023-11-05"), by = "1 day") ~ "Oct. 30-Nov. 5",
  date %in% seq(date("2023-11-06"), date("2023-11-12"), by = "1 day") ~ "Nov. 6-12",
  date %in% seq(date("2023-11-13"), date("2023-11-19"), by = "1 day") ~ "Nov. 13-19",
  date %in% seq(date("2023-11-20"), date("2023-11-26"), by = "1 day") ~ "Nov. 20-26",
  TRUE ~ "Nov. 27-"

))

nrow(df_pal)
with(df_pal, length(unique(paste(locality, state, sep = ", "))))
with(df_pal, length(unique(state[state %in% state.abb])))
sum(!is.na(df_pal$size_mean))
round(sum(!is.na(df_pal$size_mean)) / nrow(df_pal), 2)
sum(df_pal$size_mean, na.rm = TRUE)
sum(df_pal$size_low, na.rm = TRUE)
sum(df_pal$size_high, na.rm = TRUE)
round(mean(df_pal$size_mean, na.rm = TRUE), 0)
median(df_pal$size_mean, na.rm = TRUE)
max(df_pal$size_mean, na.rm = TRUE)

sum(!is.na(df_pal$macroevent))
round(sum(!is.na(df_pal$macroevent)) / nrow(df_pal), 3)

# pal_ids <- df_pal$macroevent[!is.na(df_pal$macroevent)]
# df_pal_counters <- filter(ccc, macroevent %in% pal_ids)

sum(as.numeric(df_pal$arrests), na.rm = TRUE)

table(df_pal$electeds)["yes"]
round(table(df_pal$electeds)["yes"] / nrow(df_pal), 3)

sum(df_pal$property_damage_any)
round(sum(df_pal$property_damage_any) / nrow(df_pal), 2)
# print(unlist(df_pal[df_pal$property_damage_any == 1,]$property_damage))


## PRO-ISRAEL STATS ##

df_isr <- ccc %>%
  filter(date >= "2023-10-07" & date <= Sys.Date() - 1) %>%
  filter(grepl("against violent attack on israel|in (?:solidarity with|support of) israel|stand with israel", claims, perl = TRUE, ignore.case = TRUE)) %>%
  filter(!grepl("counter-protest", type)) %>%
  filter(!grepl("for palestinian liberation|palestine will be free|free free Palestine", claims, perl = TRUE, ignore.case = TRUE)) %>%
  mutate(hamas = ifelse(grepl("\\bhamas|terroris", claims, ignore.case = TRUE), "yes", "no"),
         usaflag = ifelse(grepl("(?:american|usa) flag", claims, ignore.case = TRUE), "yes", "no"),
         electeds = ifelse(grepl("elected official|lawmaker|legislator", participants), "yes", "no"),
         acttype = case_when(grepl("vigil", type) ~ "vigil",
                             TRUE ~ "other"))

nrow(df_isr)
with(df_isr, length(unique(paste(locality, state, sep = ", "))))
with(df_isr, length(unique(state)))
sum(!is.na(df_isr$size_mean))
round(sum(!is.na(df_isr$size_mean)) / nrow(df_isr), 2)
sum(df_isr$size_mean, na.rm = TRUE)
sum(df_isr$size_low, na.rm = TRUE)
sum(df_isr$size_high, na.rm = TRUE)
mean(df_isr$size_mean, na.rm = TRUE)
median(df_isr$size_mean, na.rm = TRUE)

sum(!is.na(df_isr$macroevent))
round(sum(!is.na(df_isr$macroevent)) / nrow(df_isr), 3)

# isr_ids <- df_isr$macroevent[!is.na(df_isr$macroevent)]
# df_isr_counters <- filter(ccc, macroevent %in% isr_ids)

sum(as.numeric(df_isr$arrests), na.rm = TRUE)

sum(df_isr$property_damage_any)

table(df_isr$electeds)["yes"]
round(table(df_isr$electeds)["yes"] / nrow(df_isr), 3)


## CHARTS ##

png("pal-daily-counts-20231128.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
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
                  subtitle = "October 7\u2013November 26, 2023",
                  caption = "Source: Crowd Counting Consortium",
                  tag_levels = list(c("count of\nevents",
                                      "estimated\nparticipants"
                                      ))
                  ) & theme(
                  plot.tag = element_text(size = 10),
                  plot.tag.position = "right"
                  )
dev.off()

png("isr-daily-counts-20231128.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
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
                  subtitle = "October 7\u2013November 26, 2023",
                  caption = "Source: Crowd Counting Consortium",
                  tag_levels = list(c("count of\nevents",
                                      "estimated\nparticipants"
                                      ))
                  ) & theme(
                  plot.tag = element_text(size = 10),
                  plot.tag.position = "right"
                  )
dev.off()

png("pal-daily-acttype-20231128.png", res = 300, width = 16/1.67, height = 9/1.67, unit = "in")
df_pal %>%
  group_by(date, acttype) %>%
  tally() %>%
  ungroup() %>%
  mutate(type = fct_relevel(acttype, "other", "civil disobedience", "direct action")) %>%
  ggplot(aes(date, n, fill = type)) +
    geom_col() +
    theme_minimal() +
    theme(axis.title = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(title = "Daily counts of U.S. pro-Palestine events by action type",
         subtitle = "October 7\u2013November 26, 2023",
         caption = "Source: Crowd Counting Consortium") +
    scale_fill_manual(values = wes_palette("Royal1")[c(1,4,2)],
                      name = NULL)
dev.off()

png("palestine-weekly_rhetoric-trends-20231128.png", res = 300, width = 7, height = 10, unit = "in")
df_pal_weekly <- df_pal %>%
  group_by(week) %>%
  summarize(
    event_count = n(),
    apartheid = round(sum(apartheid == "yes") / event_count, 2),
    resistance = round(sum(resistance == "yes") / event_count, 2),
    genocide = round(sum(genocide == "yes") / event_count, 2),
    ceasefire = round(sum(ceasefire == "yes") / event_count, 2)
  ) %>%
  mutate(week = fct_relevel(week, "Oct. 7-15", "Oct. 16-22", "Oct. 23-29", "Oct. 30-Nov. 5", "Nov. 6-12", "Nov. 13-19"))

p_apartheid <- ggplot(df_pal_weekly, aes(x = week, y = apartheid)) +
  geom_col() +
  theme_minimal() +
  ylim(0,1) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())
p_resistance <- ggplot(df_pal_weekly, aes(x = week, y = resistance)) +
  geom_col() +
  theme_minimal() +
  ylim(0,1) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())
p_genocide <- ggplot(df_pal_weekly, aes(x = week, y = genocide)) +
  geom_col() +
  theme_minimal() +
  ylim(0,1) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())
p_ceasefire <- ggplot(df_pal_weekly, aes(x = week, y = ceasefire)) +
  geom_col() +
  theme_minimal() +
  ylim(0,1) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())
p_apartheid / p_resistance / p_genocide / p_ceasefire +
  plot_annotation(title = "Trends in rhetoric at pro-Palestine protests in the U.S.",
                  subtitle = "Share of events at which term or its cognates was seen in signs or chants",
                  caption = "Source: Crowd Counting Consortium",
                  tag_levels = list(c("apartheid",
                                      "resistance",
                                      "genocide",
                                      "ceasefire"
                                      ))
                  ) & theme(
                  plot.tag = element_text(size = 12),
                  plot.tag.position = "right"
                  )
dev.off()

png("pal-top-claims-20231128.png", res = 300, width = 10, height = 6, units = "in")

pal_claims <- str_trim(unlist(str_split(df_pal$claims, ",")))
pal_claims <- pal_claims[!grepl("^(?:for|against) |^in [[:alpha:]]{3,} (?:of|with)", pal_claims, ignore.case = TRUE)]
pal_claims <- pal_claims[!grepl("flag$", pal_claims, ignore.case = TRUE)]
pal_claims <- gsub("[[:punct:]]", "", pal_claims)
pal_claims <- gsub("[[:space:]]{2,}", " ", pal_claims)
pal_claims <- tolower(pal_claims)
pal_claims <- gsub("isreal", "israel", pal_claims)
pal_claims <- gsub("^cease.?fire( now)?", "ceasefire( now)", pal_claims)
pal_claims <- gsub("^free free", "free", pal_claims)
pal_claims <- gsub("from the river to the sea( palestine will be free)?", "from the river to the sea( palestine will be free)", pal_claims)
pal_claims <- gsub("selfdefense", "self defense", pal_claims)
pal_claims <- gsub("bombing (?:children|kids|babies) is not self defense", "bombing (children/kids/babies) is not self defense", pal_claims)
pal_claims <- gsub("^(?:end|stop) (the )?genocide( in gaza)?$", "(stop/end)( the) genocide (in gaza)", pal_claims)
pal_claims <- gsub("end (all )?us aid to israel", "end (all )us aid to israel", pal_claims)

df_pal_claims <- setNames(as.data.frame(table(pal_claims)), c("claim", "n"))
df_pal_claims <- df_pal_claims[df_pal_claims$claim != "",]
df_pal_claims <- arrange(df_pal_claims, -n)

# view(df_pal_claims)

df_pal_claims %>%
 slice(1:25) %>%
  arrange(-n) %>%
  ggplot(aes(reorder(claim, n, sum), n)) + 
    geom_col(fill = "gray60") +
    theme_minimal() +
    coord_flip() +
    theme(axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(title = "25 claims seen most often at U.S. pro-Palestine protests",
         subtitle = "October 7\u2013November 26, 2023",
         caption = "Source: Crowd Counting Consortium",
         y = "count of events at which phrase was seen")
dev.off()
