library(tidyverse)
library(scales)
library(wesanderson)
library(tidytext)
library(wordcloud)
library(zoo)
library(patchwork)

options(stringsAsFactors = FALSE)

setwd('nval/ad-hocs')

ccc_path <- "c:/users/ulfel/documents/nval/ccc/data_clean/ccc_compiled.csv"

ccc <- read.csv(ccc_path) %>%
  mutate(date = lubridate::date(date),
         fips_code = ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code),
         issues = ifelse(issues == "", "other", issues)) %>%
  filter(!is.na(date))

df_pal <- ccc %>%
  filter(date >= "2023-10-07" & date <= Sys.Date() - 1) %>%
  filter(grepl("for palestinian liberation|free palestine(?! from hamas)|in solidarity with palestin|in remembrance of palestin", claims, ignore.case = TRUE)) %>%
  filter(!grepl("counter-protest", type)) %>%
  mutate(genocide = ifelse(grepl("genocid|holocaust|never again", claims, ignore.case = TRUE), "yes", "no"),
         apartheid = ifelse(grepl("apartheid", claims, ignore.case = TRUE), "yes", "no"),
         riversea = ifelse(grepl("from the river to the sea", claims, ignore.case = TRUE), "yes", "no"),
         ceasefire = ifelse(grepl("cease.?fire", claims, ignore.case = TRUE), "yes", "no"),
         biden = ifelse(grepl("\\bbiden\\b|genocide joe", claims, ignore.case = TRUE, perl = TRUE), "yes", "no"),
         electeds = ifelse(grepl("elected official|lawmaker|legislator", participants), "yes", "no"),
         diract = ifelse(grepl("direct action", type), "yes", "no"))

nrow(df_pal)
with(df_pal, length(unique(paste(locality, state, sep = ", "))))
with(df_pal, length(unique(state[state %in% state.abb])))
sum(!is.na(df_pal$size_mean))
round(sum(!is.na(df_pal$size_mean)) / nrow(df_pal), 2)
sum(df_pal$size_mean, na.rm = TRUE)
sum(df_pal$size_low, na.rm = TRUE)
sum(df_pal$size_high, na.rm = TRUE)
median(df_pal$size_mean, na.rm = TRUE)
max(df_pal$size_mean, na.rm = TRUE)
df_pal[which(df_pal$size_mean == max(df_pal$size_mean, na.rm = TRUE)),c("date", "locality", "state", "location_detail")]
sum(df_pal$size_mean >= 1000, na.rm = TRUE)
nrow(df_pal[df_pal$date == "2023-11-04",])
sum(df_pal[df_pal$date == "2023-11-04",]$size_mean, na.rm = TRUE)
sum(!is.na(df_pal[df_pal$date == "2023-11-04",]$size_mean))

sum(!is.na(df_pal$macroevent))
round(sum(!is.na(df_pal$macroevent)) / nrow(df_pal), 3)
pal_ids <- df_pal$macroevent[!is.na(df_pal$macroevent)]
df_pal_counters <- filter(ccc, macroevent %in% pal_ids)

sum(as.numeric(df_pal$arrests), na.rm = TRUE)
filter(df_pal_counters, grepl("counter-protest", type)) %>% summarize(tot = sum(as.numeric(arrests), na.rm = TRUE))


# flags
flags <- unlist(str_extract_all(df_pal$claims, "(?:^|, ?)[[:graph:]]{3,} flag"))
flags <- str_replace_all(flags, ", ?|\\[|\\]", "")
table(flags)


## TREND CHARTS BY WEEK ##

df_pal$week <- with(df_pal, case_when(

  date %in% seq(date("2023-10-07"), date("2023-10-08"), by = "1 day") ~ "week 1",
  date %in% seq(date("2023-10-09"), date("2023-10-15"), by = "1 day") ~ "week 2",
  date %in% seq(date("2023-10-16"), date("2023-10-22"), by = "1 day") ~ "week 3",
  date %in% seq(date("2023-10-23"), date("2023-10-29"), by = "1 day") ~ "week 4",
  date %in% seq(date("2023-10-30"), date("2023-11-05"), by = "1 day") ~ "week 5",
  TRUE ~ "week 6"

))

pal_claims <- str_split(df_pal$claims, ",")
pal_claims <- map(pal_claims, ~str_trim(.x))
df_pal$any_verbatim <- sapply(pal_claims, function(x) {

  any(!grepl("^(?:for|against) |^in [[:alpha:]]{3,} (?:of|with)", x, ignore.case = TRUE))

})

df_pal_weekly <- df_pal %>%
  group_by(week) %>%
  summarize(

    n = n(),
    n_size = sum(!is.na(size_mean)),
    size_mean = sum(size_mean, na.rm = TRUE),
    any_verbatim = sum(any_verbatim),
    apartheid = sum(apartheid == "yes") / n,
    genocide = sum(genocide == "yes") / n,
    ceasefire = sum(ceasefire == "yes") / n,
    biden = sum(biden == "yes") / n,
    electeds = sum(electeds == "yes") / n,
    diract = sum(diract == "yes") / n,
    arrests = sum(as.numeric(arrests), na.rm = TRUE)

  )

p_n <- ggplot(df_pal_weekly, aes(x = week, y = n)) +
  geom_col() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())

p_size <- ggplot(df_pal_weekly, aes(x = week, y = size_mean)) +
  geom_col() +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())

p_apartheid <- ggplot(df_pal_weekly, aes(x = week, y = apartheid)) +
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

p_biden <- ggplot(df_pal_weekly, aes(x = week, y = biden)) +
  geom_col() +
  theme_minimal() +
  ylim(0,1) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())

p_diract <- ggplot(df_pal_weekly, aes(x = week, y = diract)) +
  geom_col() +
  theme_minimal() +
  ylim(0,1) +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())

p_arrests <- ggplot(df_pal_weekly, aes(x = week, y = arrests)) +
  geom_col() +
  theme_minimal() +
  theme(axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        title = element_blank())


png(sprintf("palestine-weekly-trends-%s.png", gsub("-", "", Sys.Date() - 1)),
  res = 300, width = 7, height = 24, units = "in")
p_n / p_size / p_apartheid / p_genocide / p_ceasefire / p_biden / p_diract / p_arrests +
  plot_annotation(title = "Weekly trends in U.S. pro-Palestine protests",
                  subtitle = sprintf("October 7\u2013%s", lubridate::stamp_date("January 1, 2017", quiet = TRUE)(Sys.Date() - 1)),
                  caption = sprintf("Source: Crowd Counting Consortium (as compiled on %s)", Sys.Date()),
                  tag_levels = list(c("total\nevents",
                                      "total\nparticipants\n(conservative\nestimate)",
                                      "references to\napartheid\n(% of events)",
                                      "references to\ngenocide\n(% of events)",
                                      "references to\nceasefire\n(% of events)",
                                      "references to\nbiden\n(% of events)",
                                      "direct actions\n(% of events)",
                                      "total\narrests"
                                      ))
                  ) & theme(
                  plot.tag = element_text(size = 10),
                  plot.tag.position = "right"
                  )
dev.off()
