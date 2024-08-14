library(tidyverse)

options(stringsAsFactors = FALSE)

# replace or remove this if you're not me
my_path <- "nval/ad-hocs"

# replace or remove this if you're not me with path to downloaded and merged files from repo
ccc_path <- "c:/users/ulfel/documents/nval/ccc/data_clean/ccc_compiled.csv"

ccc <- read.csv(ccc_path) %>%
  mutate(date = lubridate::date(date),
         fips_code = ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code),
         issues = ifelse(issues == "", "other", issues)) %>%
  filter(!is.na(date))

# regular expressions to spot various markers for DJT, Biden, and Harris in 'notes' field
regex_trump <- "f(ea)?t. (?:(former president )?donald (j\\. )?trump(?!,? jr)|president trump)"
regex_biden <- "f(ea)?t. (?:president|jo(?:e|seph)) biden"
regex_harris <- "f(ea)?t. (?:v(ice )?p(resident)?( kamala)?|kamala) harris"

# code I initially used to cast a wide net and tweak regexes until they worked as expected
# ccc$rallytag <- with(ccc, case_when(
# 
#   grepl(regex_trump, notes, perl = TRUE, ignore.case = TRUE) ~ "trump",
#   grepl(regex_biden, notes, perl = TRUE, ignore.case = TRUE) ~ "biden",
#   grepl(regex_harris, notes, perl = TRUE, ignore.case = TRUE) ~ "harris",
#   .default = ""
# 
# ))
# rally_check_set <- filter(ccc, grepl("\\btrump\\b|\\bbiden\\b|\\bharris\\b", notes, ignore.case = TRUE) & date >= "2021-01-01")
# write.csv(rally_check_set, "c:/users/ulfel/documents/nval/ad-hocs/rally-check-set-20240812.csv", row.names = FALSE)

trump <- ccc |>
  filter(grepl("^(campaign )?rally$", type, perl = TRUE, ignore.case = TRUE)) |>
  filter(grepl(regex_trump, notes, perl = TRUE, ignore.case = TRUE)) |>
  mutate(year = lubridate::year(date))

trump |>
  group_by(year) |>
  summarize(n_events = n(),
            n_na = sum(is.na(size_mean)),
            size_avg = mean(size_mean, na.rm = TRUE),
            size_median = median(size_mean, na.rm = TRUE),
            size_min = min(size_mean, na.rm = TRUE),
            size_max = max(size_mean, na.rm = TRUE))

biden <- ccc |>
  filter(grepl("^(campaign )?rally$", type, perl = TRUE, ignore.case = TRUE)) |>
  filter(grepl(regex_biden, notes, perl = TRUE, ignore.case = TRUE)) |>
  filter(date <= "2024-07-24") |>
  mutate(year = lubridate::year(date))

biden |>
  group_by(year) |>
  summarize(n_events = n(),
            n_na = sum(is.na(size_mean)),
            size_avg = mean(size_mean, na.rm = TRUE),
            size_median = median(size_mean, na.rm = TRUE),
            size_min = min(size_mean, na.rm = TRUE),
            size_max = max(size_mean, na.rm = TRUE))

harris <- ccc |>
  filter(grepl("^(campaign )?rally$", type, perl = TRUE, ignore.case = TRUE)) |>
  filter(grepl(regex_harris, notes, perl = TRUE, ignore.case = TRUE)) |>
  filter(date > "2024-07-24") |>
  filter(grepl("harris for president", organizations, ignore.case = TRUE))

harris |>
  summarize(n_events = n(),
            n_na = sum(is.na(size_mean)),
            size_avg = mean(size_mean, na.rm = TRUE),
            size_median = median(size_mean, na.rm = TRUE),
            size_min = min(size_mean, na.rm = TRUE),
            size_max = max(size_mean, na.rm = TRUE))
