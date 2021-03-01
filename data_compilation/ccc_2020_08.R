source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates August 2020.xlsx", sheet = "Tally")

dat <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro2-Anti1`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 0)

write.csv(dat, "data_clean/ccc_2020_08.csv", row.names = FALSE)
