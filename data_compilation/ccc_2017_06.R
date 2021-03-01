source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates June 2017.xlsx", sheet = "Tally")

dat <- dat %>%
  select(-...26) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

write.csv(dat, "data_clean/ccc_2017_06.csv", row.names = FALSE)
