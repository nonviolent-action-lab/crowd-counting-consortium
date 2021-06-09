source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates, January 20-31, 2017.xlsx", sheet = 'Tally')

dat <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 1) %>%
  select(-BestGuess, -AdjustedLow, -AdjustedHigh)

write.csv(dat, "data_clean/ccc_2017_01_addendum.csv", row.names = FALSE)
