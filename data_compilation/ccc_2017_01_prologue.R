source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates, January 1-20, 2017.xlsx", sheet = "Tally")

dat <- dat %>%
  select(-AdjustedLow, -AdjustedHigh, -BestGuess) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`,
         Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

write.csv(dat, "data_clean/ccc_2017_01_prologue.csv", row.names = FALSE)
