source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates Dec 2017.xlsx", sheet = "Tally")

dat <- dat %>%
  # note additional cols needing renaming here
  rename(CityTown = `City/Town`, EventType = protest, ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 1)
           
write.csv(dat, "data_clean/ccc_2017_12.csv", row.names = FALSE)

