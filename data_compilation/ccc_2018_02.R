source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates Feb 2018.xlsx", sheet = "Tally")

dat <- dat %>%
  rename(CityTown = `City/Town`, ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 1)
              
write.csv(dat, "data_clean/ccc_2018_02.csv", row.names = FALSE)
