source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates October 2019.xlsx", sheet = "Tally")

dat <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 0) %>%
  arrange(Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2019_10.csv", row.names = FALSE)
