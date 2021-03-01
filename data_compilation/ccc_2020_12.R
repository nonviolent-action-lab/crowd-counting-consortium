source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates December 2020.xlsx", sheet = "Tally")

dat <- dat %>%
  rename(ClaimType = Pro2Anti1, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 0)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2020_12.csv", row.names = FALSE)
