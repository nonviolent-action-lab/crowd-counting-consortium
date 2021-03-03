source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates Jan 2018.xlsx", sheet = "Tally (Other January Protests)")

dat_1 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-`Women's?`) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates Jan 2018.xlsx", sheet = "WomensMarch")

dat_2 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-...27, -`Women's?`) %>%
  slice(1:413) %>%  # remove trailing rows with summary cells 
  mutate(Date = datescrub(Date),
         MacroEvent = "Women's March (January 20, 2018)",
         Final = 1)

dat <- bind_rows(dat_1, dat_2)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2018_01.csv", row.names = FALSE)
