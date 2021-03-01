source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates August 2019.xlsx", sheet = "Tally")

# looks like the events on the NAA [Never Again Action] tab are already on Tally, so not
# reading them in separately; should confirm

dat <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 0)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2019_08.csv", row.names = FALSE)
