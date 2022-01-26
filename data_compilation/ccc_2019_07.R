source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates July 2019.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(CityTown = City, ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-starts_with("...")) %>%
  mutate(Date = datescrub(Date),
         Final = 0)

dat <- read_excel("data_raw/Crowd Estimates July 2019.xlsx", sheet = "LightsForLiberty")

dat_2 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-starts_with("...")) %>%
  mutate(Date = datescrub(Date),
         Country = "US",  # not sure why, but links were in this col on this tab
         Claim = "end human detention camps, protest inhumane conditions faced by refugees",  # this col was left blank on this tab
         MacroEvent = "20190712-lightsforliberty",
         Final = 0)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2019_07.csv", row.names = FALSE)
