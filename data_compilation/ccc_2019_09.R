source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates September 2019.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-starts_with("...")) %>%
  mutate(Date = datescrub(Date),
         Final = 0)

dat <- read_excel("data_raw/Crowd Estimates September 2019.xlsx", sheet = "Climate920US")

dat_2 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-starts_with("...")) %>%
  mutate(Date = datescrub(Date),
         Claim = "for urgent action on climate change, for climate justice and equity",
         ClaimType = 1,
         MacroEvent = "20190920-climatestrike",
         Final = 1)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2019_09.csv", row.names = FALSE)
