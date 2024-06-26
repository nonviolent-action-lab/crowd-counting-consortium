source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates May 2018.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates May 2018.xlsx", sheet = "WalkoutMay2")

dat_2 <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "",
         Final = 1)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2018_05.csv", row.names = FALSE)
