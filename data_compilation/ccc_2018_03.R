source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates March 2018.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-...26) %>%
  mutate(Date = datescrub(Date),
         Final = 1) %>%
  slice(1:821)

dat <- read_excel("data_raw/Crowd Estimates March 2018.xlsx", sheet = "WalkoutMarch14")

dat_2 <- dat %>%
  rename(ClaimType = Pro2Anti1) %>%
  select(-...26) %>%
  mutate(MacroEvent = "20180314-nationalschoolwalkout",
         Date = as.character(Date),
         Final = 1) %>%
  mutate_at(c("EstimateLow", "EstimateHigh"), as.numeric) %>%
  mutate_at(vars(starts_with("Reported")), as.numeric)

dat <- read_excel("data_raw/Crowd Estimates March 2018.xlsx", sheet = "MarchLives24")

dat_3 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "20180324-marchforourlives",
         Final = 1) %>%
  # get rid of summary rows at bottom of table
  slice(1:765)

dat <- bind_rows(list(dat_1, dat_2, dat_3))

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2018_03.csv", row.names = FALSE)
