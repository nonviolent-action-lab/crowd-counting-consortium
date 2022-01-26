source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates April 2018.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(CityTown = `City/Town`, ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-...26, -...27) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates April 2018.xlsx", sheet = "MarchforScience")

dat_2 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "20180414-marchforscience",
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates April 2018.xlsx", sheet = "WalkoutApril20")

dat_3 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "20180420-nationalschoolwalkout",
         Final = 1)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2, dat_3), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2018_04.csv", row.names = FALSE)
