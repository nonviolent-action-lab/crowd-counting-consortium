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
  select(-`Women's?`) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "",
         Final = 1)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2018_01.csv", row.names = FALSE)
