source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates November 2018.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates November 2018.xlsx", sheet = "MuellerNov8")

dat_2 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  slice(1:885) %>%  # bottom of sheet has some summary rows and notes; get rid of them
  mutate(Date = datescrub(Date),
         MacroEvent = "Rally to Protect Mueller Investigation (November 8, 2018)",
         Final = 1)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2018_11.csv", row.names = FALSE)
