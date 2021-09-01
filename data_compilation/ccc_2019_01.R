source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates January 2019.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-starts_with("...")) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates January 2019.xlsx", sheet = "WomensMarch")

dat_2 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-`1.12 Go`, -`1.12 Int`, -`time (EST)`, -`1.19 Go`, -`1.19 Int`, -`time (EST`) %>%
  select(-starts_with("...")) %>%
  slice(1:339) %>%  # bottom of sheet has some summary rows and notes; get rid of them
  mutate(Date = datescrub(Date),
         MacroEvent = "20190119-womensmarch",
         Final = 1)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2019_01.csv", row.names = FALSE)

