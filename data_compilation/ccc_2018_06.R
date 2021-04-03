source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates June 2018.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-...26) %>%
  slice(1:998) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates June 2018.xlsx", sheet = "FamiliesBelong")

dat_2 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  slice(1:738) %>% 
  mutate(Date = datescrub(Date),
         MacroEvent = "20180630-familiesbelongtogether",
         Final = 1)

# 'Pride, June 24' tab not read b/c it is duplicated on Tally, according to note in sheet

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2018_06.csv", row.names = FALSE)

