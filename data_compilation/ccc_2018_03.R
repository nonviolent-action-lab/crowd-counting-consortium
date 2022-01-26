source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates March 2018.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-starts_with("...")) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates March 2018.xlsx", sheet = "WalkoutMarch14")

dat_2 <- dat %>%
  rename(ClaimType = Pro2Anti1) %>%
  select(-starts_with("...")) %>%
  mutate(MacroEvent = "20180314-nationalschoolwalkout",
         Date = as.character(Date),
         Final = 1)

dat <- read_excel("data_raw/Crowd Estimates March 2018.xlsx", sheet = "MarchLives24")

dat_3 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-starts_with("...")) %>% 
  mutate(Date = datescrub(Date),
         MacroEvent = "20180324-marchforourlives",
         Final = 1)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2, dat_3), fill = TRUE)

dat <- arrange(dat, Date, StateTerritory, CityTown)
              
write.csv(dat, "data_clean/ccc_2018_03.csv", row.names = FALSE)
