source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("~/nval/ccc/data_raw/April 2017.xlsx", sheet = "Tally")

dat <- dat %>%
  select(-...25) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = NA,
         Final = 1)

dat$MacroEvent[dat$Claim == "March for Science"] <- "20170422-marchforscience"
dat$MacroEvent[dat$Claim == "People's Climate March"] <- "20170429-peoplesclimatemarch"

dat$Claim[dat$Claim == "March for Science"] <- "against political suppression of science"
dat$Claim[dat$Claim == "People's Climate March"] <- "for action on climate change, for environmental protection, for clean energy, climate jobs justice"

write.csv(dat, "data_clean/ccc_2017_04.csv", row.names = FALSE)
