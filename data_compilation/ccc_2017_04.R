source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("~/nval/ccc/data_raw/April 2017.xlsx", sheet = "Tally")

dat <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = NA,
         Final = 1)

dat$MacroEvent[grepl("March for Science", dat$Claim)] <- "20170422-marchforscience"
dat$MacroEvent[grepl("People's Climate March", dat$Claim)] <- "20170429-peoplesclimatemarch"

dat$Claim[grepl("March for Science", dat$Claim)] <- "against political suppression of science"
dat$Claim[grepl("People's Climate March", dat$Claim)] <- "for action on climate change, for environmental protection, for clean energy, climate jobs justice"

write.csv(dat, "data_clean/ccc_2017_04.csv", row.names = FALSE)
