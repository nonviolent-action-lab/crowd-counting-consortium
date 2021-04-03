source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates September 2018.xlsx", sheet = "Tally")

dat <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = NA,
         Final = 1)

# replace claim field for Rise for Climate events with string that more fully captures
# their demands and trips the right regexes in our dictionary; on demands, see
# https://ca.riseforclimate.org/?_ga=2.154501489.874599614.1614286287-129489914.1614286287
dat$MacroEvent[grepl("Rise for Climate", dat$Actor)] <- "20180908-riseforclimate"
dat$Claim[grepl("Rise for Climate", dat$Actor)] <- "for action against climate change; environmental, racial, and economic justice for all; no new fossil fuel development and a managed decline of existing fossil fuel production; a just transition to 100% renewable energy that protects workers, Indigenous peoples and frontline communities, both in these extractive industries and more broadly, and ensures family-sustaining jobs with the right to unionize, that are safe for people and the planet; just and equitable resiliency and recovery efforts led by the communities most impacted"
              
write.csv(dat, "data_clean/ccc_2018_09.csv", row.names = FALSE)
