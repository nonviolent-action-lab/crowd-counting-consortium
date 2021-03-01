source("~/nval/ccc/r/ccc_scrubber.r")

# events on Climate March tab are already included on Tally tab

dat <- read_excel("data_raw/April 2017.xlsx", sheet = "Tally")

dat <- dat %>%
  select(-...25) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = ifelse(Claim == "People's Climate March", "People's Climate March (April 29, 2017)", NA),
         Final = 1)

write.csv(dat, "data_clean/ccc_2017_04.csv", row.names = FALSE)
