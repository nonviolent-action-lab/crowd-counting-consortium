source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates July 2018.xlsx", sheet = "Tally")

dat <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  select(-...26) %>%
  slice(1:743) %>%
  mutate(Date = datescrub(Date),
         Final = 1)
              
write.csv(dat, "data_clean/ccc_2018_07.csv", row.names = FALSE)
