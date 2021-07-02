source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/March 2017.xlsx", sheet = 'Tally')

dat <- dat %>%
  select(-...25, -...26) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

write.csv(dat, "data_clean/ccc_2017_03.csv", row.names = FALSE)
