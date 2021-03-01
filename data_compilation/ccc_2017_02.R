source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/February 2017 Final.xlsx")

dat <- dat %>%
  rename(ClaimType = `Pro (2)/anti(1)`) %>%
  mutate(Date = datescrub(Date),
         Final = 1)

write.csv(dat, "data_clean/ccc_2017_02.csv", row.names = FALSE)
