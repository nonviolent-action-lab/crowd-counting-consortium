source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates October 2018.xlsx", sheet = "Tally")

dat <- dat %>%
  select(-S1, -S2) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.)

dat <- dat %>%
  mutate(Date = ifelse(grepl("0208-", Date), "43387.0", Date)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate(Date = ifelse(Date >= "2019-10-01", Date - 365, Date)) %>%
  mutate(Date = as.Date(as.numeric(Date), origin = "1970-01-01")) %>%
  mutate(Date = as.character(Date),
         Final = 1)
              
write.csv(dat, "data_clean/ccc_2018_10.csv", row.names = FALSE)
