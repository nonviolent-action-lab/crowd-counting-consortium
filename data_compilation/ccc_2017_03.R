source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/March 2017 CCC Data.xlsx")

dat <- dat %>%
  select(-...26, -...27, -...28) %>%
  rename(ClaimType = `Pro(2)/Anti(1)/Neither(0)`) %>%
  mutate(Date = datescrub(Date),
         Final = 1) %>%
  slice(1:590)

write.csv(dat, "data_clean/ccc_2017_03.csv", row.names = FALSE)
