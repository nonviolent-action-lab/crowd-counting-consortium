source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates October 2020.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 0)

dat <- read_excel("data_raw/Crowd Estimates October 2020.xlsx", sheet = "WomensMarch")

dat_2 <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "20201017-womensmarch",
         Final = 0)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2), fill = TRUE)
              
write.csv(dat, "data_clean/ccc_2020_10.csv", row.names = FALSE)
