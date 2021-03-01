source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates December 2019.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 0)

dat <- read_excel("data_raw/Crowd Estimates December 2019.xlsx", sheet = "ClimateStrike1206")

dat_2 <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "Climate Strike (December 2019)",
         Final = 0)

dat <- read_excel("data_raw/Crowd Estimates December 2019.xlsx", sheet = "Impeach1217")

dat_3 <- dat %>%
  select(-starts_with("...")) %>%  # there is some content in some of these, incl urls
  slice(1:599) %>%  # remove summary rows
  rename(ClaimType = `Pro(2)/Anti(1)`) %>%
  # date field is extra wack here, so gotta go manual
  mutate(Date = as.character(as.Date(Date, origin = "1899-12-30")),
         MacroEvent = "Impeach Trump (December 2019)",
         Final = 0)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2, dat_3), fill = TRUE)
              
write.csv(dat, "data_clean/ccc_2019_12.csv", row.names = FALSE)
