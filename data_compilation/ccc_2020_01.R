source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates January 2020.xlsx", sheet = "Tally")

dat_1 <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         Final = 0)

dat <- read_excel("data_raw/Crowd Estimates January 2020.xlsx", sheet = "antiwarJan9")

dat_2 <- dat %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "No War With Iran (January 2020)",
         Final = 0)

dat <- read_excel("data_raw/Crowd Estimates January 2020.xlsx", sheet = "WM20")

dat_3 <- dat %>%
  select(-starts_with("...")) %>%  # there is some content in some of these, incl urls
  slice(1:268) %>%  # remove summary rows
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  mutate(Date = datescrub(Date),
         MacroEvent = "Women's March (January 2020)",
         Final = 0)

# need to use data.table here to resolve issue with inconsistent col types
dat <- data.table::rbindlist(list(dat_1, dat_2, dat_3), fill = TRUE)
              
write.csv(dat, "data_clean/ccc_2020_01.csv", row.names = FALSE)
