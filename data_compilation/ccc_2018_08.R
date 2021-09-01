source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates August 2018.xlsx", sheet = "Tally")

dat <- dat %>%
  select(-starts_with("...")) %>%
  rename(ClaimType = `Pro(2)/Anti(1)`, Misc = Misc.) %>%
  slice(1:573) %>%
  # date format screwed up by row with "week of..." in it, so different scrubbing needed
  mutate(Date = as.Date(as.numeric(Date), origin = "1899-12-30")) %>%
  mutate(Date = as.character(Date),
         Final = 1)
              
write.csv(dat, "data_clean/ccc_2018_08.csv", row.names = FALSE)

