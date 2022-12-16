library(tidyverse)
library(googlesheets4)

options(stringsAsFactors = FALSE)

# need this for googlesheets4
gs4_deauth()

url <- "https://docs.google.com/spreadsheets/d/1xa0iLqYKz8x9Yc_rfhtmSOJQ2EGgeUVjvV4A8LsIaxY/htmlview?sle=true#gid=0"

dat <- read_sheet(url,
                 sheet = "By Town/City",
                 col_names = FALSE,
                 skip = 11)

names(dat) <- c("CityTown", "StateTerritory", "Country", "EstimateLow", "BestGuess",
                "EstimateHigh", "Estimate3", "AdjustedLow", "AdjustedHigh", "blank",
                "Source1", "Source2", "Source3", "TownsCities", "Expected",
                "OrganizerLow", "OrganizerHigh", "PublicLow", "PublicHigh", "ReporterLow",
                "ReporterHigh", "ParticipantLow", "ParticipantHigh", "OtherLow", "OtherHigh")

dat2 <- dat %>%
  mutate_if(is.list, function(x) unlist(lapply(x, function(y) ifelse(is.null(y), NA, unlist(y)))) ) %>%
  mutate(Date = "2017-01-21",
         EventType = "march",
         ClaimType = 1,
         Claim = "women's rights are human rights",
         MacroEvent = "",
         Final = 1) %>%
  select(-blank) %>%
  # drop international data and data for online events
  filter(Country == "US" & !is.na(StateTerritory) & StateTerritory != "--")

# isolate town names in CityTown col
dat3 <-  mutate(dat2, CityTown = sapply(CityTown, function(x) { 

  y <- strsplit(x, ",")
  z <- getElement(y[[1]], 1)

  return(z)

}))

# drop the island name and state code from towns in HI
dat3 <- mutate(dat3, CityTown = ifelse(StateTerritory == "HI", word(CityTown, 1), CityTown))

# save the results to local dir
write.csv(dat3, "c:/users/ulfel/documents/nval/ccc/data_clean/ccc_2017_01.csv", row.names = FALSE)
