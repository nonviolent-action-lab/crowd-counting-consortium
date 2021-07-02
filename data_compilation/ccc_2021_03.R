source("~/nval/ccc/r/ccc_scrubber.r")

dat <- read_excel("data_raw/Crowd Estimates March 2021.xlsx", sheet = "Tally")

dat <- dat %>%
  filter(state != "INT") %>%
  rename_at(vars(starts_with("source")), ~str_to_title(.)) %>% 
  mutate(Country = "US",
         Misc = paste(title, notes),
         Misc = str_trim(str_replace_all(Misc, "NA", "")),
         Actor = case_when(
           (organizations == "na" | organizations == "" | is.na(organizations)) & is.na(participants) ~ "",
           (organizations == "na" | organizations == "" | is.na(organizations)) & !is.na(participants) ~ participants,
           is.na(participants) ~ organizations,
           TRUE ~ paste(organizations, participants, sep = "; ")
         ),
         TearGas = as.numeric(grepl("tear gas|chemical|pepper|irritant", police_measures)),
         Final = 0) %>%
  select(CityTown = locality,
         StateTerritory = state,
         Country,
         Location = location,
         Date = date,
         EstimateText = size_text,
         EstimateLow = size_low,
         EstimateHigh = size_high,
         Actor,
         Claim = claims,
         ClaimType = valence,
         EventType = event_type,
         ReportedArrests = arrests,
         ReportedParticipantInjuries = participant_injuries,
         ReportedPoliceInjuries = police_injuries,
         ReportedPropertyDamage = property_damage,
         TearGas,
         TownsCities = city_days,
         Events = events,
         MacroEvent = macroevent,
         Misc,
         starts_with("Source"),
         Final,
         title,
         organizations,
         participants,
         participant_measures,
         police_measures,
         participant_deaths,
         police_deaths)

write.csv(dat, "data_clean/ccc_2021_03.csv", row.names = FALSE)
