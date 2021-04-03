library(data.table)
library(googleway)
library(tidyverse)

options(stringsAsFactors = FALSE)

setwd("~/nval/ccc")


### DATA COMPILATION ###

file_names <- list.files("data_clean")
file_names <- file_names[!grepl("compiled", file_names)]

clean_files <- map(paste0("data_clean/", file_names), read.csv)

dat_raw <- data.table::rbindlist(clean_files, fill = TRUE)


### DATA CLEANING ###

dat <- dat_raw

# get rid of events outside US and then remove country col (will get replaced in merge with geoloc data below)
valid_country_strings <- c("US", "USA", "Us", "USa", "us", "UA", "US and 26 others", "US`", "PR", "DC", "UD", "IA", "NV", "UW", "YS", "0.0", "1.0")
dat <- filter(dat, Country %in% valid_country_strings)
dat <- select(dat, -Country)

# standardize state labels based on hand checking of each oddity
# table(dat$StateTerritory)
# z <- filter(dat, StateTerritory == "CN")
dat$StateTerritory[dat$StateTerritory=="CA/NV"] <- "CA"; dat$CityTown[dat$StateTerritory=="CA/NV"] <- "South Lake Tahoe"
dat$StateTerritory[dat$StateTerritory=="Puerto Rico"] <- "PR"
dat$StateTerritory[dat$StateTerritory=="Guam"] <- "GU"
dat$StateTerritory[dat$StateTerritory=="GM"] <- "GU"
dat$StateTerritory[dat$StateTerritory=="HA"] <- "HI"
dat$StateTerritory[dat$StateTerritory=="IA/IL"] <- "IL"; dat$CityTown[dat$StateTerritory=="IA/IL"] <- "Rock Island"
dat$StateTerritory[dat$StateTerritory=="KA"] <- "KS"
dat$StateTerritory[dat$StateTerritory=="KN"] <- "KS"
dat$StateTerritory[dat$StateTerritory=="NB"] <- "NE"
dat$StateTerritory[dat$StateTerritory=="IO"] <- "IA"
dat$StateTerritory[dat$StateTerritory=="T<U+03A7>"] <- "TX"
dat$StateTerritory[dat$StateTerritory=="Texas"] <- "TX"
dat$StateTerritory[dat$StateTerritory=="VY"] <- "VT"
dat$StateTerritory[dat$StateTerritory=="CN"] <- "CT"
dat$StateTerritory <- toupper(dat$StateTerritory)

# deal with problem where recent town names include state code as well
# z <- filter(dat, grepl(", [A-Z]{2}", CityTown))
# fix(z)
dat <- mutate(dat, State = StateTerritory, Town = CityTown)
dat <- mutate(dat, Town = ifelse(grepl(", [A-Z]{2}", CityTown), str_split(CityTown, ",")[[1]][1], CityTown))
dat <- mutate(dat, State = ifelse(grepl(", [A-Z]{2}", CityTown), str_split(CityTown, ",")[[1]][2], State))
# create dummy for online/virtual events
dat <- mutate(dat, Online = ifelse(StateTerritory == "VIRTUAL" | tolower(CityTown) == "online", 1, 0))
dat <- select(dat, -CityTown, -StateTerritory)
dat <- rename(dat, CityTown = Town, StateTerritory = State)

# clean up crowd estimates, convert to numeric, and create average and ordered factor for orders of magnitude
dat$EstimateLow <- as.integer(dat$EstimateLow)
dat$EstimateHigh <- as.integer(dat$EstimateHigh)
dat$EstimateMean <- (dat$EstimateLow + dat$EstimateHigh)/2
dat$EstimateMean[is.na(dat$EstimateHigh)] <- dat$EstimateLow[is.na(dat$EstimateHigh)]
dat$EstimateMean <- round(dat$EstimateMean, 0)
dat <- mutate(dat, EstimateCat = case_when(
  is.na(EstimateMean) ~ 0,
  EstimateMean < 100 ~ 1,
  EstimateMean < 1000 ~ 2,
  EstimateMean < 10000 ~ 3,
  TRUE ~ 4 
))

# add binary markers for arrests, injuries, and property damage identifying cases with any vs. none
dat$arrests_any <- case_when(
  as.numeric(dat$ReportedArrests) >= 1 ~ 1,
  grepl("^DEAD|^o$", dat$ReportedArrests) ~ 0,
  grepl("[[:alnum:]]{2,}", dat$ReportedArrests) ~ 1,
  TRUE ~ 0)

dat$injuries_crowd_any <- case_when(
  as.numeric(dat$ReportedParticipantInjuries) >= 1 ~ 1,
  grepl("^DEAD|^o$", dat$ReportedParticipantInjuries) ~ 0,
  grepl("[[:alnum:]]{2,}", dat$ReportedParticipantInjuries) ~ 1,
  TRUE ~ 0)

dat$injuries_police_any <- case_when(
  as.numeric(dat$ReportedPoliceInjuries) >= 1 ~ 1,
  grepl("^DEAD|^o$", dat$ReportedPoliceInjuries) ~ 0,
  grepl("[[:alnum:]]{2,}", dat$ReportedPoliceInjuries) ~ 1,
  TRUE ~ 0)

dat$property_damage_any <- case_when(
  as.numeric(dat$ReportedPropertyDamage) == 0 ~ 0,
  as.numeric(dat$ReportedPropertyDamage) >= 1 ~ 1,
  grepl("^DEAD|^o$", dat$ReportedPropertyDamage) ~ 0,
  grepl("[[:alnum:]]{2,}", dat$ReportedPropertyDamage) ~ 1,
  TRUE ~ 0)

# create clean version of claim type col
dat$ClaimType <- with(dat, case_when(
  ClaimType %in% c("0", "0.0", "o") ~ 0,
  ClaimType %in% c("1", "1.0", "3") ~ 1,  # not sure where 3 came from, but it's all 2017 March for Science, so...
  ClaimType %in% c("2", "2.0") ~ 2,
  TRUE ~ NA_real_
))

# filter out non-events identified by 0 (and, apparently, other things) in the Events col
dat <- filter(dat, !(Events %in% c("0", "cancelled", "postponed", "DEAD LINK", "DEADLINK")))

# clean up the TearGas col
dat$TearGas <- with(dat, case_when(
  TearGas %in% c("0", "0.0", "o", "no") ~ 0,
  TearGas %in% c("1", "1.0", "yes") ~ 1,
  TRUE ~ NA_real_
))

# rename Location to LocationDetail to avoid conflict with new col to come
dat <- rename(dat, LocationDetail = Location)

# add Location col used as unique id for geocoding
dat$Location <- with(dat, paste(CityTown, StateTerritory, sep = ", "))


### GEOCODING ###

# get a df with only unique combos of locality, state
locations <- dat %>% 
  select(CityTown, StateTerritory, Location) %>%
  distinct(Location, .keep_all = TRUE) %>%
  # need this next bit to prevent api from choking on non-UTF-8 characters (e.g., smart quotes, n's with tildes)
  mutate(CityTown2 = iconv(CityTown, to = "ASCII//TRANSLIT"))

# load the stored lookup table
location_lookup <- read.csv("r/location_lookup.csv") %>%
  mutate(Location = paste(CityTown, StateTerritory, sep = ", "))

# reduce the locations to ones in the new data but not in the lookup table
new_locations <- anti_join(locations, location_lookup, by = "Location")

# if that reduced set has 1 or more rows, do the geocoding for it
if(nrow(new_locations) > 0) {

  # load api key for googleway
  source("r/googleway-key.R")

  # set the api key for googleway
  set_key(key = my_googleway_key)

  # hit the api
  DF <- mutate(new_locations, gw_res = map(paste(CityTown2, StateTerritory), google_geocode, simplify = TRUE))

  # extract lat/lon from those results. the 'ifelse' bit here and in the next step is 
  # required to handle cases where Google Maps couldn't resolve the location; otherwise,
  # pull() fails and throws an error
  X <- DF %>% 
    mutate(latlon = map(gw_res, purrr::pluck, "results", "geometry", "location")) %>%
    mutate(lat = map_chr(latlon, ~ifelse(is.null(.), NA, pull(., lat))),
           lon = map_chr(latlon, ~ifelse(is.null(.), NA, pull(., lng)))) %>%
    select(CityTown, StateTerritory, Location, lat, lon)

  # extract the county and other resolved elements of address
  Y <- DF %>% mutate(address_components = map(gw_res, purrr::pluck, "results", "address_components", 1))

  Y$locality <- map_chr(Y$address_components, function(x) {

    if(is.null(x)) { return(NA) }

    # need 2nd filter for rare cases where sublocality is also a thing
    y <- as.data.frame(x) %>% filter(grepl("locality", types)) %>% filter(!grepl("sublocality", types))

    if(nrow(y) == 0) { return(NA) }

    pull(y, long_name)
  
  })

  Y$county <- map_chr(Y$address_components, function(x) {

    if(is.null(x)) { return(NA) }

    y <- as.data.frame(x) %>% filter(grepl("administrative_area_level_2", types))

    if(nrow(y) == 0) { return(NA) }

    pull(y, long_name)
  
  })

  Y$state <- map_chr(Y$address_components, function(x) {

    if(is.null(x)) { return(NA) }

    y <- as.data.frame(x) %>% filter(grepl("administrative_area_level_1", types))

    if(nrow(y) == 0) { return(NA) }

    pull(y, short_name)
  
  })

  Y <- select(Y, CityTown, StateTerritory, Location, locality, state, county)

  # merge those two sets of results
  Z <- merge(X, Y)

  # append those merged results to the existing lookup table, minus the extra Location col
  location_lookup <- data.table::rbindlist(list(location_lookup, Z), use.names = TRUE)

  write.csv(location_lookup, "r/location_lookup.csv", row.names = FALSE)

}

# now join the lookup table to the data to add geo cols
dat <- left_join(dat, location_lookup)


### ISSUE TAGGING ###

source("r/ccc_issue_regex_list.R")

dat$ClaimCodes <- claimcoder(dat$Claim)

dat <- claimcoder_addendum("ClaimCodes", dat)


### FINAL ARRANGING ###

# remove cols that will be excluded from the compiled version, usually because they are specific to
# one macro-event (e.g., Women's March 2017) and therefore missing for almost all events, or b/c
# they are only useful inside the Google Sheets (Events, TownsCities)
dat <- dat %>%
  rename_at(vars(starts_with("Source")), ~str_replace(., "Source", "source_")) %>%
  select(date = Date,
         locality = CityTown,
         state = StateTerritory,
         location = Location,
         location_detail = LocationDetail,
         county = County,
         online = Online,
         type = EventType,
         macroevent = MacroEvent,
         actors = Actor,
         claims = Claim,
         valence = ClaimType,
         issues = ClaimCodes,
         size_text = EstimateText,
         size_low = EstimateLow,
         size_high = EstimateHigh,
         size_mean = EstimateMean,
         size_cat = EstimateCat,
         arrests = ReportedArrests,
         arrests_any,
         injuries_crowd = ReportedParticipantInjuries,
         injuries_crowd_any,
         injuries_police = ReportedPoliceInjuries,
         injuries_police_any,
         property_damage = ReportedPropertyDamage,
         property_damage_any,
         chemical_agents = TearGas,
         starts_with("source_"),
         misc = Misc,
         final = Final,
         lat,
         lon,
         resolved_locality = locality,
         resolved_county = county,
         resolved_state = state)
                   
dat <- dat %>%
  mutate(date = as.Date(date)) %>%
  arrange(date, state, locality)

# may solve problem with stray carriage returns and other oddities from GS
dat <- mutate_if(dat, is.character, str_trim)

### OUTPUT ###

write.csv(dat, "data_clean/ccc_compiled.csv", row.names = FALSE)

write.csv(dat, "c:/users/ulfel/documents/ccc-data-dashboard/data/ccc_compiled.csv", row.names = FALSE)

write.csv(dat, "c:/users/ulfel/documents/github/crowd-counting-consortium/ccc_compiled.csv", row.names = FALSE)
