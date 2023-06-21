library(data.table)
library(googleway)
library(tidyverse)
library(readxl)
library(tigris)

options(stringsAsFactors = FALSE)

setwd("~/nval/ccc")

memory.limit(size = 10000)

edge_date <- Sys.Date() - 3


### DATA COMPILATION ###

# NOTE: This assumes that current versions of all relevant CCC Google Sheets have
# been downloaded as .xls files into a local directory called ~/nval/ccc/data_raw

# preprocess all raw monthly files from Jan 2021 to current month
# NOTE: this function depends on cols in all of these files having the same
# number of cols in the same order, bc it prespecifies col types
source("r/ccc_data_preprocessor.r")

date_seq <- seq(from = as.Date("2021-01-01"), to = Sys.Date(), by = "month")

file_seq <- map_chr(date_seq, function(x) {

  yr <- substr(x, 1, 4)

  mo <- substr(x, 6, 7)

  sprintf("data_raw/Crowd Estimates %s %s.xlsx", month.name[as.integer(mo)], yr)

})

purrr::walk(file_seq, ccc_prepro)

# now preprocess raw file for super-repeater events
ccc_prepro("data_raw/CCC Super Repeaters.xlsx")

# compile all pre-processed monthly files into one table
file_names <- grep("ccc_\\d{4}_\\d{2}", list.files("data_clean"), value = TRUE)
file_names <- file_names[as.numeric(substr(file_names, 5, 8)) >= 2021]  # reduce to 2021-
file_names <- append(file_names, "ccc_super_repeaters.csv")  # add super-repeaters
clean_files <- map(paste0("data_clean/", file_names), read.csv)
dat_raw <- data.table::rbindlist(clean_files, fill = TRUE)
dat_raw <- dat_raw[date(dat_raw$Date) >= "2021-01-01",]  # drop super-repeaters from before 2021


### DATA CLEANING ###

dat <- dat_raw

# remove country col
dat <- select(dat, -Country)

# create dummy for online/virtual events
dat <- mutate(dat, Online = ifelse(StateTerritory == "VIRTUAL" | tolower(CityTown) == "online", 1, 0))

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
  filter(!is.na(CityTown)) %>%
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
    mutate(lat = map_chr(latlon, ~ifelse(is.null(.), NA, as.character(pull(., lat)))),
           lon = map_chr(latlon, ~ifelse(is.null(.), NA, as.character(pull(., lng))))) %>%
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

# eyeball how many new locations were added to lookup table
print(nrow(new_locations))


### ISSUE TAGGING ###

source("r/ccc_issue_regex_list.R")

# run issue tagging functions over claims fields

dat$ClaimCodes <- claimcoder(dat$Claim)
dat <- claimcoder_addendum(dat)

# now do for coder-generated only

# generate column of only coder-generated claim summaries, no verbatim
dat$claims_major <- map_chr(dat$Claim, function(x) {

  # regex to select coder-generated claim summaries
  regex_verbatim <- "^(?:for|against) |^in [[:alpha:]]{3,} (?:of|with)"

  # split the claims field at the commas into vector of strings
  if(!is.na(x)) { 

    y <- str_split_1(x, ",") }

  else { 

    y <- "for unspecified"

  }

  # trim whitespace from ends of strings
  y <- str_trim(y)

  # use regex to filter down to coder-generated claim summaries
  z <- y[grepl(regex_verbatim, y, ignore.case = TRUE)]

  # recombine remaining elements into a single string
  z <- paste(z, collapse = ", ")

  return(z)

})

dat$ClaimCodesMajor <- claimcoder(dat$claims_major)


### ARRANGING ###

# remove cols that will be excluded from the compiled version, usually because they are specific to
# one macro-event (e.g., Women's March 2017) and therefore missing for almost all events, or b/c
# they are only useful inside the Google Sheets (Events, TownsCities)
dat <- dat %>%
  rename_at(vars(starts_with("Source")), ~str_replace(., "Source", "source_")) %>%
  select(date = Date,
         locality = CityTown,
         state = StateTerritory,
         location_detail = LocationDetail,
         online = Online,
         type = EventType,
         title = title,
         macroevent = MacroEvent,
         actors = Actor,
         organizations = organizations,
         participants = participants,
         claims = Claim,
         valence = ClaimType,
         issues = ClaimCodes,
         issues_major = ClaimCodesMajor,
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
         participant_measures,
         police_measures,
         participant_deaths,
         police_deaths,
         starts_with("source_"),
         notes = Misc,
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


### ADD FIPS CODES ###

source("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/data_compilation/fips_for_county_function.R")

dat <- fips_for_county(dat)


### OUTPUT ###

nrow(dat)

# load archived pre-2021 data
dat_2017 <- read.csv("data_clean/ccc_compiled_2017.csv") %>%
  mutate(date = lubridate::date(date),
         fips_code = ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code))

# merge pre- and post-2021 data
dat <- rbindlist(list(dat_2017, dat))

# store local version with all events from all sheets
write.csv(dat, "data_clean/ccc_compiled.csv", row.names = FALSE, fileEncoding = "UTF-8")

# now drop past few days and all future days for posted version
dat <- filter(dat, date <= edge_date)
nrow(dat)

# store version for GitGub
write.csv(dat, "c:/users/ulfel/documents/github/crowd-counting-consortium/ccc_compiled.csv", row.names = FALSE, , fileEncoding = "UTF-8")

# produce and store version with preprocessing and col dropping for CCC Data Dashboard
source("r/ccc_dashboard_data_prep.r")

