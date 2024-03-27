# function to preprocess CCC data for months from January 2021 forward
# expected input is a file path a la "data_raw/Crowd Estimates February 2021.xlsx"
# point is to use purrr::walk or similar do call to iterate calls to this function
# over a vector of file paths, so something like...
# dateseq <- seq(from = as.Date("2021-01-01"), to = Sys.Date(), by = "month")
# file_seq <- purrr::map_chr(date_seq, function(x) {
#   yr <- substr(x, 1, 4)
#   mo <- substr(x, 6, 7)
#   sprintf("data_raw/Crowd Estimates %s %s.xlsx", month.name[as.integer(mo)], yr)
# })
# purrr::walk(dateseq, ccc_prepro)

ccc_prepro <- function(x, write_path = "data_clean/ccc_super_repeaters.csv") {

  require(tidyverse)
  require(readxl)

  options(stringsAsFactors = FALSE)

  my_col_types <- c("date", rep("text", 5), rep("numeric", 2), rep("text", 3), "numeric", rep("text", 42))

  dat <- read_excel(x, sheet = "Tally", col_types = my_col_types)

  dat <- dat %>%
    rename_at(vars(starts_with("source")), ~str_to_title(.)) %>% 
    mutate(Country = "US",
           date = as.character(date),
           Misc = paste(title, notes),
           Misc = str_trim(str_replace_all(Misc, "NA", "")),
           Actor = case_when(
             (organizations == "na" | organizations == "" | is.na(organizations)) & is.na(participants) ~ "",
             (organizations == "na" | organizations == "" | is.na(organizations)) & !is.na(participants) ~ participants,
             is.na(participants) ~ organizations,
             TRUE ~ paste(organizations, participants, sep = "; ")
           ),
           TearGas = as.numeric(grepl("tear gas|chemical|pepper|irritant", police_measures))) %>%
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
           MacroEvent = macroevent,
           Misc,
           starts_with("Source"),
           title,
           organizations,
           participants,
           participant_measures,
           police_measures,
           participant_deaths,
           police_deaths)

  # establish name for output file
  if (str_detect(x, paste(month.name, collapse = "|")) & str_detect(x, "[0-9]{4}")) {

      mo <- str_pad(which(month.name == str_extract(x, paste(month.name, collapse = "|"))), 2, pad = "0")

      yr <- str_extract(x, "[0-9]{4}")

      write_path <- sprintf("data_clean/ccc_%s_%s.csv", yr, mo)

  }

  write.csv(dat, write_path, row.names = FALSE)

}