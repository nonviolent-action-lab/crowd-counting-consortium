# Crowd Counting Consortium Crowd Data Dictionary #
This dictionary describes the compiled and cleaned version of the Crowd Counting Consortium (CCC) Crowd Data stored in this repository. Columns that appear in some of the CCC Google Sheets but are specific to certain months or macro-events have been dropped; some columns have been added or modified to facilitate mapping and analysis; and events occurring outside the United States and U.S. territories have been omitted. If you want to work with those columns or events, or just want to work with the data in its raw form, please use the original Google Sheets,  which you can access from the CCC website ([here](https://sites.google.com/view/crowdcountingconsortium/view-download-the-data)).

- **date**. Date of event in YYYY-MM-DD format, starting with the earliest action in the locality. *Date* in the source data.

- **locality**. Name of locality in which the event took place. *CityTown* in the source data.

- **state**. Two-letter U.S. postal abbreviation for the state or territory in which the event took place. *StateTerritory* in the source data.

- **location**. Concatenation of locality and state strings used in geolocation.

- **location_detail**. If known, details on the location within the city or town where the action took place. *Location* in the source data.

- **county**. The name of the county (or similar administrative unit) in which the event's locality is situated, as recorded by human coder. *County* in the source data.

- **online**. Binary indicator for online-only events. 1 = yes, 0 = no.

- **type**. Type(s) of action (e.g. march, protest, demonstration, strike, counter-protest, sit-in), separated with semi-colons if more than one. *EventType* in the source data.

- **macroevent**. Where relevant, a string composed of a date followed by a text label that can be used to associate an event with other events. These get used two ways. First, across the entire dataset, these may be used to identify coordinated actions on or around a specific date (e.g., "20170121-womensmarch", "20210301-nyc-cancelrent"). Second, starting in 2021, these are also used to associate counter-protests with the protests they target (e.g., "20210304-byu-lgbtq"). In the latter case, both/all events get the same macroevent tag, and at least one of the events will have *type* equal to 'counter-protest'. In rare cases where an event is associated with more than one macroevent---e.g., it is part of a larger coordinated action and it is counter-protested---the macroevent ids are separated with semicolons.

- **actors**. The organization that organized the event (e.g. Women's March, Greenpeace, etc.) or, in some cases, the type of people participating (e.g., students, nurses), separated by semi-colons if more than one. *Actor* in the source data.

- **claims**. The claim(s) the participants make (e.g. for women's rights, anti-Muslim Ban, against racism, etc.). *Claim* in the source data.

- **valence**. Political valence of the event relative to President Trump, broadly construed. 2 = pro-Trump, 1 = anti-Trump, 0 = neither or unrelated. *Pro(2)/Anti(1)* in the source data.

- **issues**. String of semicolon-separated tags identifying political issues associated with the event (e.g., "democracy; women's rights" for events associated with the 2017 Women's March). These tags are encoded by applying keyword- and keyphrase-based regular expressions to the participant claims recorded in the *claims* field.

- **size_low**. Lowest reported participation count. Often (not always) the number cited by police or public officials. Vague estimates are interpreted as follows: "hundreds" = 200; "thousands" =2000; "tens of thousands" = 20000; "hundreds of thousands" = 200000. *EstimateLow* in the source data.

- **size_high**. Highest reported participant count. Often (not always) the number cited by event organizers and/or activists. Vague estimates are interpreted as follows: "hundreds" = 200; "thousands" =2000; "tens of thousands" = 20000; "hundreds of thousands" = 200000. *EstimateHigh* in the source data.

- **size_mean**. Average of *size_low* and *size_high* (which in most cases is the same value).

- **size_text**. In cases where crowd size is only described in vague terms (e.g., "dozens"), the word(s) used. *EstimateText* in the source data.

- **size_cat**. Ordered categorical indicator of crowd size, representing orders of magnitude and based on *size_mean*. 0 = unknown; 1 = 1-99; 2 = 100-999; 3 = 1,000-9,999; 4 = 10,000+.

- **arrests**. String, usually identifying the number of reported arrests, sometimes a phrase indicating ambiguity (e.g., "more than 5", "unclear"). *ReportedArrests* in the source data.

- **arrests_any**. Binary indicator for whether or not any arrests occurred. 1 = yes, 0 = no.

- **injuries_crowd**. String, usually identifying the number of protesters reportedly injured, sometimes a phrase indicating ambiguity (e.g., "more than 5", "unclear"). *ReportedParticipantInjuries* in the source data.

- **injuries_crowd_any**. Binary indicator for whether or not any protesters were reportedly injured. 1 = yes, 0 = no.

- **injuries_police**. String, usually identifying the number of police officers reportedly injured, sometimes a phrase indicating ambiguity (e.g., "more than 5", "unclear"). *ReportedPoliceInjuries* in the source data.

- **injuries_police_any**. Binary indicator for whether or not any police officers were reportedly injured. 1 = yes, 0 = no.

- **property_damage**. String, usually a binary indicator for whether or not any property damage occurred, sometimes a count, and from January 2021 forward, a semicolon-separated list of types of property damage reported (e.g., "graffiti; broken windows; cars damaged"). *ReportedPropertyDamage* in the source data.

- **property_damage_any**. Binary indicator for whether or not protesters caused any property damage. 1 = yes, 0 = no.

- **chemical_agents**. Binary indicator of whether or not police or other state security forces used tear gas or other chemical irritants such as pepper spray or pepper balls on protesters. *TearGas* in the source data; only coded starting in May 2020.

- **source_n**. URL of nth source, or description where none is available. *Sourcen* in the source data.

- **misc**. Miscellaneous additional information about the event as noted by the coder. *Misc.* or *Misc* in the source data.

- **final**. Binary indicator of whether or not CCC identifies the data for that month as "finalized." 1 = yes, 0 = no.

- **lat**. Latitude of locality in which the event took place, as resolved by Google Maps Geocoding API.

- **lon**. Longitude of locality in which the event took place, as resolved by Google Maps Geocoding API.

- **resolved_locality**. Name of the locality in which the event took place, as resolved by running *location* through the Google Maps Geocoding API.

- **resolved_county**. Name of the county in which that locality falls, as resolved by running *location* through the Google Maps Geocoding API..

- **resolved_state**. Postal abbreviation of the state or territory in which that locality falls, as resolved by the Google Maps Geocoding API.

- **fips_code**. Five-digit FIPS code for the county (or LA parish or AK borough or independent city or DC or U.S. territory). See 'data-compilation/fips_for_county_function.r' for details on how these are generated using the 'tigris' package and some custom code to handle various exceptions. When you load the data from this repository, you will probably need to add leading zeros back to codes that have them, because your software will probably read that column as integers instead of strings. In R, you could do this with `ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code)`.