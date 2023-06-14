# Crowd Counting Consortium Crowd Data Dictionary #
This dictionary describes the compiled and cleaned version of the Crowd Counting Consortium (CCC) Crowd Data stored in this repository. Columns that appear in some of the CCC Google Sheets but are specific to certain months or macro-events have been dropped; some columns have been added or modified to facilitate mapping and analysis; and events occurring outside the United States and U.S. territories have been omitted. If you want to work with those columns or events, or just want to work with the data in its raw form, please use the original Google Sheets,  which you can access from the CCC website ([here](https://sites.google.com/view/crowdcountingconsortium/view-download-the-data)).

- **date**. Date of event in YYYY-MM-DD format, starting with the earliest action in the locality. *Date* or *date* in the source data.

- **locality**. Name of locality in which the event took place. *CityTown* in the source data from 2017-2020; *locality* in source data from 2021-.

- **state**. Two-letter U.S. postal abbreviation for the state or territory in which the event took place. *StateTerritory* in source data from 2017-2020; *state* in source data from 2021-.

- **location_detail**. If known, details on the location within the city or town where the action took place. *Location* or *location* in the source data.

- **online**. Binary indicator for online-only events. 1 = yes, 0 = no.

- **type**. Type(s) of action (e.g. march, protest, demonstration, strike, counter-protest, sit-in), separated with semi-colons if more than one. *EventType* in source data for 2017-2020l *type* in source data for 2021-.

- **title**. Name of action, if given. *title* in source data (2021- only).

- **macroevent**. Where relevant, a unique id that associates a counter-protest with the event it targeted. These strings are composed of a date, a location, and something about the nature of the event, all separated by hyphens (e.g., "20220624-phoenix-abortion"). In most cases, these ids will uniquely identify pairs of events. In cases where the counter-protest is itself countered, however---e.g., a community defense action in response to a protest targeting an LGBTQ+ pride festival---the additional events are given the same id, so these clusters will sometimes include three or more events. [NB. Until December 2022, this field also included ids for events that were linked in other ways, and, as a result, events could have more than one macroevent id attached to them. We decided to simplify this field to facilitate analysis of cases where protesters and counter-protesters directly interact---the primary use case in our own research---and have revised the historical data accordingly.]

- **actors**. The organization that organized the event (e.g. Women's March, Greenpeace, etc.) or, in some cases, the type of people participating (e.g., students, nurses), separated by semi-colons if more than one. *Actor* in source data for 2017-2020; concatenation of *organizations* and *participants* in source data from 2021-.

- **organizations**. Names of organizations that participated in event. *organizations* in source data (2021- only).

- **participants**. Descriptors of participants in the event. *participants* in source data (2021- only).

- **claims**. The claim(s) the participants make (e.g. for women's rights, anti-Muslim Ban, against racism, etc.), as summarized by coders. *Claim* or *claims* in the source data. Distinct claims are separated by semicolons or commas through 2020, then only commas from 2021 onward. Starting in spring 2022, the strings in this field also include verbatim phrases from protesters' placards, banners, chants, and the like (see the [Coding Guidelines](https://docs.google.com/document/d/1oaOf9s72FQnzQA8sbE8h0PwMIZLP6p0EDUV2ya065is/edit?usp=sharing) for details). All coder-summarized claims have a distinct syntax, so they can be recognized by the following regular expression: ```"^(?:for|against) |^in [[:alpha:]]{3,} (?:of|with)"```. To isolate verbatim claim phrases, split the 'claims' strings by the commas, trim leading and trailing white space, then filter out all results recognized by that regex. What's left will be the verbatim claim phrases.

- **valence**. Political valence of the event, broadly construed. 2 = pro-Trump (during Trump presidency)/right wing (after Trump presidency), 1 = anti-Trump (during Trump presidency)/left wing (after Trump presidency), 0 = neither. *Pro(2)/Anti(1)* in the source data from 2017-2020; *valence* in source data for 2021-.

- **issues**. String of semicolon-separated tags identifying political issues (or themes) associated with the event (e.g., "democracy; women's rights" for events associated with the 2017 Women's March). These tags are encoded by applying keyword- and keyphrase-based regular expressions to the participant claims recorded in the *claims* field.

- **issues_major**. For events since 2021 only, and most reliably for events since spring 2022: string of semicolon-separated tags identifying the main political issues (or themes) associated with the event, as judged by the human coder or coders. These tags are encoded by applying keyword- and keyphrase-based regular expressions to only the coder-summarized claims in the *claims* field; the verbatim elements of that field are omitted. These are a subset of the issue tags that appear in the *issues* column. This column is NA for all events before 2021, but the *issues* field is essentially the same thing for that period (before we started including verbatim claims).

- **size_low**. Lowest reported participation count. Often (not always) the number cited by police or public officials. Vague estimates are interpreted as follows: "hundreds" = 200; "thousands" =2000; "tens of thousands" = 20000; "hundreds of thousands" = 200000. *EstimateLow* in source data from 2017-2020; *size_low* in source data from 2021-.

- **size_high**. Highest reported participant count. Often (not always) the number cited by event organizers and/or activists. Vague estimates are interpreted as follows: "hundreds" = 200; "thousands" =2000; "tens of thousands" = 20000; "hundreds of thousands" = 200000. *EstimateHigh* in source data from 2017-2020; *size_high* in source data from 2021-.

- **size_mean**. Average of *size_low* and *size_high* (which, in many cases, is the same value or NA).

- **size_text**. In cases where crowd size is only described in vague terms (e.g., "dozens"), the word(s) used. *EstimateText* in the source data from 2017-2020; *size_text* in source data from 2021-.

- **size_cat**. Ordered categorical indicator of crowd size, representing orders of magnitude and based on *size_mean*. 0 = unknown; 1 = 1-99; 2 = 100-999; 3 = 1,000-9,999; 4 = 10,000+.

- **arrests**. String, usually identifying the number of reported arrests, sometimes a phrase indicating ambiguity (e.g., "more than 5", "unclear"). *ReportedArrests* in the source data from 2017-2020; *arrests* in source data from 2021-.

- **arrests_any**. Binary indicator for whether or not any arrests occurred. 1 = yes, 0 = no.

- **injuries_crowd**. String, usually identifying the number of protesters reportedly injured, sometimes a phrase indicating ambiguity (e.g., "more than 5", "unclear"). *ReportedParticipantInjuries* in the source data from 2017-2020; *participant_injuries* in source data from 2021-.

- **injuries_crowd_any**. Binary indicator for whether or not any protesters were reportedly injured. 1 = yes, 0 = no.

- **injuries_police**. String, usually identifying the number of police officers reportedly injured, sometimes a phrase indicating ambiguity (e.g., "more than 5", "unclear"). *ReportedPoliceInjuries* in the source data from 2017-2020; *police_injuries* in source data from 2021-.

- **injuries_police_any**. Binary indicator for whether or not any police officers were reportedly injured. 1 = yes, 0 = no.

- **property_damage**. String, usually a binary indicator for whether or not any property damage occurred, sometimes a count. For 2021 forward, a semicolon-separated list of types of property damage reported (e.g., "graffiti; broken windows; cars damaged"). *ReportedPropertyDamage* in the source data from 2017-2020; *property_damage* in source data from 2021-.

- **property_damage_any**. Binary indicator for whether or not protesters caused any property damage. 1 = yes, 0 = no.

- **chemical_agents**. Binary indicator of whether or not police or other state security forces used tear gas or other chemical irritants such as pepper spray or pepper balls on protesters. *TearGas* in the source data for May-December 2020. Based on NLP of *police_measures* for 2021-.

- **participant_measures**. String summarizing notable partipant actions (e.g., "black bloc; marched in street" or "blockaded building entrances").

- **police_measures**. String summarizing police posture and notable actions (e.g., "called to scene; riot gear; declared unlawful assembly; pepper spray").

- **participant_deaths**. Integer count of participant deaths occurring at event (2021- only).

- **police_deaths**. Integer count of police deaths occurring at event (2021- only).

- **source_n**. URL of nth source, or description where none is available. *Sourcen* in the source data.

- **notes**. Miscellaneous additional information about the event as noted by the coder. *Misc.* or *Misc* in the source data for 2017-2020; *notes* in source data for 2021-.

- **lat**. Latitude of locality in which the event took place, as resolved by Google Maps Geocoding API.

- **lon**. Longitude of locality in which the event took place, as resolved by Google Maps Geocoding API.

- **resolved_locality**. Name of the locality in which the event took place, as resolved by running *location* through the Google Maps Geocoding API.

- **resolved_county**. Name of the county in which that locality falls, as resolved by running *location* through the Google Maps Geocoding API..

- **resolved_state**. Postal abbreviation of the state or territory in which that locality falls, as resolved by the Google Maps Geocoding API.

- **fips_code**. Five-digit FIPS code for the county (or LA parish or AK borough or independent city or DC or U.S. territory). See 'data-compilation/fips_for_county_function.r' for details on how these are generated using the 'tigris' package and some custom code to handle various exceptions. When you load the data from this repository, you will probably need to add leading zeros back to codes that have them, because your software will probably read that column as integers instead of strings. In R, you could do this with `ifelse(nchar(fips_code) == 4, paste0("0", fips_code), fips_code)`.
