# Crowd Counting Consortium Crowd Data Dictionary #
This dictionary describes the compiled and cleaned version of the Crowd Counting Consortium (CCC) Crowd Data stored in this repository. Columns that appear in some of the CCC Google Sheets but are specific to certain months or macro-events have been dropped; some columns have been added or modified to facilitate mapping and analysis; and events occurring outside the United States and U.S. territories have been omitted. If you want to work with those columns or events, or just want to work with the data in its raw form, please use the original Google Sheets,  which you can access from the CCC website ([here](https://sites.google.com/view/crowdcountingconsortium/view-download-the-data)).

- **Date**. Date of event in YYYY-MM-DD format, starting with the earliest action in the locality.

- **CityTown**. Name of locality in which the event took place.

- **StateTerritory**. Two-letter U.S. postal abbreviation for the state or territory in which the event took place.

- **Location**. Concatenation of CityTown and StateTerritory in "[locality], [state]" format used for geocoding.

- **LocationDetail**. If known, details on the location within the city or town where the action took place.

- **Online**. Binary indicator for online-only events. 1 = yes, 0 = no.

- **EventType**. Type of action (e.g. march, protest, demonstration, strike, riot). If more than one, separate with semi-colons.

- **MacroEvent**. Where relevant, a string identifying a larger coordinated action or wave with which an event is associated (e.g., "Women's March (January 2017)", "Movement for Black Lives (2020)", or "Climate Strike (March 15, 2019)").

- **Actor**. Who organized the action (e.g. Women's March, Greenpeace, etc.). If not known, enter "general protestors." If more than one actor, separate with semi-colons.

- **Claim**. Type of claim(s) involved (e.g. women's rights, anti-Muslim Ban, environmental rights, etc.). If more than one, separate with semi-colons. If not known, enter "NA."

- **ClaimType**. Valence of event relative to President Trump, broadly construed. 2 = pro-Trump, 1 = anti-Trump, 0 = neither or unrelated.

- **EstimateLow**. Lowest reported participation count. Often (not always) the number cited by police or public officials. Vague estimates are interpreted as follows: "hundreds" = 200; "thousands" =2000; "tens of thousands" = 20000; "hundreds of thousands" = 200000.

- **EstimateHigh**. Highest reported participant count. Often (not always) the number cited by event organizers and/or activists. Vague estimates are interpreted as follows: "hundreds" = 200; "thousands" =2000; "tens of thousands" = 20000; "hundreds of thousands" = 200000.

- **EstimateText**. In cases where crowd size is only described in vague terms (e.g., "dozens"), the word(s) used.

- **EstimateCat**. Ordered categorical indicator of crowd size, representing orders of magnitude and based on EstimateLow. 0 = unknown; 1 = 1-99; 2 = 100-999; 3 = 1,000-9,999; 4 = 10,000+.

- **ReportedArrests**. Number of participants reportedly arrested, if applicable.

- **ReportedParticipantInjuries**. Number of participants with reported injuries, if applicable.

- **ReportedPoliceInjuries**. Number of police reportedly injured, if applicable.

- **ReportedPropertyDamage**. 1 if property was reportedly damaged, 0 if otherwise.

- **TownsCities**. Coded 1 if this is an independent event (i.e., not already listed under another city/town name on the same date), 0 otherwise.

- **Events**. Coded 1 if this is an independent event (i.e., not already listed under another city/town name on the same date), 0 if otherwise. A protest and counter-protest in the same location on the same day both get a 1.

- **Source1**. URL or description of source for EstimateText or EstimateLow.

- **Source2**. URL of source for EstimateHigh, when relevant.

- **Source3**. URL of additional source, when relevant.

- **Misc**. Miscellaneous additional information.

- **Final**. Binary indicator of whether or not CCC identifies the data for that month as "finalized." 1 = yes, 0 = no.

- **lat**. Latitude of locality in which the event took place, as geocoded by passing Location to the Google Maps Geocoding API.

- **lon**. Longitude of locality in which the event took place, as geocoded by passing Location the Google Maps Geocoding API.
