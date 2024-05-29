# lists of regular expressions used frequently in analysis of ccc data

# for application to 'participant_measures' field since 2020
regex_actions <- list(

  # any guns
  armed = "(?<!water |squirt |bb |stun )\\b(hand ?)?guns?|rifle|(?:fire|side)arm",

  # any scuffling
  scuffle = "scuffle|push|shove|bump",

  # any assaults or fighting
  assault = "assault|punch|kick|attack|shot|stab|struck|brawl|fight|fought"

)

# for flagging events at schools of all kinds
regex_schools <- paste(
  
  c("college(?! (?:st(reet)?|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights|point|green|athletic))",
    "university(?! (?:st(reet)?|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights|city|behavioral|hospital|plaza|lakes|office|irving))",
    "school(?! (?:st(reet)?\\b|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights))",
    "\\bcooper union",
    "institute of technology",
    "\\bpoly(technic (state )?(?:institute|university))?",
    "auraria campus",
    "pentacrest",
    "(?:naval|air force|military) academy|west point(?! hwy)",
    "\\b(?:c|s)uny\\b",
    "\\buc\\b(?! theatre)"),

  collapse = "|"

)

# for application to 'organizations' field since 2020
regex_groups_farright <- list(
  
  `Proud Boys` = "proud boys",

  `Oath Keepers` = "oath keepers",

  `Three Percenters` = "three percenters|\\biii|threeper",

  `Boogaloo Bois` = "boog(aloo)?|kekoas|wolverine watchmen|citizens liberty|redacted republic",

  `Patriot Front` = "patriot front",

  `White Lives Matter` = "white lives matter|\\bwlm\\b",

  `Goyim Defense League` = "goyim defense league|\\bgdl\\b",

  `NSC-131` = "nsc.131",

  `Gays Against Groomers` = "gays against groomers",

  `Protect Texas Kids` = "protect texas kids"

)

regex_groups_white <- list(

  `Proud Boys` = "proud boys",

  `Patriot Front` = "patriot front",

  `White Lives Matter` = "white lives matter|\\bwlm\\b",

  `Goyim Defense League` = "g(oyim )?d(efense )?l(eague)?",

  `National Socialists` = "national socialist(?: club| movement)?|\\bnsc|natsoc",

  `Aryan Freedom Network` = "a(ryan )?f(reedom )?n(etwork)?",

  `Active Clubs` = "active club",

  `National Justice Party` = "national justice party",

  `ACTBAC` = "actbac",

  `Rose City Nationalists` = "rose city nationalist",

  `Sunshine State Nationalists` = "sunshine state nationalist",

  `Folksfront` = "folksfront",

  `League of the South` = "league of the south",

  `Sons of Confederate Veterans` = "sons of confederate veterans",

  `Russian Imperial Movement` = "russian imperial movement",

  `New Jersey European Heritage Association` = "n(ew )?j(ersey )?e(uropean )?h(eritage )?a(ssociation)?",

  `Ku Klux Klan` = "\\bkkk\\b|klan",

  `American Nationalist Initiative` = "\\ba(merican )?n(ationalist )?i(nitiative)?\\b",

  `Patriotic Alternative` = "patriotic alternative",

  `Blood Tribe` = "blood tribe",

  `Empire State Stormers` = "empire state stormers",

  `Crew 319` = "\\bcrew 319"

)

