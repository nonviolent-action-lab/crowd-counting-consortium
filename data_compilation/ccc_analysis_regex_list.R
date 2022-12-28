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

# for application to 'organizations' field since 2020
regex_groups_farright <- list(
  
  `Proud Boys` = "proud boys",

  `Oath Keepers` = "oath keepers",

  `Three Percenters` = "three percenters|\\biii|threeper",

  `Boogaloo Bois` = "boog(aloo)?",

  `Patriot Front` = "patriot front",

  `White Lives Matter` = "white lives matter|\\bwlm\\b",

  `Goyim Defense League` = "goyim defense league|\\bgdl\\b",

  `NSC-131` = "nsc.131",

  `Gays Against Groomers` = "gays against groomers",

  `Protect Texas Kids` = "protect texas kids"

)

