ccc_issue_regex_list <- list(

  `animal rights` = "\\banimal\\b|(?:factory|meat) farm|\\bpeta\\b|\\bcircus|puppy mill|horse.drawn carriage|slaughter ?house|\\bfur\\b",

  `banking and finance` = "(?<!west )bank|financ|wall street|\\bnyse\\b|nasdaq|wells fargo|jpmorgan|credit card|debt|loan|stock market|monetary|interest rate|(?:in|di)vest|hedge fund|crypto(currenc)?|(?:bit|doge)coin|foreign exchange",

  `civil rights` = "civil rights|free (?:speech|assembly|religion)|freedom of (?:speech|assembly|religion)|(?:first|1st) amendment|freedom|censor|anti.protest",

  covid = "covid|corona(virus)?|\\b(anti.?)?mask\\b|fauci|pandemic|\\bcdc\\b|ventilator|personal protective equipment|\\bppe\\b|\\bre.?open|ivermectin|remdisvir|lockdown|(?:wuhan |china )flu|china.?virus",

  corruption = "\\bcorrupt|\\bbribe|graft",

  `criminal justice` = "criminal justice|prison|jail|carceral|(?:in|de)carcerat|parole|death (?:penalty|sentence)|capital punishment|\\bexecution|juvenile detention|\\binmate|wrongful (?:conviction|prosecution)|clemency|\\bfelon|ex-felon|correctional|cash bail|bail reform",

  democracy = "democracy|voter|voting rights|election|ballot|redistrict|gerrymander|\\bfascis.|\\bmarxis.|\\bcommunis.|anarchis.|socialis.|\\bcoup\\b|dc statehood|Trump won",

  development = "development|zoning|\\brezon|gentrif|historic preservation|construction(?! worker)|nimby|yimby|displacement",

  `disabled people's rights` = "disab(?:ilit|led?)|otherly abled?|\\bada\\b|accessib|\\bblind\\b|\\bdeaf\\b|wheelchair|special needs|down(\'s)? syndrome|tourette|\\bautis.{1,}",

  drugs = "\\bdrug|alcohol|marijuana|cannabis|\\bpot\\b|\\bcbd\\b|cocaine|opioid|opiate|narcotic|lsd|heroin|hemp|safe injection|overdose|oxycontin|\\bmeth\\b|methadone|methamphetamine|needle|syringe service|fentanyl",

  economy = "econom(?:y|ic)\\b|inflation|business|industry|manufactur(?:er|ing)|\\bfarm|agricultur|\\bjobs?\\b|employment|\\bwage\\b|\\bsalar(?:y|ied)|recession|financ|wall street|stock market|monetary|fiscal|interest rate|\\bbank|capitalis|\\brent\\b|eviction|hiring|tourism|basic income|\\bubi\\b|hunger|food insecurity|poverty|\\bpoor|corporat(?:e|ion)",

  education = "\\beducat(?:ion|or)|school|learning|student|pupil|\\bteach|faculty|universit(?:y|ies)|college|professor|principal|superintendent|classroom|\\bed\\b|campus|alumn|\\bpta\\b|\\btuition|betsy devos",

  energy = "energy|\\boil\\b|\\bcoal\\b|\\bgas\\b|fossil fuel|power plant|nuclear power|renewable energy|solar power|wind power|hydropower|frack|electricity|utility|nuclear waste|drilling|pipeline|\\bdapl\\b|\\bline 3|refinery",

  environment = "environment|\\bgreen\\b|climate|global warming|fossil fuel|renewable|clean energy|plastic|soil|\\bpollut|conservation|wildlife|endangered species|poach|ivory|\\bair\\b|\\bwater\\b|earth day|hydropower|pipeline|nuclear waste|hunting|public land|wetland|\\bepa\\b|anwr|\\bdapl\\b|dakota access|\\bpcb|injection well|paris accord|\\btree|carbon neutral|carbon tax|carbon emissions|ecocide",

  executive = "presiden(?:t|cy)|impeach|electoral college|\\btrump\\b|\\bobama\\b|\\bbiden\\b|\\bsanders\\b|\\bclinton\\b|\\bwarren\\b|mueller|russia investigation|\\bpence\\b|kamala harris|executive order|let\'s go brandon",

  `foreign affairs` = "Afghanistan|Albania|Algeria|Andorra|Angola|Antigua and Barbuda|Argentina|Armenia|Australia|Austria|Azerbaijan|Bahamas|Bahrain|Bangladesh|Barbados|Belarus|Belgium|Belize|Benin|Bhutan|Bolivia|Bosnia|Botswana|Brazil|Brunei|Bulgaria|Burkina Faso|Burundi|Cabo Verde|Cambodia|Cameroon|Canada|Central African Republic|Chad|Chile|China(?!town)|Tibet|Xinjiang|Uighur|Colombia|Comoros|Congo|Republic of Congo|Costa Rica|Cote D'Ivoire|Ivory Coast|Croatia|Cuba|Cyprus|Czech Republic|Czechia|Democratic People's Republic of Korea|DPRK|North Korea|Democratic Republic of the Congo|DROC|Denmark|Djibouti|Dominica|Dominican Republic|Ecuador|Egypt|El Salvador|Equatorial Guinea|Eritrea|Estonia|Ethiopia|Fiji|Finland|France|Gabon|Gambia|Georgia|Germany|Ghana|Greece|Grenada|Guatemala|Guinea|Guinea Bissau|Guyana|Haiti|Honduras|Hungary|Iceland|\\bIndia\\b|Kashmir|Indonesia|Iran|Iraq|Kurdistan|Ireland|Israel|Palestin|West Bank|Gaza Strip|Italy|Jamaica|Japan|Jordan|Kazakhstan|Kenya|Kiribati|Kuwait|Kyrgyzstan|Laos|Latvia|Lebanon|Lesotho|Liberia|Libya|Liechtenstein|Lithuania|Luxembourg|Madagascar|Malawi|Malaysia|Maldives|Mali|Malta|Marshall Islands|Mauritania|Mauritius|Mexico|Micronesia|Monaco|Mongolia|Montenegro|Morocco|Western Sahara|Mozambique|Myanmar|Burma|Namibia|Nauru|Nepal|Netherlands|New Zealand|Nicaragua|Niger|Nigeria|Norway|Oman|Pakistan|Palau|Panama|Papua New Guinea|Paraguay|Peru|Philippines|Poland|Portugal|Qatar|Republic of Korea|South Korea|Moldova|Romania|\\bRussia|Rwanda|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Samoa|San Marino|Sao Tome and Principe|Saudi|Senegal|Serbia|Seychelles|Sierra Leone|Singapore|Slovakia|Slovenia|Solomon Islands|Somalia|Somaliland|Puntland|South Africa|South Sudan|Spain|Sri Lanka|Sudan|Suriname|Eswatini|Swaziland|Sweden|Switzerland|Syria|Tajikistan|Thailand|Macedonia|Timor.Leste|East Timor|Togo|Tonga|Trinidad and Tobago|Tunisia|Turkey|Turkmenistan|Tuvalu|Uganda|Ukraine|United Arab Emirates|\\bUAE\\b|United Kingdom|\\bUK\\b|Britain|Northern Ireland|Tanzania|Zanzibar|Uruguay|Uzbekistan|Vanuatu|Venezuela|Viet Nam|Vietnam|Yemen|Zambia|Zimbabwe|united nations|\\bUN\\b|\\bNATO\\b|\\bAfrica\\b|\\bAsia\\b|World Trade Organization|\\bWTO\\b|\\bG[0-9]{1}\\b|World Bank|International Monetary Fund|\\bIMF\\b",

  guns = "\\bgun\\b|firearm|(?:second|2nd) amendment|\\b2a\\b|\\bnra\\b|national rifle association|concealed carry|open.carry|ar-15|(?:assault|automatic) rifle|bump stock|right to bear arms",

  healthcare = "\\bhealth( ?care)?\\b|doctor|physician|nurse|nursing|hospital|\\bclinic\\b|\\bpatient|medical|medic(?:ine|are|aid)|prescription|\\brx\\b|obamacare|\\baca\\b|affordable care act|preexisting condition|vaccin|\\bjab\\b|\\bvax|disease|cancer|depression|suicide|maternal care|dhhr|\\bhiv\\b|\\baids\\b|hiv.aids|birthing|family act|\\bahca\\b|osteopath|chiropract|acupunctur|\\bhealer|\\bpharma",

  housing = "\\bhous(?:e|ing|ed)\\b|(?:home|house)less|unhoused|\\beviction|\\brent\\b|mortgage|panhandl|\\btenant|home ?owner|(?:land|slum)lord|apartment|\\bcondo(minium)?\\b|\\b421a\\b",

  judiciary = "judicia|judge|lawyer|prosecutor|attorney|\\bda\\b|public defender|\\btrial\\b|\\bcourt\\b|gorsuch|kavanaugh|ginsburg|\\brbg\\b|amy coney barrett|\\bscotus\\b|\\balito\\b|roe v.? wade",

  labor = "\\blabor(er)?\\b|\\b(farm.?)?worker|employee|lockout|shutout|\\bunion|collective bargain|\\bpay\\b|\\bjob\\b|layoff|furlough|pension|\\bwages?\\b|\\bcontract\\b|working|benefits|compensation|paid leave|sick leave|maternity leave|paternity leave|hazardous conditions|pink slip|hiring|may day|right.to.work|workplace|hazard pay|furlough|profession|sex work",

  legislative = "legislat|\\blaw(?! enforcement)\\b|\\bbills?\\b|lawmaker|\\bsenat|representative|congress|rep\\.|sen\\.|general assembly|\\bhb ?\\d{1,}|\\bsb ?\\d{1,}|\\bab ?\\d{1,}|heroes act|build back better",

  `lgbtq rights`= "lgbt|lesbian|\\bgay\\b|bisexual|transgender|trans (?!-)|queer|pride (?:parade|march)|same.sex|conversion therapy|bathroom bill|rainbow capitalism|two.spirit|\\bhrt\\b|\\bcissue\\b",

  migration = "\\bimmigra|\\bmigrant|dreamers|daca|border|refugee|asylum|h-?1b|\\bvisa\\b|sanctuary cit|sanctuary state|illegal alien|undocumented|\\bice\\b|anti-ice|family separation|child separation|deportation|deportee|travel ban|travel eo|muslim ban|\\btps\\b|temporary protected status|\\bexile|\\bmigra\\b",

  military = "military|(?<!civil )\\b(anti.?)?war\\b(?! on )|\\bdod\\b|army|navy|air force|marines|space force|coast guard|(?<!confederate|union) soldier|veteran|us strike|air strike|airstrike|bombing|nuclear (?:weapon|arms|missile)|armed force|\\bva\\b|imperial|\\bvfw\\b|service member|fort hood|\\bicbm|peace",

  `native peoples' rights` = "indigenous|native (?:people|american)|\\btrib(?:al|e)\\b|\\bindians?\\b|land back|leonard peltier",

  patriotism = "patriot(?! missile)",

  policing = "polic(?:e|ing)|she[r]{1,2}iff|\\bdeput|law enforcement|\\bcops?\\b|(?:blue|black) lives matter|(?:back|defend) the blue|thin blue line|\\bblm\\b|school resource officer|\\bsro|\\bacab\\b|(?:ny|la|m)pd|f(uck )?12|george floyd|breonna taylor",

  racism = "\\b(anti-?)?racis|\\bracial|bigot|\\bhate\\b|discriminat|\\bredlin|diversity|prejudice|of color|\\bbipoc\\b|\\bblack|african.american|white (?:supremac|nationalis.)|swastika|blm|(?:black|all|white|asian|native) lives matter|no white guilt|george floyd|breonna taylor|alt.right|confedera(?:te|cy)|kkk|klan|mlk|conquistador|christopher columbus|affirmative action|hispanic|\\blatin[a-z]{1,2}\\b|\\bchican[a-z]{1,2}\\b|minority|aapi|\\basian\\b|critical race theory|\\bcrt\\b|1619 project",

  religion = "religio|evangeli|\\bgod\\b|\\bjesus\\b|\\ballah|christian|catholic|protestant|presbyterian|baptist|islam|muslim|buddhis|sikh|\\bhindu\\b|jewish|semiti|mormon|church|synagogue|temple|mosque|sharia|pray(er)?|zionis.|judaism|hijab|pastor|reverend|priest|cleric|\\bimam\\b|\\brabbi\\b|monk|worship|\\bsatan|secular|atheis.|\\bfaith\\b|appeal to heaven|bible|rosar(?:y|ies)|theo(?:logy|cracy)",

  `reproductive rights` = "reproductive rights|abortion|plann?ed parenthood|pro(?:\\s|-)(?:life|choice)|birth control|contracepti|condom|family planning|womb|uterus|\\bovar(?:y|ies)|\\broe( v.? wade)?\\b|\\brepro\\b|vasectom(?:y|ies)",

  science = "scien(?:ce|tist|tific)|biolog(?:y|ist)|astronom(?:y|er)|physic(?:s|ist)|chemist(ry)?|epidemiolog(?:y|ist)",

  `sexual violence` = "sexual (?:violence|assault|ha[r]{1,2}assment|misconduct)|\\brap(e|ists?)\\b|sex(ual)? (?:offender|traffic|abuse)|vanessa guillen",

  sports = "\\bsports?\\b|[[:alpha:]]{4,}ball|hockey|wrestling|\\bcycling|cyclo-?cross|biking|bmx|track and field|swimming|diving|soccer|tennis|golf|cheerlead(?:ing|er)|skiing|snowboarding|rock climbing|bouldering|martial arts|equestrian|\\bfishing|frisbee|gymnast|(?:figure|speed) skating|rodeo|(?:tri|bi|du)athlon|lacrosse|\\bpolo\\b|sailing|surfing|canoeing|kayaking|olympic(?:s| games)",

  taxes = "\\btax\\b(?! return)|\\btaxes\\b",

  transportation = "\\btransport(ation)?\\b|subway|\\btransit\\b|bicycle|\\bbike\\b|bi?cyclist|pedestrian|(?:high|free)way|interstate|\\broad(way)?\\b|(?<!wall )street|\\bcar\\b|\\bbus\\b|\\btrain\\b|rail(?:road|way)|tunnel|\\bbridge\\b|air(?:port|line)|ferry|\\btram\\b|\\btoll\\b",

  `women's rights` = "women|metoo|\\bsexis(?:t|m)|patriarch|\\bgender\\b|domestic violence|femicide|equal rights amendment|\\bmen\'?s rights|\\bmother|misogyn|uterus|feminis|pussy"

)

issues <- names(ccc_issue_regex_list)

claimcoder <- function(myvec) {

  map_chr(myvec, function(claim) {
  
    hits <- map_lgl(issues, function(issue) {

      grepl(ccc_issue_regex_list[[issue]], claim, ignore.case = TRUE, perl = TRUE)

    })

    paste(issues[hits], collapse = "; ")   

  })

}

# kludge for a couple of known issues
claimcoder_addendum <- function(df) {

  df$ClaimCodes <- replace(df$ClaimCodes,
                           df$ClaimCodes == "" & grepl("women's march", df$MacroEvent, ignore.case = TRUE),
                          "women's rights; democracy")

  df$ClaimCodes <- replace(df$ClaimCodes,
                           df$ClaimCodes == "" & grepl("teach|student|educat|faculty", df$Actor, ignore.case = TRUE) & grepl("fund|budget|firing", df$Claim, ignore.case = TRUE),
                          "education")

  return(df)

}