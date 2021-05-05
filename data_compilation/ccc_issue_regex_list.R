ccc_issue_regex_list <- list(

  `animal rights` = "\\banimal\\b|meat farm|\\bpeta\\b|\\bcircus|puppy mill|horse.drawn carriage|slaughterhouse|\\bfur\\b|factory farm",

  `civil rights` = "civil rights|free speech|freedom of speech|first amendment|1st amendment|free assembly|freedom of assembly|freedom of religion|freedom|censor|anti-protest",

  covid = "covid|coronavirus|corona|\\bmask\\b|anti.mask|fauci|pandemic|\\bcdc\\b|ventilator|personal protective equipment|\\bppe\\b|\\breopen",

  corruption = "\\bcorrupt|\\bbribe|graft",

  `criminal justice` = "criminal justice|prison|jail|carceral|incarcerat|decarcerat|parole|death penalty|death sentence|capital punishment|\\bexecution|juvenile detention|\\binmate|wrongful conviction|clemency|\\bfelon|ex-felon|correctional|criminal violence",

  democracy = "democracy|voter|voting rights|election|ballot|redistrict|gerrymander|\\bfascis.|\\bmarxis.|\\bcommunis.|anarchis.|socialis.|\\bcoup\\b|dc statehood",

  development = "development|zoning|gentrif|historic preservation|construction(?! worker)|nimby|yimby",

  `disabled rights` = "disabilit|disable|otherly able|\\bada\\b|accessib|\\bblind\\b|\\bdeaf\\b|wheelchair|special needs|down's syndrome|down syndrome|tourette's|\\bautis.{1,}",

  drugs = "\\bdrug|alcohol|marijuana|cannabis|\\bpot\\b|\\bcbd\\b|cocaine|opioid|opiate|narcotic|lsd|heroin|hemp|safe injection|overdose|oxycontin|\\bmeth\\b|methadone|methamphetamine|needle",

  economy = "economy|economic|inflation|business|industry|manufacturing|\\bfarm|agricultur|\\bjob\\b|employment|housing|\\bwage\\b|\\bsalar|recession|financ|wall street|stock market|monetary|interest rate|\\bbank|capitalis|\\brent\\b|eviction|hiring|tourism|basic income|\\bubi\\b|hunger|food insecurity|poverty|poor",

  education = "\\beducat|school|learning|student|pupil|\\bteach|faculty|university|universities|college|professor|principal|superintendent|classroom|\\bed\\b|campus|alumn|\\bpta\\b|\\btuition|betsy devos",

  energy = "energy|\\boil\\b|\\bcoal\\b|\\bgas\\b|fossil fuel|power plant|nuclear power|renewable energy|solar power|wind power|hydropower|frack|electricity|utility|nuclear waste|drilling|pipeline|\\bdapl\\b|\\bline 3|refinery",

  environment = "environment|\\bgreen\\b|climate|global warming|fossil fuel|renewable|clean energy|plastic|soil|\\bpollut|conservation|wildlife|endangered species|poach|ivory|\\bair\\b|\\bwater\\b|earth day|hydropower|pipeline|nuclear waste|hunting|public land|wetland|\\bepa\\b|anwr|\\bdapl\\b|dakota access|\\bpcb|injection well|paris accord|\\btree|carbon neutral|carbon tax|carbon emissions",

  executive = "president|presidency|impeach|electoral college|\\btrump\\b|\\bobama\\b|\\bbiden\\b|\\bsanders\\b|\\bclinton\\b|\\bwarren\\b|mueller|russia investigation|\\bpence\\b|kamala harris|executive order",

  `foreign affairs` = "Afghanistan|Albania|Algeria|Andorra|Angola|Antigua and Barbuda|Argentina|Armenia|Australia|Austria|Azerbaijan|Bahamas|Bahrain|Bangladesh|Barbados|Belarus|Belgium|Belize|Benin|Bhutan|Bolivia|Bosnia|Botswana|Brazil|Brunei|Bulgaria|Burkina Faso|Burundi|Cabo Verde|Cambodia|Cameroon|Canada|Central African Republic|Chad|Chile|China|Tibet|Xinjiang|Uighur|Colombia|Comoros|Congo|Republic of Congo|Costa Rica|Cote D'Ivoire|Ivory Coast|Croatia|Cuba|Cyprus|Czech Republic|Czechia|Democratic People's Republic of Korea|DPRK|North Korea|Democratic Republic of the Congo|DROC|Denmark|Djibouti|Dominica|Dominican Republic|Ecuador|Egypt|El Salvador|Equatorial Guinea|Eritrea|Estonia|Ethiopia|Fiji|Finland|France|Gabon|Gambia|Georgia|Germany|Ghana|Greece|Grenada|Guatemala|Guinea|Guinea Bissau|Guyana|Haiti|Honduras|Hungary|Iceland|\\bIndia\\b|Kashmir|Indonesia|Iran|Iraq|Kurdistan|Ireland|Israel|Palestin|West Bank|Gaza Strip|Italy|Jamaica|Japan|Jordan|Kazakhstan|Kenya|Kiribati|Kuwait|Kyrgyzstan|Laos|Latvia|Lebanon|Lesotho|Liberia|Libya|Liechtenstein|Lithuania|Luxembourg|Madagascar|Malawi|Malaysia|Maldives|Mali|Malta|Marshall Islands|Mauritania|Mauritius|Mexico|Micronesia|Monaco|Mongolia|Montenegro|Morocco|Western Sahara|Mozambique|Myanmar|Burma|Namibia|Nauru|Nepal|Netherlands|New Zealand|Nicaragua|Niger|Nigeria|Norway|Oman|Pakistan|Palau|Panama|Papua New Guinea|Paraguay|Peru|Philippines|Poland|Portugal|Qatar|Republic of Korea|South Korea|Moldova|Romania|\\bRussia|Rwanda|Saint Kitts and Nevis|Saint Lucia|Saint Vincent and the Grenadines|Samoa|San Marino|Sao Tome and Principe|Saudi|Senegal|Serbia|Seychelles|Sierra Leone|Singapore|Slovakia|Slovenia|Solomon Islands|Somalia|Somaliland|Puntland|South Africa|South Sudan|Spain|Sri Lanka|Sudan|Suriname|Eswatini|Swaziland|Sweden|Switzerland|Syria|Tajikistan|Thailand|Macedonia|Timor.Leste|East Timor|Togo|Tonga|Trinidad and Tobago|Tunisia|Turkey|Turkmenistan|Tuvalu|Uganda|Ukraine|United Arab Emirates|\\bUAE\\b|United Kingdom|\\bUK\\b|Britain|Northern Ireland|Tanzania|Zanzibar|Uruguay|Uzbekistan|Vanuatu|Venezuela|Viet Nam|Vietnam|Yemen|Zambia|Zimbabwe|united nations|\\bUN\\b|\\bNATO\\b|\\bAfrica\\b|\\bAsia\\b|World Trade Organization|\\bWTO\\b|\\bG[0-9]{1}\\b|World Bank|International Monetary Fund|\\bIMF\\b|treaty",

  guns = "\\bgun\\b|firearm|second amendment|2nd amendment|\\b2a\\b|\\bnra\\b|national rifle association|concealed carry|open.carry|ar-15|assault rifle|automatic rifle|bump stock|right to bear arms",

  healthcare = "health|doctor|physician|nurse|nursing|hospital|\\bclinic\\b|\\bpatient|medical|medicine|medicare|medicaid|prescription|\\brx\\b|obamacare|\\baca\\b|affordable care act|preexisting condition|vaccin|disease|cancer|depression|suicide|maternal care|dhhr|\\bhiv\\b|\\baids\\b|hiv.aids|birthing|family act|\\bahca\\b|osteopath|chiropract|acupunctur|\\bhealer",

  housing = "homeless|\\bhousing|\\beviction|\\brent\\b|mortgage|panhandl|unhoused|\\btenant|homeowner",

  judiciary = "judicia|judge|lawyer|prosecutor|attorney|\\bda\\b|public defender|\\btrial\\b|\\bcourt\\b|gorsuch|kavanaugh|ginsburg|\\brbg\\b|amy coney barrett|\\bscotus\\b",

  labor = "\\blabor\\b|\\bworker|employee|lockout|shutout|\\bunion|collective bargain|\\bpay\\b|\\bjob\\b|layoff|furlough|pension|\\bwage|\\bcontract\\b|working|benefits|compensation|paid leave|sick leave|maternity leave|paternity leave|hazardous conditions|pink slip|hiring|may day|right.to.work|workplace|hazard pay|furlough|profession",

  legislative = "legislat|\\blaw(?! enforcement)|\\bbill\\b|\\bbills\\b|lawmaker|\\bsenat|representative|congress|rep\\.|sen\\.|general assembly|\\bhb \\d{1,}|\\bhb\\d{1,}|\\bsb \\d{1,}|\\bsb\\d{1,}|Heroes Act",

  `lgbtq rights`= "lgbt|lesbian|\\bgay\\b|bisexual|transgender|trans (?!-)|queer|pride parade|pride march|same.sex|conversion therapy|bathroom bill",

  migration = "\\bimmigra|\\bmigrant|dreamers|daca|border|refugee|asylum|h-1b|h1b|\\bvisa\\b|sanctuary cit|sanctuary state|illegal alien|undocumented|\\bice\\b|anti-ice|family separation|child separation|deportation|deportee|travel ban|travel eo|muslim ban|\\btps\\b|temporary protected status|\\bexile",

  military = "military|(?<!civil )\\bwar\\b(?! on drugs)|antiwar|\\dod\\b|army|navy|air force|marines|space force|coast guard|(?<!confederate|union) soldier|veteran|us strike|air strike|airstrike|bombing|nuclear weapon|armed force|\\bva\\b|imperial|\\bvfw\\b|service member|fort hood",

  `native peoples' rights` = "indigenous|native people|native american|\\btribal|\\btribe|\\bindian\\b|\\bindians\\b|land back|colonial|coloniz",

  policing = "police|policing|she[r]{1,2}iff|\\bdeput|law enforcement|\\bcop\\b|\\bcops\\b|blue lives matter|back the blue|thin blue line|defend the blue|black lives matter|\\bblm\\b|school resource officer",

  racism = "\\bracis|antiracis|anti-racis|\\bracial|bigot|\\bhate\\b|discriminat|segregat|\\bredlin|diversity|prejudice|of color|\\bbipoc\\b|\\bblack|african.american|white supremac|white nationalis.|swastika|blm|all lives matter|george floyd|breonna taylor|michael brown|alt-right|confederate|confederacy|kkk|klan|mlk|conquistador|christopher columbus|affirmative action|hispanic|\\blatin[a-z]{1,2}\\b|\\bchican[a-z]{1,2}\\b|minority|aapi|\\basian\\b",

  religion = "religio|evangeli|\\bgod\\b|\\bjesus\\b|christian|catholic|protestant|presbyterian|baptist|islam|muslim|buddhis|sikh|\\bhindu\\b|jewish|semiti|mormon|church|synagogue|temple|mosque|sharia|prayer|zionis.|judaism|hijab|pastor|reverend|priest|cleric|\\bimam\\b|\\brabbi\\b|monk|worship|\\bsatan|secular|atheis.",

  `reproductive rights` = "reproductive rights|abortion|planned parenthood|planed parenthood|pro.life|pro.choice|birth control|contracepti|condom|family planning",

  science = "science|scientific|scientist|biology|astronomy|physics|chemistry|biologist|astronomer|physicist|\\bchemist",

  `sexual violence` = "sexual violence|sexual assault|sexual ha[r]{1,2}assment|\\brape\\b|rapist|sex offender|sexual abuse|sex abuse|sex trafficking|vanessa guillen",

  taxes = "tax(?! return)",

  transportation = "\\btransport\\b|transportation|subway|\\btransit\\b|bicycle|\\bbike\\b|cyclist|pedestrian|highway|freeway|interstate|\\broad|street|\\bcar\\b|\\bbus\\b|\\btrain\\b|railroad|tunnel|\\bbridge\\b|airport|airline|ferry|\\btram\\b|\\btoll\\b",

  `women's rights` = "women|metoo|\\bsexis|patriarch|\\bgender\\b|domestic violence|femicide|equal rights amendment|take back the night|\\bmen's rights"

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

claimcoder_addendum <- function(issuecol, dat) {

  dat[,issuecol] <- ifelse(dat[,issuecol] == "" & grepl("women's march", dat[,"MacroEvent"], ignore.case = TRUE),
                           "women's rights; democracy",
                           dat[,issuecol])

  dat[,issuecol] <- ifelse(dat[,issuecol] == "" & grepl("teach|student|educat|faculty", dat[,"Actor"], ignore.case = TRUE) & grepl("fund|budget|firing", dat[,"Claim"], ignore.case = TRUE),
                           "education",
                           dat[,issuecol])

  return(dat)

}