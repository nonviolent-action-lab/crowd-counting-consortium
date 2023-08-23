ccc_issue_regex_list <- list(

  `animal rights` = "\\banimal\\b|(?:factory|meat) farm|\\bpeta\\b|\\bcircus|puppy mill|horse.drawn carriage|slaughter ?house|\\bfur\\b",

  `banking and finance` = "(?<!west )bank|financ|wall street|\\bnyse\\b|nasdaq|wells fargo|jpmorgan|credit card|debt|loan|stock market|monetary|interest rate|(?:in|di)vest|hedge fund|crypto(currenc)?|(?:bit|doge)coin|foreign exchange|private equity|warren buffet",

  `civil rights` = "civil rights|free (?:speech|assembly|religion)|freedom of (?:speech|assembly|religion)|(?:first|1st) amendment|freedom|censor|anti.protest",

  covid = "covid|corona(virus)?|\\b(anti.?)?mask\\b|fauci|pandemic|\\bcdc\\b|ventilator|personal protective equipment|\\bppe\\b|\\bre.?open|ivermectin|remdisvir|lockdown|(?:wuhan |china )flu|china.?virus",

  corruption = "\\bcorrupt|\\bbribe|graft|nepotis",

  `criminal justice` = "criminal justice|prison|jail|carceral|(?:in|de)carcerat|parole|death (?:penalty|sentence)|capital punishment|\\bexecution|juvenile detention|\\binmate|wrongful (?:conviction|prosecution)|clemency|\\bfelon|ex-felon|correctional|cash bail|bail reform|public defenders?",

  democracy = "democracy|voter|voting rights|election|ballot|redistrict|gerrymander|\\bfascis.|\\bmarxis.|\\bcommunis.|anarchis.|socialis.|\\bcoup\\b|dc statehood|Trump won",

  development = "development|zoning|\\brezon|gentrif|historic preservation|construction(?! worker)|nimby|yimby|displacement",

  `disability rights` = "disab(?:ilit|led?)|otherly abled?|\\bada\\b|accessibility|\\bblind\\b|\\bdeaf\\b|wheelchair|special needs|down(\'s)? syndrome|tourette|\\bautis.{1,}",

  drugs = "\\bdrug|alcohol|marijuana|cannabis|\\bpot\\b|\\bcbd\\b|cocaine|opioid|opiate|narcotic|lsd|heroin|hemp|safe injection|overdose|oxycontin|\\bmeth\\b|methadone|methamphetamine|needle|syringe service|fentanyl",

  economy = "econom(?:y|ic)\\b|inflation|business|industry|manufactur(?:er|ing)|\\bfarm\\b|farm(?:ing|er)|agricultur|\\bjobs?\\b|employment|\\bwages?\\b|\\bsalar(?:y|ied)|recession|financ|wall street|stock market|monetary|fiscal|interest rate|\\bbank|capitalis|\\brent\\b|eviction|hiring|tourism|basic income|\\bubi\\b|hunger|food insecurity|poverty|\\bpoor|corporat(?:e|ion)|class war(fare)?",

  education = "\\beducat(?:ion|or)|school|learning|student|pupil|\\bteach|faculty|universit(?:y|ies)|college|professor|principal|superintendent|classroom|\\bed\\b|campus|alumn|\\bpta\\b|\\btuition|betsy devos",

  energy = "energy|\\boil\\b|\\bcoal\\b|\\bgas\\b|fossil fuel|power plant|nuclear power|renewable energy|solar power|wind power|hydropower|frack|electricity|utility|nuclear waste|drilling|pipeline|\\bdapl\\b|\\bline 3|refinery",

  environment = "environment|\\bgreen\\b|climate|global warming|fossil fuel|renewable|clean energy|plastic|soil|\\bpollut|conservation|wildlife|endangered species|poach|ivory|\\bair\\b|\\bwater\\b|earth day|hydropower|pipeline|nuclear waste|hunting|public land|wetland|\\bepa\\b|anwr|\\bdapl\\b|dakota access|\\bpcb|injection well|paris accord|\\btree|carbon neutral|carbon tax|carbon emissions|ecocide|water protector|forest defender|willow project",

  `foreign affairs` = "\\bAfghanistan\\b|\\bAlbania\\b|\\bAlgeria\\b|\\bAndorra\\b|\\bAngola\\b|\\bAntigua and Barbuda\\b|\\bArgentina\\b|\\bArmenia\\b|\\bAustralia\\b|\\bAustria\\b|\\bAzerbaijan\\b|\\bBahamas\\b|\\bBahrain\\b|\\bBangladesh\\b|\\bBarbados\\b|\\bBelarus\\b|\\bBelgium\\b|\\bBelize\\b|\\bBenin\\b|\\bBhutan\\b|\\bBolivia\\b|\\bBosnia\\b|\\bBotswana\\b|\\bBrazil\\b|\\bBrunei\\b|\\bBulgaria\\b|\\bBurkina Faso\\b|\\bBurundi\\b|\\bCabo Verde\\b|\\bCambodia\\b|\\bCameroon\\b|\\bCanada\\b|\\bCentral African Republic\\b|\\bChad\\b|\\bChile\\b|\\bChina(?!town)\\b|\\bTibet\\b|\\bXinjiang\\b|\\bUighur\\b|\\bColombia\\b|\\bComoros\\b|\\bCongo\\b|\\bRepublic of Congo\\b|\\bCosta Rica\\b|\\bCote D'Ivoire\\b|\\bIvory Coast\\b|\\bCroatia\\b|\\bCuba\\b|\\bCyprus\\b|\\bCzech Republic\\b|\\bCzechia\\b|\\bDemocratic People's Republic of Korea\\b|\\bDPRK\\b|\\bNorth Korea\\b|\\bDemocratic Republic of the Congo\\b|\\bDROC\\b|\\bDenmark\\b|\\bDjibouti\\b|\\bDominica\\b|\\bDominican Republic\\b|\\bEcuador\\b|\\bEgypt\\b|\\bEl Salvador\\b|\\bEquatorial Guinea\\b|\\bEritrea\\b|\\bEstonia\\b|\\bEthiopia\\b|\\bFiji\\b|\\bFinland\\b|\\bFrance\\b|\\bGabon\\b|\\bGambia\\b|\\bGeorgia\\b|\\bGermany\\b|\\bGhana\\b|\\bGreece\\b|\\bGrenada\\b|\\bGuatemala\\b|\\bGuinea\\b|\\bGuinea Bissau\\b|\\bGuyana\\b|\\bHaiti\\b|\\bHonduras\\b|\\bHungary\\b|\\bIceland\\b|\\bIndia\\b|\\bKashmir\\b|\\bIndonesia\\b|\\bIran\\b|\\bIraq\\b|\\bKurdistan\\b|\\bIreland\\b|\\bIsrael|(?<!East )\\bPalestin(?:e|ian)\\b|\\bWest Bank\\b|\\bGaza Strip\\b|\\bItaly\\b|\\bJamaica\\b|\\bJapan\\b|\\bJordan\\b|\\bKazakhstan\\b|\\bKenya\\b|\\bKiribati\\b|\\bKuwait\\b|\\bKyrgyzstan\\b|\\bLaos\\b|\\bLatvia\\b|\\bLebanon\\b|\\bLesotho\\b|\\bLiberia\\b|\\bLibya\\b|\\bLiechtenstein\\b|\\bLithuania\\b|\\bLuxembourg\\b|\\bMadagascar\\b|\\bMalawi\\b|\\bMalaysia\\b|\\bMaldives\\b|\\bMali\\b|\\bMalta\\b|\\bMarshall Islands\\b|\\bMauritania\\b|\\bMauritius\\b|\\bMexico\\b|\\bMicronesia\\b|\\bMonaco\\b|\\bMongolia\\b|\\bMontenegro\\b|\\bMorocco\\b|\\bWestern Sahara\\b|\\bMozambique\\b|\\bMyanmar\\b|\\bBurma\\b|\\bNamibia\\b|\\bNauru\\b|\\bNepal\\b|\\bNetherlands\\b|\\bNew Zealand\\b|\\bNicaragua\\b|\\bNiger\\b|\\bNigeria\\b|\\bNorway\\b|\\bOman\\b|\\bPakistan\\b|\\bPalau\\b|\\bPanama\\b|\\bPapua New Guinea\\b|\\bParaguay\\b|\\bPeru\\b|\\bPhilippines\\b|\\bPoland\\b|\\bPortugal\\b|\\bQatar\\b|\\bRepublic of Korea\\b|\\bSouth Korea\\b|\\bMoldova\\b|\\bRomania\\b|\\b\\bRussia\\b|\\bRwanda\\b|\\bSaint Kitts and Nevis\\b|\\bSaint Lucia\\b|\\bSaint Vincent and the Grenadines\\b|\\bSamoa\\b|\\bSan Marino\\b|\\bSao Tome and Principe\\b|\\bSaudi\\b|\\bSenegal\\b|\\bSerbia\\b|\\bSeychelles\\b|\\bSierra Leone\\b|\\bSingapore\\b|\\bSlovakia\\b|\\bSlovenia\\b|\\bSolomon Islands\\b|\\bSomalia\\b|\\bSomaliland\\b|\\bPuntland\\b|\\bSouth Africa\\b|\\bSouth Sudan\\b|\\bSpain\\b|\\bSri Lanka\\b|\\bSudan\\b|\\bSuriname\\b|\\bEswatini\\b|\\bSwaziland\\b|\\bSweden\\b|\\bSwitzerland\\b|\\bSyria\\b|\\bTajikistan\\b|\\bThailand\\b|\\bMacedonia\\b|\\bTimor.Leste\\b|\\bEast Timor\\b|\\bTogo\\b|\\bTonga\\b|\\bTrinidad and Tobago\\b|\\bTunisia\\b|\\bTurkey\\b|\\bTurkmenistan\\b|\\bTuvalu\\b|\\bUganda\\b|\\bUkraine\\b|\\bUnited Arab Emirates\\b|\\bUAE\\b|\\bUnited Kingdom\\b|\\bUK\\b|\\bBritain\\b|\\bNorthern Ireland\\b|\\bTanzania\\b|\\bZanzibar\\b|\\bUruguay\\b|\\bUzbekistan\\b|\\bVanuatu\\b|\\bVenezuela\\b|\\bViet Nam\\b|\\bVietnam\\b|\\bYemen\\b|\\bZambia\\b|\\bZimbabwe\\b|\\bUnited Nations\\b|\\bUN\\b|\\bNATO\\b|\\bAfrica\\b|\\bAsia\\b|World Trade Organization|\\bWTO\\b|\\bG[0-9]{1}\\b|\\bWorld Bank\\b|International Monetary Fund|\\bIMF\\b|foreign policy|diplomacy",

  `free speech` = "free(dom of)? speech|(?:first|1st) amendment|censor(ship)?",

  guns = "(?<!water|bb )\\bguns?\\b|firearm|(?:second|2nd) amendment|\\b2a\\b|(?<!other )\\bnra\\b|national rifle association|concealed carry|open.carry|\\bar-15|\\bak-47|(?:assault|automatic) (?:rifle|weapon)|bump stock|right to bear arms|(?:school|mass) shooting|\\bbullets\\b|molon labe|come and take it",
  
  healthcare = "\\bhealth( ?care)?\\b|doctor|physician|nurse|nursing|hospital|\\bclinic\\b|\\bpatient|medical|medic(?:ine|are|aid)|prescription|\\brx\\b|obamacare|\\baca\\b|affordable care act|preexisting condition|vaccin|\\bjab\\b|\\bvax|disease|cancer|depression|suicide|maternal care|dhhr|\\bhiv\\b|\\baids\\b|hiv.aids|birthing|family act|\\bahca\\b|osteopath|chiropract|acupunctur|\\bhealer|\\bpharma|monkeypox|circumcision",

  housing = "\\b(un)?hous(?:es?|ing|ed)\\b|(?:home|house)less|\\beviction|\\brent\\b|mortgage|panhandl|\\btenant|home ?owner|(?:land|slum)lord|apartment|\\bcondo(minium)?s?\\b|\\b421a\\b|mobile home|trailer park",

  immigration = "\\bimmigra|\\bmigrant|dreamers|\\bdaca\\b|border|refugee|asylum|h-?1b|\\bvisa\\b|sanctuary (?:cit|state)|illegal alien|undocumented|\\bice\\b|anti-ice|family separation|child separation|deport(?:ation|ee)|travel (?:ban|eo)|muslim ban|\\btps\\b|temporary protected status|\\bexile|\\bmigra\\b|great replacement",

  `indigenous peoples` = "indigenous|native (?:people|american)|\\btrib(?:al|e)\\b|\\bindians?\\b|land back|leonard peltier|stolen land|\\bicwa\\b|\\bwiphala\\b",

  judiciary = "judicia|judge|lawyer|prosecutor|attorney|\\bda\\b|public defender|\\btrial\\b|\\bcourt\\b|gorsuch|kavanaugh|ginsburg|\\brbg\\b|amy coney barrett|\\bscotus\\b|\\balito\\b|roe v.? wade",

  labor = "\\blabor(er)?\\b|\\b(farm.?)?worker|employee|lockout|shutout|\\bunion|collective bargain|\\bpay\\b|\\bjob\\b|layoff|furlough|pension|\\bwages?\\b|\\bcontract\\b|working|benefits|compensation|paid leave|sick leave|maternity leave|paternity leave|hazardous conditions|pink slip|hiring|may day|right.to.work|workplace|hazard pay|furlough|profession|sex work",

  legislative = "legislat|\\blaw(?! enforcement)\\b|\\bbills?\\b|lawmaker|\\bsenat|representative|congress|rep\\.|sen\\.|general assembly|\\bhb ?\\d{1,}|\\bsb ?\\d{1,}|\\bab ?\\d{1,}|heroes act|build back better|codify",

  `lgbtqia`= "lgbt|lesbian|\\bgay\\b|(?:bi|pan|homo)sexual|transgender|trans (?!-)|queer|pride (?:parade|march)|same.sex|conversion therapy|bathroom bill|rainbow capitalism|two.spirit|\\bhrt\\b|\\bcissue\\b|non.?binary|stonewall|(anti-)?drag(?: queen| show| brunch| bingo| story| culture| convention| make.?up| performance)?\\b|\\bgender.(?:affirming|transition)|detransition|intersex|club q|gender (?:theory|dysphoria|ideology)|sisters of perpetual indulgence",

  military = "military|(?<!civil )\\b(anti.?)?war\\b(?! on )|\\bdod\\b|army|navy|air force|marines|space force|coast guard|(?<!confederate|union) soldier|veteran|us strike|air strike|airstrike|bombing|nuclear (?:weapon|arms|missile)|armed force|\\bva\\b|imperial|\\bvfw\\b|service member|fort hood|\\bicbm|peace",

  patriotism = "patriot(?! missile)|american flag",

  policing = "(?<!coochie )polic(?:e|ing)|she[r]{1,2}iff|\\bdeput|law enforcement|\\bcops?\\b|(?:blue|black) lives matter|(?:back|defend) the blue|thin blue line|\\bblm\\b|school resource officer|\\bsro|\\bacab\\b|(?:ny|la|m)pd|f(uck )?12|george floyd|breonna taylor",

  presidency = "presiden(?:t|cy)|impeach|electoral college|\\btrump\\b|\\bobama\\b|\\bbiden\\b|\\bsanders\\b|\\bclinton\\b|\\bwarren\\b|mueller|russia investigation|\\bpence\\b|kamala harris|executive order|let\'s go brandon|fjb",

  racism = "\\b(anti-?)?racis|\\bracial|bigot|\\bhate\\b|discriminat|\\bredlin|prejudice|of color|\\bbipoc\\b|\\bblack(?! friday| (american )?flag| ?rock)|african.american|white (?:supremac|nationalis.)|swastika|blm|(?:black|all|white|asian|native) lives matter|no white guilt|george floyd|breonna taylor|alt.right|confedera(?:te|cy)|kkk|klan|mlk|conquistador|christopher columbus|affirmative action|hispanic|\\blatin[a-z]{1,2}\\b|\\bchican[a-z]{1,2}\\b|minority|aapi|\\basian\\b|critical race theory|\\bcrt\\b|1619 project|\\bdei\\b|diversity equity (?:and|&) inclusion",

  religion = "religio|evangeli|\\bgod\\b|\\bjesus\\b|\\ballah|christian|catholic|protestant|presbyterian|baptist|islam|muslim|buddhis|sikh|\\bhindu\\b|jewish|semiti|mormon|church|synagogue|temple|mosque|\\bsharia\\b|pray(er)?|zionis.|judaism|hijab|pastor|reverend|priest|cleric|\\bimam\\b|\\brabbi\\b|\\bmonk\\b|worship|\\bsatan(?:ic|is.)?\\b|secular|atheis.|\\bpagan(is)?|\\bfaith\\b|appeal to heaven|bible|rosar(?:y|ies)|theo(?:logy|cracy)|christo.?fascis",

  `reproductive rights` = "reproductive rights|abortion|plann?ed parenthood|pro(?:\\s|-)(?:life|choice)|birth control|contracepti|condom|family planning|womb|uterus|\\bovar(?:y|ies)|\\broe( v.? wade)?\\b|\\brepro\\b|vasectom(?:y|ies)|mifepristone",

  science = "scien(?:ce|tist|tific)|biolog(?:y|ist)|astronom(?:y|er)|physic(?:s|ist)|chemist(ry)?|epidemiolog(?:y|ist)",

  `sexual violence` = "sexual (?:violence|assault|ha[r]{1,2}assment|misconduct)|\\brap(e|ists?)\\b|sex(ual)? (?:offender|traffic|abuse)|vanessa guillen",

  sports = "\\bsports?\\b|[[:alpha:]]{4,}ball|hockey|wrestling|\\bcycling|cyclo-?cross|biking|bmx|track and field|swimm(?:ing|er)|diving|soccer|tennis|golf|cheerlead(?:ing|er)|skiing|snowboarding|rock climbing|bouldering|martial arts|equestrian|\\bfishing|frisbee|gymnast|(?:figure|speed) skating|rodeo|(?:tri|bi|du)athlon|lacrosse|\\bpolo\\b|sailing|surfing|canoeing|kayaking|olympic(?:s| games)|\\bfifa\\b|world cup",

  taxes = "\\btax\\b(?! return)|\\btaxes\\b|taxation",

  transportation = "\\btransport(ation)?\\b|subway|\\btransit\\b|bicycl(?:e|ist|ing)|\\bbike\\b|pedestrian|(?:high|free|express)way|interstate|\\broad(way)?\\b|(?<!wall )street|\\bcar\\b|\\bbus\\b|\\btrain\\b|rail(?:road|way)|light rail|tunnel|\\bbridge\\b|air(?:port|line)|ferry|\\btram\\b|\\btoll\\b|scooter|hov lane|streetcar|turnpike",

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