library(tidyverse)

# comment this out if you're not me
setwd('nval/ad-hocs')

edge_date <- Sys.Date() - 1

# regex for schools

schools <- c("college(?! (?:st(reet)?|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights|point|green|athletic))",
             "university(?! (?:st(reet)?|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights|city|behavioral|hospital|plaza|lakes|office|irving))",
             "school(?! (?:st(reet)?\\b|ave(nue)?|r(oa)?d|cir(cle)?|dr(ive)?\\b|blvd|heights))",
             "\\bcooper union",
             "institute of technology",
             "\\bpoly(technic (state )?(?:institute|university))?",
             "auraria campus",
             "pentacrest",
             "(?:naval|air force|military) academy|west point(?! hwy)",
             "\\b(?:c|s)uny\\b",
             "\\buc\\b(?! theatre)")

regex_schools <- paste(schools, collapse = "|")

# get ccc data
ccc <- read.csv("c:/users/ulfel/documents/nval/ccc/data_clean/ccc_compiled.csv")

# pro-pal

pal <- ccc %>%
  filter(date >= "2023-10-07" & date <= edge_date) %>%
  filter(grepl("for palestinian (?:liberation|rights)|free palestine(?! from hamas)|in solidarity with (?:palestin|gaza)|in remembrance of palestin|ceasefire in gaza|against apartheid in israel|(?:for ending|against) israel's occupation of palestin|genocide of palestin", claims, ignore.case = TRUE, perl = TRUE)) %>%
  filter(!grepl("counter-protest", type)) %>%
  filter(!grepl("in solidarity with Israel", claims, ignore.case = TRUE)) %>%
  mutate(schools = ifelse(grepl(regex_schools, location_detail, ignore.case = TRUE, perl = TRUE), 1, 0),
         encampment = ifelse(grepl("encamp", participant_measures, ignore.case = TRUE), 1, 0),
         counterprotested = ifelse(!is.na(macroevent), 1, 0))

print(nrow(pal))
print(table(pal$schools))
print(table(pal$schools, pal$encampment))

pal_sch_sum <- pal %>%
  filter(schools == 1) %>%
  mutate(date = date(date)) |>
  mutate(school = map_chr(location_detail, function(x) {

      y <- str_split_1(x, ";")

      y <- str_trim(y)

      z <- grepl(regex_schools, y, perl = T, ignore.case = T)

      a <- str_trim(y[z])[1]

      return(a)

    })

  ) |>
  mutate(school = gsub(" - |-", " ", school),
         school = gsub(" at ", " ", school),
         school = gsub("State University of New York", "SUNY", school),
         school = gsub("City University of New York", "CUNY", school),
         school = gsub("\\bUC\\b", "University of California", school)) |>
  group_by(school, locality, state) %>%
  summarize(school = first(school),
            n = n(),
            date_min = min(date),
            date_max = max(date),
            encampment_days = sum(encampment, na.rm = T),
            arrests_any_sum = sum(arrests_any),
            arrests = sum(as.integer(arrests), na.rm = T),
            counters = sum(!is.na(macroevent)),
            injuries_crowd_any_sum = sum(injuries_crowd_any),
            injuries_police_any_sum = sum(injuries_police_any),
            property_damage_any_sum = sum(property_damage_any)) |>
  arrange(-n) |>
  mutate(school = gsub(", (?:Manhattan|Queens|Bronx|Staten Island|Brooklyn)", "", school))

write.csv(pal_sch_sum,
          sprintf("pal-sch-summary-%s.csv", gsub("-", "", edge_date)),
          row.names = F)

nrow(filter(pal, schools == 1))
filter(pal, schools == 1) %>% mutate(loc = paste(locality, state)) %>% { length(unique(.$loc)) }
nrow(filter(pal, schools == 1 & date >= "2024-04-17"))
nrow(filter(pal, schools == 1 & date < "2024-04-17"))
nrow(pal_sch_sum)
sum(pal_sch_sum$encampment_days > 0)
sum(pal_sch_sum$encampment_days)
sum(pal_sch_sum$arrests_any_sum > 0)
sum(pal_sch_sum$arrests_any_sum)
sum(pal_sch_sum$arrests)
sum(pal_sch_sum$counters)
sum(pal_sch_sum$injuries_crowd_any_sum > 0)
sum(pal_sch_sum$injuries_police_any_sum > 0)
sum(pal_sch_sum$property_damage_any_sum > 0)

png("ccc-pal-schools-daily-event-counts.png", res = 300, width = 16/2, height = 9/2, unit = "in")
filter(pal, schools == 1) |>
  mutate(date = date(date),
         encampment = ifelse(encampment == 1, "yes", "no")) |>
  group_by(date, encampment) |>
  tally() |>
  ggplot(aes(date, n, fill = encampment)) + 
    geom_col() +
    theme_minimal() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank()) +
    scale_x_date(breaks = seq(date("2023-10-01"), date("2024-05-01"), "month"),
                 date_labels = "%b %Y") +
    scale_fill_manual(values = wes_palette("Royal1")[c(1,2)],
                      name = "encampment") +
    labs(title = "Daily counts of U.S. schools with pro-Palestinian protest activity",
         subtitle = "October 7\u2013May 29, 2024",
         caption = "Source: Crowd Counting Consortium")
dev.off()

png("ccc-pal-schools-days.png", res = 300, width = 7, height = 7, unit = "in")
pal_sch_sum[1:30,] |>
  ggplot(aes(reorder(school, n, sum), n)) + 
    geom_col(fill = "gray60") +
    theme_minimal() +
    coord_flip() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(title = "Schools with most pro-Palestine protest days",
         subtitle = "October 7\u2013May 29, 2024",
         caption = "Source: Crowd Counting Consortium")
dev.off()

png("ccc-pal-schools-campdays.png", res = 300, width = 7, height = 7, unit = "in")
X <- arrange(pal_sch_sum, -encampment_days)[1:30,]
X |>
  ggplot(aes(reorder(school, encampment_days, sum), encampment_days)) + 
    geom_col(fill = "gray60") +
    theme_minimal() +
    coord_flip() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(title = "Schools with most pro-Palestine encampment days",
         subtitle = "October 7\u2013May 29, 2024",
         caption = "Source: Crowd Counting Consortium")
dev.off()

png("ccc-pal-schools-arrests.png", res = 300, width = 7, height = 7, unit = "in")
X <- arrange(pal_sch_sum, -arrests)[1:30,]
X |>
  ggplot(aes(reorder(school, arrests, sum), arrests)) + 
    geom_col(fill = "gray60") +
    theme_minimal() +
    coord_flip() +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          panel.grid.minor = element_blank(),
          panel.grid.major.y = element_blank()) +
    labs(title = "Schools with most arrests at pro-Palestine actions",
         subtitle = "October 7\u2013May 29, 2024",
         caption = "Source: Crowd Counting Consortium")
dev.off()
