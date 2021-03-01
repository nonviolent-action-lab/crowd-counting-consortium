library(tidyverse)
library(rvest)

options(stringsAsFactors = FALSE)

rallylist_page_scraper <- function(url) {

  x <- read_html(url)

  title <- x %>% html_node("title") %>% html_text()

  description <- x %>% html_node("p") %>% html_text()

  image <- x %>% html_nodes("img") %>% html_attr("src") %>% purrr::pluck(2)
  if(length(image) == 0) { image <- "none given" }

  y <- x %>% html_nodes("div div") %>% html_text()

  organizer <- str_subset(y, "^Organizer:[[:graph:]]{1,}") %>% purrr::pluck(2) %>% str_replace(., "Organizer:", "")

  website <- str_subset(y, "^Event Website:[[:graph:]]{1,}") %>% str_replace(., "Event Website:", "")
  if(length(website) == 0) { website <- "none given" }

  raw_date <- str_subset(y, "^Date of Event:[[:graph:]]{1,}") %>% str_replace(., "Date of Event:", "")
  formatted_date <- as.Date(date, format = "%A, %d %B, %Y")

  time <- str_subset(y, "^Time of Event:[[:graph:]]{1,}") %>% str_replace(., "Time of Event:", "")

  location <- str_subset(y, "^City:[[:graph:]]{1,}") %>% str_replace(., "City:", "")

  address <- str_subset(y, "^Address:[[:graph:]]{1,}") %>% str_replace(., "Address:", "")

  df <- data.frame(location, raw_date, formatted_date, address, organizer, description, website, image)
  
  return(df)

}


url <- "https://rallylist.com/browse-protest-and-rallies/"

landing_page <- read_html(url)

max_page <- landing_page %>% html_nodes("a.page-numbers") %>% html_text() %>% unlist() %>% as.integer() %>% max(., na.rm = TRUE)

url_list <- map(seq(max_page), ~{
  target <- sprintf("https://rallylist.com/browse-protest-and-rallies/page/%s/", .)
  x <- read_html(target) %>% html_nodes(".more-link") %>% html_attr("href") })

page_links <- unique(unlist(url_list))
page_links <- page_links[!grepl("cancellations-due-to-coronavirus", page_links)]

kahuna <- map_dfr(page_links, rallylist_page_scraper)

output <- sprintf("c:/users/ulfel/documents/nval/ccc/rallylist/dump_%s.csv", Sys.Date())

write.csv(kahuna, output, row.names = FALSE)
