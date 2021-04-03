require(tidyverse)

options(stringsAsFactors = FALSE)

source("ccc_issue_regex_list.R")

ccc <- read.csv("https://raw.githubusercontent.com/nonviolent-action-lab/crowd-counting-consortium/master/ccc_compiled.csv")

ccc$ClaimCodes <- claimcoder(ccc$Claim)

ccc <- claimcoder_addendum("ClaimCodes", ccc)

issue_bars <- function(x) {

  require(zoo)

  ccc %>%
    mutate(yrmo = zoo::as.yearmon(Date), claim = str_detect(ClaimCodes, x)) %>%
    # next bit is just until data making catches up
    filter(as.Date(Date) < as.Date("2020-07-01")) %>%
    group_by(yrmo) %>%
    tally(claim) %>%
    ggplot(aes(yrmo, n)) + 
      geom_col(orientation = "x") +
    theme_minimal() +
    labs(title = sprintf("monthly count of events with claims related to %s", x)) + 
    theme(axis.title = element_blank())
  
}

setwd("c:/users/ulfel/documents/nval/ccc")

walk(issues, function(x) {

  png(sprintf("ccc-claim-tally-%s.png", x), res = 300, width = 16, height = 9, unit = "cm")
    print(issue_bars(x))
  dev.off()

})

write.csv(ccc, "c:/users/ulfel/documents/nval/ccc/data_clean/ccc_compiled_issues.csv", row.names = FALSE)