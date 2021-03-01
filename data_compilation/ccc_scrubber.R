library(data.table)
library(tidyverse)
library(lubridate)
library(readxl)

options(stringsAsFactors = FALSE)

setwd("~/nval/ccc")

# function to fix typos in some dates, force common format
# note that this can't be piped straight from file reading because
# it refers to whole dataset to get median year
datescrub <- function(x) {

  require(lubridate)

  yr <- year(x)
  medyr <- median(year(dat$Date), na.rm = TRUE)
  yr <- ifelse(yr != medyr, medyr, yr)
  month <- month(x)
  month <- ifelse(nchar(month) == 1, sprintf("0%s", month), month)
  day <- mday(x)
  day <- ifelse(nchar(day) == 1, sprintf("0%s", day), day)

  ifelse(is.na(yr) | is.na(month) | is.na(day), NA, paste(yr, month, day, sep = "-"))

}