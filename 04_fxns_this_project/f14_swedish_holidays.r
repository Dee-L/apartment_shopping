# Purpose: Swedish holidays functions
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

###Swedish holidays and pinch days
is_newyearsday <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 1 &
           mday(date_as_yyyy_mm_dd) == 1,
         TRUE,
         FALSE)}

is_twelfthnight <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 1 &
           mday(date_as_yyyy_mm_dd) == 5,
         TRUE,
         FALSE)}

is_epiphany <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 1 &
           mday(date_as_yyyy_mm_dd) == 6,
         TRUE,
         FALSE)}

is_walpurgis <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 4 &
           mday(date_as_yyyy_mm_dd) == 30,
         TRUE,
         FALSE)}

is_mayday <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 5 &
           mday(date_as_yyyy_mm_dd) == 1,
         TRUE,
         FALSE)}

is_nationalday <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 6 &
           mday(date_as_yyyy_mm_dd) == 6,
         TRUE,
         FALSE)}

is_halloween <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 10 &
           mday(date_as_yyyy_mm_dd) == 31,
         TRUE,
         FALSE)}

is_christmaseve <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 12 &
           mday(date_as_yyyy_mm_dd) == 24,
         TRUE,
         FALSE)}

is_christmasday <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 12 &
           mday(date_as_yyyy_mm_dd) == 25,
         TRUE,
         FALSE)}

is_seconddayofchristmas <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 12 &
           mday(date_as_yyyy_mm_dd) == 26,
         TRUE,
         FALSE)}

is_newyearseve <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 12 &
           mday(date_as_yyyy_mm_dd) == 31,
         TRUE,
         FALSE)}

is_allsaintsday <- function(date_as_yyyy_mm_dd) {
  ifelse((month(date_as_yyyy_mm_dd) == 10 &
            mday(date_as_yyyy_mm_dd) == 31 &
            wday(date_as_yyyy_mm_dd) == 7) |
           (month(date_as_yyyy_mm_dd) == 11 &
              mday(date_as_yyyy_mm_dd) %in% 1:6 &
              wday(date_as_yyyy_mm_dd) == 7),
         TRUE,
         FALSE)}

is_allsaintseve <- function(date_as_yyyy_mm_dd) {
  next_day <- date_as_yyyy_mm_dd %>% as.Date + 1
  ifelse(is_allsaintsday(next_day %>% as.character),
         TRUE,
         FALSE)
}

is_midsummersday <- function(date_as_yyyy_mm_dd) {
  ifelse(month(date_as_yyyy_mm_dd) == 6 &
           mday(date_as_yyyy_mm_dd) %in% 20:26 &
           wday(date_as_yyyy_mm_dd) == 7,
         TRUE,
         FALSE)}

is_midsummerseve <- function(date_as_yyyy_mm_dd) {
  next_day <- date_as_yyyy_mm_dd %>% as.Date + 1
  ifelse(is_midsummersday(next_day %>% as.character),
         TRUE,
         FALSE)
}

is_eastersunday <- function(date_as_yyyy_mm_dd) {
  ifelse(date_as_yyyy_mm_dd %in%
           c("2009-04-12",
             "2010-04-04",
             "2011-04-24",
             "2012-04-08",
             "2013-03-31",
             "2014-04-20",
             "2015-04-05",
             "2016-03-27",
             "2017-04-16",
             "2018-04-01",
             "2019-04-21"),
         TRUE,
         FALSE)}

is_eastersundayeve <- function(date_as_yyyy_mm_dd) {
  next_day <- date_as_yyyy_mm_dd %>% as.Date + 1
  
  ifelse(is_eastersunday(next_day %>% as.character),
         TRUE,
         FALSE)
}

is_eastermonday <- function(date_as_yyyy_mm_dd) {
  previous_day <- date_as_yyyy_mm_dd %>% as.Date - 1
  
  ifelse(is_eastersunday(previous_day %>% as.character),
         TRUE,
         FALSE)
}

is_goodfriday <- function(date_as_yyyy_mm_dd) {
  two_days_later <- date_as_yyyy_mm_dd %>% as.Date + 2
  
  ifelse(is_eastersunday(two_days_later %>% as.character),
         TRUE,
         FALSE)
}

is_ascension <- function(date_as_yyyy_mm_dd) {
  thirtynine_days_earlier <- date_as_yyyy_mm_dd %>% as.Date - 39
  
  ifelse(is_eastersunday(thirtynine_days_earlier %>% as.character),
         TRUE,
         FALSE)
}

is_pentecost <- function(date_as_yyyy_mm_dd) {
  fortynine_days_earlier <- date_as_yyyy_mm_dd %>% as.Date - 49
  
  ifelse(is_eastersunday(fortynine_days_earlier %>% as.character),
         TRUE,
         FALSE)
}

is_pentecosteve <- function(date_as_yyyy_mm_dd) {
  next_day <- date_as_yyyy_mm_dd %>% as.Date + 1
  
  ifelse(is_pentecost(next_day %>% as.character),
         TRUE,
         FALSE)
}

is_swedish_red_or_pink_day <- function(date_as_yyyy_mm_dd) {
  ifelse(
    (is_newyearsday(date_as_yyyy_mm_dd)) |
      (is_twelfthnight(date_as_yyyy_mm_dd)) |
      (is_epiphany(date_as_yyyy_mm_dd)) |
      (is_goodfriday(date_as_yyyy_mm_dd)) |
      (is_eastersundayeve(date_as_yyyy_mm_dd)) |
      (is_eastersunday(date_as_yyyy_mm_dd)) |
      (is_eastermonday(date_as_yyyy_mm_dd)) |
      (is_walpurgis(date_as_yyyy_mm_dd)) |
      (is_mayday(date_as_yyyy_mm_dd)) |
      (is_ascension(date_as_yyyy_mm_dd)) |
      (is_pentecosteve(date_as_yyyy_mm_dd)) |
      (is_pentecost(date_as_yyyy_mm_dd)) |
      (is_midsummerseve(date_as_yyyy_mm_dd)) |
      (is_midsummersday(date_as_yyyy_mm_dd)) |
      (is_nationalday(date_as_yyyy_mm_dd)) |
      (is_halloween(date_as_yyyy_mm_dd)) |
      (is_allsaintseve(date_as_yyyy_mm_dd)) |
      (is_allsaintsday(date_as_yyyy_mm_dd)) |
      (is_christmaseve(date_as_yyyy_mm_dd)) |
      (is_christmasday(date_as_yyyy_mm_dd)) |
      (is_seconddayofchristmas(date_as_yyyy_mm_dd)) |
      (is_newyearseve(date_as_yyyy_mm_dd)),
    TRUE,
    FALSE)
}

is_swedish_pinch_day <- function(date_as_yyyy_mm_dd) {
  previous_day <- date_as_yyyy_mm_dd %>% as.Date - 1
  next_day <- date_as_yyyy_mm_dd %>% as.Date + 1
  
  ifelse((is_swedish_red_or_pink_day(previous_day) &
            wday(date_as_yyyy_mm_dd) == 6) | 
           (is_swedish_red_or_pink_day(next_day) &
              wday(date_as_yyyy_mm_dd) == 2),
         TRUE,
         FALSE)
}

is_swedish_day_off <- function(date_as_yyyy_mm_dd) {
  ifelse(
    (is_swedish_red_or_pink_day(date_as_yyyy_mm_dd)) |
      (is_swedish_pinch_day(date_as_yyyy_mm_dd)),
    TRUE,
    FALSE)
}
