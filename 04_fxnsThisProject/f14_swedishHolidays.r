# Purpose: Swedish holidays functions
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "lubridate"
    )

installMyPkgs(pkgs)

###Swedish holidays and pinch days
isNewYearsDay <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 1 &
           lubridate::mday(dateAsYYYYMMDD) == 1,
         TRUE,
         FALSE)}

isTwelfthNight <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 1 &
           lubridate::mday(dateAsYYYYMMDD) == 5,
         TRUE,
         FALSE)}

isEpiphany <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 1 &
           lubridate::mday(dateAsYYYYMMDD) == 6,
         TRUE,
         FALSE)}

isWalpurgis <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 4 &
           lubridate::mday(dateAsYYYYMMDD) == 30,
         TRUE,
         FALSE)}

isMayday <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 5 &
           lubridate::mday(dateAsYYYYMMDD) == 1,
         TRUE,
         FALSE)}

isNationalDay <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 6 &
           lubridate::mday(dateAsYYYYMMDD) == 6,
         TRUE,
         FALSE)}

isHalloween <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 10 &
           lubridate::mday(dateAsYYYYMMDD) == 31,
         TRUE,
         FALSE)}

isChristmasEve <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 12 &
           lubridate::mday(dateAsYYYYMMDD) == 24,
         TRUE,
         FALSE)}

isChristmasDay <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 12 &
           lubridate::mday(dateAsYYYYMMDD) == 25,
         TRUE,
         FALSE)}

isSecondDayOfChristmas <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 12 &
           lubridate::mday(dateAsYYYYMMDD) == 26,
         TRUE,
         FALSE)}

isNewYearsEve <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 12 &
           lubridate::mday(dateAsYYYYMMDD) == 31,
         TRUE,
         FALSE)}

isAllSaintsDay <- function(dateAsYYYYMMDD) {
  ifelse((lubridate::month(dateAsYYYYMMDD) == 10 &
            lubridate::mday(dateAsYYYYMMDD) == 31 &
            lubridate::wday(dateAsYYYYMMDD) == 7) |
           (lubridate::month(dateAsYYYYMMDD) == 11 &
              lubridate::mday(dateAsYYYYMMDD) %in% 1:6 &
              lubridate::wday(dateAsYYYYMMDD) == 7),
         TRUE,
         FALSE)}

isAllSaintsEve <- function(dateAsYYYYMMDD) {
  nextDay <- dateAsYYYYMMDD %>% as.Date + 1
  ifelse(isAllSaintsDay(nextDay %>% as.character),
         TRUE,
         FALSE)
}

isMidsummersDay <- function(dateAsYYYYMMDD) {
  ifelse(lubridate::month(dateAsYYYYMMDD) == 6 &
           lubridate::mday(dateAsYYYYMMDD) %in% 20:26 &
           lubridate::wday(dateAsYYYYMMDD) == 7,
         TRUE,
         FALSE)}

isMidsummersEve <- function(dateAsYYYYMMDD) {
  nextDay <- dateAsYYYYMMDD %>% as.Date + 1
  ifelse(isMidsummersDay(nextDay %>% as.character),
         TRUE,
         FALSE)
}

isEasterSunday <- function(dateAsYYYYMMDD) {
  ifelse(dateAsYYYYMMDD %in%
           c(
             "2009-04-12"
             , "2010-04-04"
             , "2011-04-24"
             , "2012-04-08"
             , "2013-03-31"
             , "2014-04-20"
             , "2015-04-05"
             , "2016-03-27"
             , "2017-04-16"
             , "2018-04-01"
             , "2019-04-21"
             , "2020-04-12"
             , "2021-04-04"
             , "2022-04-17"
             , "2023-04-09"
             , "2024-03-31"
             , "2025-04-20"
             , "2026-04-05"
             , "2027-03-28"
             , "2028-04-16"
             , "2029-04-01"
             , "2030-04-21"
             ),
         TRUE,
         FALSE)}

isEasterSundayEve <- function(dateAsYYYYMMDD) {
  nextDay <- dateAsYYYYMMDD %>% as.Date + 1

  ifelse(isEasterSunday(nextDay %>% as.character),
         TRUE,
         FALSE)
}

isEasterMonday <- function(dateAsYYYYMMDD) {
  previousDay <- dateAsYYYYMMDD %>% as.Date - 1

  ifelse(isEasterSunday(previousDay %>% as.character),
         TRUE,
         FALSE)
}

isGoodFriday <- function(dateAsYYYYMMDD) {
  twoDaysLater <- dateAsYYYYMMDD %>% as.Date + 2

  ifelse(isEasterSunday(twoDaysLater %>% as.character),
         TRUE,
         FALSE)
}

isAscension <- function(dateAsYYYYMMDD) {
  thirtynineDaysEarlier <- dateAsYYYYMMDD %>% as.Date - 39

  ifelse(isEasterSunday(thirtynineDaysEarlier %>% as.character),
         TRUE,
         FALSE)
}

isPentecost <- function(dateAsYYYYMMDD) {
  fortynineDaysEarlier <- dateAsYYYYMMDD %>% as.Date - 49

  ifelse(isEasterSunday(fortynineDaysEarlier %>% as.character),
         TRUE,
         FALSE)
}

isPentecostEve <- function(dateAsYYYYMMDD) {
  nextDay <- dateAsYYYYMMDD %>% as.Date + 1

  ifelse(isPentecost(nextDay %>% as.character),
         TRUE,
         FALSE)
}

isSwedishRedOrPinkDay <- function(dateAsYYYYMMDD) {
  ifelse(
    (isNewYearsDay(dateAsYYYYMMDD)) |
      (isTwelfthNight(dateAsYYYYMMDD)) |
      (isEpiphany(dateAsYYYYMMDD)) |
      (isGoodFriday(dateAsYYYYMMDD)) |
      (isEasterSundayEve(dateAsYYYYMMDD)) |
      (isEasterSunday(dateAsYYYYMMDD)) |
      (isEasterMonday(dateAsYYYYMMDD)) |
      (isWalpurgis(dateAsYYYYMMDD)) |
      (isMayday(dateAsYYYYMMDD)) |
      (isAscension(dateAsYYYYMMDD)) |
      (isPentecostEve(dateAsYYYYMMDD)) |
      (isPentecost(dateAsYYYYMMDD)) |
      (isMidsummersEve(dateAsYYYYMMDD)) |
      (isMidsummersDay(dateAsYYYYMMDD)) |
      (isNationalDay(dateAsYYYYMMDD)) |
      (isHalloween(dateAsYYYYMMDD)) |
      (isAllSaintsEve(dateAsYYYYMMDD)) |
      (isAllSaintsDay(dateAsYYYYMMDD)) |
      (isChristmasEve(dateAsYYYYMMDD)) |
      (isChristmasDay(dateAsYYYYMMDD)) |
      (isSecondDayOfChristmas(dateAsYYYYMMDD)) |
      (isNewYearsEve(dateAsYYYYMMDD)),
    TRUE,
    FALSE)
}

isSwedishPinchDay <- function(dateAsYYYYMMDD) {
  previousDay <- dateAsYYYYMMDD %>% as.Date - 1
  nextDay <- dateAsYYYYMMDD %>% as.Date + 1

  ifelse((isSwedishRedOrPinkDay(previousDay) &
            lubridate::wday(dateAsYYYYMMDD) == 6) | 
           (isSwedishRedOrPinkDay(nextDay) &
              lubridate::wday(dateAsYYYYMMDD) == 2),
         TRUE,
         FALSE)
}

isSwedishDayOff <- function(dateAsYYYYMMDD) {
  ifelse(
    (isSwedishRedOrPinkDay(dateAsYYYYMMDD)) |
      (isSwedishPinchDay(dateAsYYYYMMDD)),
    TRUE,
    FALSE)
}

# Note, this function is incredibly slow

addSwedishDaysOffData <- function(dataframe, columnWithDateAsYMD) {
  dataframe[["swedishRedOrPinkDay"]] <- NA

  for (d in seq_len(nrow(dataframe))) {
    cat("\nRow", d, "of", nrow(dataframe), "\n")
    dataframe[["swedishRedOrPinkDay"]][d] <-
      ifelse(
        isSwedishRedOrPinkDay(dataframe[[columnWithDateAsYMD]][d])
        , 1
        , 0
        )

    dataframe[["swedishPinchDay"]][d] <-
      ifelse(
        isSwedishPinchDay(dataframe[[columnWithDateAsYMD]][d])
        , 1
        , 0
        )

    dataframe[["swedishDayOff"]][d] <-
      ifelse(
        (
          dataframe[["swedishRedOrPinkDay"]][d] == 1) |
            (dataframe[["swedishPinchDay"]][d] == 1)
          , 1
          , 0
          )

  }

  dataframe
}