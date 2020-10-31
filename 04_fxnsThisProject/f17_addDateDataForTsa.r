# Purpose: Adds engineered date data for time-series analysis
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
        , "dplyr"
    )

installMyPkgs(pkgs)

# 02 Define the function ####
addDateDataForTsa <- function(dataframe, columnWithDateAsYMD) {

    oldCols <- names(dataframe)

    dataframe[["quarterOfYearSold"]] <-
        lubridate::quarter(dataframe[[columnWithDateAsYMD]])

    dataframe[["monthOfYearSold"]] <-
        lubridate::month(dataframe[[columnWithDateAsYMD]])

    dataframe[["monthOfQuarterSold"]] <-
        (floor((lubridate::month(
            dataframe[[columnWithDateAsYMD]]
            ) - 1) / 4) + 1)

    dataframe[["weekOfYearSold"]] <-
        lubridate::week(dataframe[[columnWithDateAsYMD]])

    dataframe[["weekOfQuarterSold"]] <-
        (floor((lubridate::week(
            dataframe[[columnWithDateAsYMD]]
            ) - 1) / 4) + 1)

    dataframe[["weekOfMonthSold"]] <-
        (floor((lubridate::mday(
            dataframe[[columnWithDateAsYMD]]
            ) - 1) / 7) + 1)

    dataframe[["dayOfYearSold"]] <-
        lubridate::yday(dataframe[[columnWithDateAsYMD]])

    dataframe[["dayOfQuarterSold"]] <-
        lubridate::qday(dataframe[[columnWithDateAsYMD]])

    dataframe[["dayOfMonthSold"]] <-
        lubridate::mday(dataframe[[columnWithDateAsYMD]])

    dataframe[["dayOfWeekSold"]] <-
        dplyr::case_when(
            lubridate::wday(dataframe[[columnWithDateAsYMD]]) == 1 ~ "sun",
            lubridate::wday(dataframe[[columnWithDateAsYMD]]) == 2 ~ "mon",
            lubridate::wday(dataframe[[columnWithDateAsYMD]]) == 3 ~ "tue",
            lubridate::wday(dataframe[[columnWithDateAsYMD]]) == 4 ~ "wed",
            lubridate::wday(dataframe[[columnWithDateAsYMD]]) == 5 ~ "thu",
            lubridate::wday(dataframe[[columnWithDateAsYMD]]) == 6 ~ "fri",
            lubridate::wday(dataframe[[columnWithDateAsYMD]]) == 7 ~ "sat",
        )

    newCols <-
        c(
            "quarterOfYearSold",
            "monthOfYearSold",
            "monthOfQuarterSold",
            "weekOfYearSold",
            "weekOfQuarterSold",
            "weekOfMonthSold",
            "dayOfYearSold",
            "dayOfQuarterSold",
            "dayOfMonthSold",
            "dayOfWeekSold"
            )

    newColOrder <- unique(c(oldCols, newCols), fromLast = T)

    dataframe %<>% .[, newColOrder]
}
