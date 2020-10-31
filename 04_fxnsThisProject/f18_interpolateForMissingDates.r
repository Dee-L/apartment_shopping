# Purpose: Interpolates linearly for dates that have NA
# Author: David Gray Lassiter, PhD
# Date: 2020-Oct-01
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "imputeTS"
        , "sqldf"
    )

installMyPkgs(pkgs)

interpolateForMissingDates <-
    function(dataframe, columnName, dateColumn) {
        # Sorting the dataframe
        dataframe %<>% .[order(.[[dateColumn]], decreasing = T), ]

        # Getting column name to populate
        newColumnName <- paste0(columnName, "Interpolated")

        dataframe[[newColumnName]] <- dataframe[[columnName]]

        # Find min and max in range
        minColumn <- min(dataframe[[columnName]], na.rm = T)
        maxColumn <- max(dataframe[[columnName]], na.rm = T)

        # Find min and max date
        firstDate <- min(dataframe[[dateColumn]], na.rm = T)
        lastDate <- min(dataframe[[dateColumn]], na.rm = T)

        # If first or last is NA, replace with min or max
        firstInColumn <-
            dataframe[[columnName]][dataframe[[dateColumn]] == minDate]

        lastInColumn <-
            dataframe[[columnName]][dataframe[[dateColumn]] == maxDate]

        if (is.na(firstInColumn)) {
            dataframe[[newColumnName]][
                dataframe[[dateColumn]] == minDate
            ] <- minColumn
        }

        if (is.na(lastInColumn)) {
            dataframe[[newColumnName]][
                dataframe[[dateColumn]] == maxDate
            ] <- maxColumn
        }

        # Do linear interpolation for the rest of the NAs now that have endpoints
        dataframe[[newColumnName]] %<>%
            imputeTS::na_interpolation(option = "linear")

        dataframe
}
