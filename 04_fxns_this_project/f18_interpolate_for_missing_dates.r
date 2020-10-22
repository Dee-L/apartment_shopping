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

install_my_pkgs(pkgs)

interpolate_for_missing_dates <-
    function(data_frame, column_name, date_column) {
        # Sorting the data_frame
        data_frame %<>% .[order(.[[date_column]], decreasing = T), ]

        # Getting column name to populate
        new_column_name <- paste0(column_name, "_interpolated")

        data_frame[[new_column_name]] <- data_frame[[column_name]]

        # Find min and max in range
        min_column <- min(data_frame[[column_name]], na.rm = T)
        max_column <- max(data_frame[[column_name]], na.rm = T)

        # Find min and max date
        first_date <- min(data_frame[[date_column]], na.rm = T)
        last_date <- min(data_frame[[date_column]], na.rm = T)

        # If first or last is NA, replace with min or max
        first_in_column <-
            data_frame[[column_name]][data_frame[[date_column]] == min_date]

        last_in_column <-
            data_frame[[column_name]][data_frame[[date_column]] == max_date]

        if (is.na(first_in_column)) {
            data_frame[[new_column_name]][
                data_frame[[date_column]] == min_date
            ] <- min_column
        }

        if (is.na(last_in_column)) {
            data_frame[[new_column_name]][
                data_frame[[date_column]] == max_date
            ] <- max_column
        }

        # Do linear interpolation for the rest of the NAs now that have endpoints
        data_frame[[new_column_name]] %<>%
            imputeTS::na_interpolation(option = "linear")

        data_frame
}
