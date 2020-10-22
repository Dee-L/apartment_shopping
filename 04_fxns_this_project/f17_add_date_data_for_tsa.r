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

install_my_pkgs(pkgs)

# 02 Define the function ####
add_date_data_for_tsa <- function(data_frame, column_with_date_as_ymd) {

    old_cols <- names(data_frame)

    data_frame[["quarterofyear_sold"]] <-
        lubridate::quarter(data_frame[[column_with_date_as_ymd]])

    data_frame[["monthofyear_sold"]] <-
        lubridate::month(data_frame[[column_with_date_as_ymd]])

    data_frame[["monthofquarter_sold"]] <-
        (floor((lubridate::month(
            data_frame[[column_with_date_as_ymd]]
            ) - 1) / 4) + 1)

    data_frame[["weekofyear_sold"]] <-
        lubridate::week(data_frame[[column_with_date_as_ymd]])

    data_frame[["weekofquarter_sold"]] <-
        (floor((lubridate::week(
            data_frame[[column_with_date_as_ymd]]
            ) - 1) / 4) + 1)

    data_frame[["weekofmonth_sold"]] <-
        (floor((lubridate::mday(
            data_frame[[column_with_date_as_ymd]]
            ) - 1) / 7) + 1)

    data_frame[["dayofyear_sold"]] <-
        lubridate::yday(data_frame[[column_with_date_as_ymd]])

    data_frame[["dayofquarter_sold"]] <-
        lubridate::qday(data_frame[[column_with_date_as_ymd]])

    data_frame[["dayofmonth_sold"]] <-
        lubridate::mday(data_frame[[column_with_date_as_ymd]])

    data_frame[["dayofweek_sold"]] <-
        dplyr::case_when(
            lubridate::wday(data_frame[[column_with_date_as_ymd]]) == 1 ~ "sun",
            lubridate::wday(data_frame[[column_with_date_as_ymd]]) == 2 ~ "mon",
            lubridate::wday(data_frame[[column_with_date_as_ymd]]) == 3 ~ "tue",
            lubridate::wday(data_frame[[column_with_date_as_ymd]]) == 4 ~ "wed",
            lubridate::wday(data_frame[[column_with_date_as_ymd]]) == 5 ~ "thu",
            lubridate::wday(data_frame[[column_with_date_as_ymd]]) == 6 ~ "fri",
            lubridate::wday(data_frame[[column_with_date_as_ymd]]) == 7 ~ "sat",
        )

    new_cols <-
        c(
            "quarterofyear_sold",
            "monthofyear_sold",
            "monthofquarter_sold",
            "weekofyear_sold",
            "weekofquarter_sold",
            "weekofmonth_sold",
            "dayofyear_sold",
            "dayofquarter_sold",
            "dayofmonth_sold",
            "dayofweek_sold"
            )

    new_col_order <- unique(c(old_cols, new_cols), fromLast = T)

    data_frame %<>% .[, new_col_order]
}
