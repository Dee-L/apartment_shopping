# Purpose: Replace low frequency with "other"
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c("sqldf")

install_my_pkgs(pkgs)

# 02 Define the function ####

replace_low_freq_with_other <-
    function(data_frame, column, frequency_cutoff = 50) {

    data_frame_as_string <- deparse(substitute(data_frame))

    # 03 Find the low frequency hits ####
    low_freq_hits <-
        sqldf::sqldf(
            paste0(
                "select "
                , column
                , ", count(*) as count
                    from "
                , data_frame_as_string
                , " where "
                , column
                , " != '<NA>'
                    group by "
                , column
                , " order by count desc"
                )
            ) %>%
        .[frequency_cutoff : nrow(.), column] %>%
        .[complete.cases(.)]

    # 04 Make a new column name ####    
    new_column_name <- paste0(column, "_low_freq_generalized")

    # 05 Copy the results into the new column ####
    data_frame_modified <- data_frame
    
    data_frame_modified[[eval(new_column_name)]] <-
        ifelse(data_frame_modified[[column]] %in% low_freq_hits,
            "other",
            data_frame_modified[[column]]
        )

    # 06 Return a modified version of the original data_frame ####
    data_frame_modified
}