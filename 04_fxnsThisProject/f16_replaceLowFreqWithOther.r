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

installMyPkgs(pkgs)

# 02 Define the function ####

replaceLowFreqWithOther <-
    function(dataframe, column, FrequencyCutoff = 50) {

    dataframeAsString <- deparse(substitute(dataframe))

    # 03 Find the low frequency hits ####
    lowFreqHits <-
        sqldf::sqldf(
            paste0(
                "select "
                , column
                , ", count(*) as count
                    from "
                , dataframeAsString
                , " where "
                , column
                , " != '<NA>'
                    group by "
                , column
                , " order by count desc"
                )
            ) %>%
        .[FrequencyCutoff : nrow(.), column] %>%
        .[complete.cases(.)]

    # 04 Make a new column name ####
    newColumnName <- paste0(column, "_low_freq_generalized")

    # 05 Copy the results into the new column ####
    dataframeModified <- dataframe

    dataframeModified[[eval(newColumnName)]] <-
        ifelse(dataframeModified[[column]] %in% lowFreqHits,
            "other",
            dataframeModified[[column]]
        )

    # 06 Return a modified version of the original dataframe ####
    dataframeModified
}