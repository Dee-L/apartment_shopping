# Purpose: Compiling scraped Hemnet data into a df
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-18
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:


# This code works for a single output folder, but will need to
# be modified to loop through all output files
# May be able to just add recursive argument to list.files

for (file in
    list.files("07_outputs//01_scraped_new/price_1000000_to_2187000/")) {

        temp_df <<- reassign_rda(
            paste0(
                "07_outputs//01_scraped_new/price_1000000_to_2187000/"
                , file
                )
            )

        if (!exists("results_df")) {
           results_df <<- temp_df
        } else {
            results_df <<- rbind(results_df, temp_df)
        }
    }
