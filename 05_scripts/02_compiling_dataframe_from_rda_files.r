# Purpose: Compiling scraped Hemnet data into a df
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-18
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:


for (file in list.files("07_outputs//01_scraped/", recursive = T)) {

    # create path for file
    this_path <-
        paste0("07_outputs//01_scraped/", file)

    # ignore the failed_pages file
    if (!(grepl("failed_pages.rds", this_path))) {

        # save into a temp_df the file
        temp_df <<- reassign_rda(this_path)

        # create or append to results_df
        if (!exists("results_df")) {
            results_df <<- temp_df
        } else {
            results_df <<- rbind(results_df, temp_df)
        }

    }
}