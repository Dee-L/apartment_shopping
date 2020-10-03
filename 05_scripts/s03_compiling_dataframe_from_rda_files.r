# Purpose: Compiling scraped Hemnet data into a df
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-18
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:


# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c()

install_my_pkgs(pkgs)

# 02 Check if need to compile new data ####

latest_scraped_results <-
    output_folder_scraped_gparent %>%
    list.dirs(recursive = F) %>%
    .[length(.)] %>%
    right(8) %>%
    as.numeric
    
latest_compiled_results <-
    output_folder_compiled %>%
    list.files %>%
    .[length(.)] %>%
    gsub(".rds", "", .) %>%
    right(8) %>%
    as.numeric

if (length(latest_compiled_results) == 0) {
    latest_compiled_results <- latest_scraped_results - 1
}

# 03 Only proceed if compiled results are older than scraped results ####
if (latest_compiled_results < latest_scraped_results) {

    # 04 Subset the scraped folders to those that contain new data ####
    scraped_folders <-
        list.dirs(output_folder_scraped_gparent, recursive = F)

    dates_of_scraped_folders <-
        scraped_folders %>%
            right(8) %>%
            as.numeric

    relevant_scraped_folders <-
        scraped_folders[dates_of_scraped_folders > latest_compiled_results]

    # 05 compile the data ####
    for (file in list.files(relevant_scraped_folders, recursive = T)) {

        # 06 create path for file ####
        this_path <-
            paste0(relevant_scraped_folders, "/", file)

        # 07 ignore the failed_pages files ####
        if (!(grepl("failed_pages.rds", this_path))) {

            cat("Adding: ", file, "\n\n")

            # 08 save into a temp_df the file ####
            temp_df <<- reassign_rda(this_path)

            # 09 create or append to results_df ####
            if (!exists("results_df")) {
                results_df <<- temp_df
            } else {
                results_df <<- rbind(results_df, temp_df)
            }
        }
    }

    # 10 save the object ####
    name_of_results_df <-
        paste0(
            "date_"
            , today_8digit()
        )

    saveRDS(
        results_df
        , paste0(output_folder_compiled, eval(name_of_results_df))
        )
}
