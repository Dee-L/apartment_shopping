# Purpose: Attempts to get my variables of interest from a Hemnet page
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c()

install_my_pkgs(pkgs)

try_scrape_save_hemnet_html <- function() {

    # 02 Tries the expression. If fails, runs the error block. ####

    tryCatch(
        expr = {
            get_all_variables(html_to_read)
            test_all()
            save_hemnet_object()
        },

        # 03 The error block that runs if errors crop up in the expression ####

        error = function(error_message) {
            message("Error! Saving to failed_pages df.")
            error_message %<>% as.character()

            # 04 Creates "failed pages" df to capture error message ####

            if (exists("failed_pages")) {
                assign(
                    "failed_pages",
                    rbind(
                        failed_pages,
                        data.frame(
                            url = url_to_scrape,
                            error_message = error_message
                        )
                    ),
                    envir = .GlobalEnv
                )
            } else {
                assign(
                    "failed_pages",
                    data.frame(
                        url = url_to_scrape
                        , error_message = error_message
                    ),
                    envir = .GlobalEnv
                )

            }

            # 05 Saves the "failed pages" df as an rda object ####

            save_as_r_object_with_its_name(
                failed_pages,
                output_folder_scraped
            )
        }
    )

}
