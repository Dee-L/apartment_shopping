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

installMyPkgs(pkgs)

tryScrapeSaveHemnetHtml <- function() {

    # 02 Tries the expression. If fails, runs the error block. ####

    tryCatch(
        expr = {
            getAllVariables(htmlToRead)
            testAll()
            saveHemnetObject()
        },

        # 03 The error block that runs if errors crop up in the expression ####

        error = function(errorMessage) {
            message("Error! Saving to failedPages df.")
            errorMessage %<>% as.character()

            # 04 Creates "failed pages" df to capture error message ####

            if (exists("failedPages")) {
                assign(
                    "failedPages",
                    rbind(
                        failedPages,
                        data.frame(
                            url = urlToScrape,
                            errorMessage = errorMessage
                        )
                    ),
                    envir = .GlobalEnv
                )
            } else {
                assign(
                    "failedPages",
                    data.frame(
                        url = urlToScrape
                        , errorMessage = errorMessage
                    ),
                    envir = .GlobalEnv
                )

            }

            # 05 Saves the "failed pages" df as an rda object ####

            saveAsRObjectWithItsName(
                failedPages,
                outputFolderScraped
            )
        }
    )

}
