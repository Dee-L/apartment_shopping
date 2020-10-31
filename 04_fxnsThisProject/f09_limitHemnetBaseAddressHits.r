# Purpose: Adjust Hemnet base address to limited hits
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-19
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "rvest"
    )

installMyPkgs(pkgs)

# 02 Define the function ####

limitHemnetBaseAddressHits <- function(minNPages = 1, maxNPages = 50) {

    repeat {
        updateHemnetBaseAddress()

        # 03 Create html session for navigating and scraping ####

        htmlToRead <<-
            hemnetBaseAddress %>%
            getHtml()

        # 04 Calculate the number of pages from the page landed on ####

        totalPages <<-
            htmlToRead %>%
            rvest::html_nodes("#result .clear-children .centered") %>%
            rvest::html_text() %>%
            gsub("[^[:alnum:] ]| ", "", .) %>%
            gsub("Visar150av", "", .) %>%
            as.numeric() %>%
            prod(., 1 / 50) %>%
            ceiling(.)

        # 05 Reset total pages to 1 if only 1 page in range ####

        if (is.na(totalPages)) {
            totalPages <<- 1
        }

        cat(
            "total pages are: ",
            totalPages,
            "\n\n"
        )

        # 06 Adjust price ranges so that have fewer than maxNPages ####

        if (totalPages < minNPages) {
            cat(
                "Increasing currentMaxPrice.\n\nMax price was: ",
                currentMaxPrice,
                "\n\n"
            )
            currentMaxPrice <<- currentMaxPrice + 1000000
            cat("Max price is now: ", currentMaxPrice, "\n\n")

        } else if (totalPages >= maxNPages) {
            cat(
                "Decreasing currentMaxPrice.\n\nMax price was: ",
                currentMaxPrice,
                "\n\n"
            )
            currentMaxPrice <<-
                median(c(currentMinPrice, currentMaxPrice)) %>%
                floor
            cat("Max price is now: ", currentMaxPrice, "\n\n")
        } else if (
            all(
                minNPages < totalPages
                , totalPages < maxNPages
            )
        ) {
            break
        }
    }

}
