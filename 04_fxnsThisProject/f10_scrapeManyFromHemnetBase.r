# Purpose: Scrape 35 to 50 pages from a Hemnet base address
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
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

# 02 Start function definition ####

scrapeManyFromHemnetBase <- function() {

    # 03 Set the range for how many pages the base page should point to ####

    limitHemnetBaseAddressHits(minNPages = 1, maxNPages = 50)

    # 04 Set where data will be saved ####

    outputFolderScraped <<-
        paste0(
            outputFolderScrapedParent
            , "price_"
            , currentMinPrice
            , "_to_"
            , currentMaxPrice
            , "/"
        )

    makeDirIfDoesNotExist(outputFolderScraped)


    # 05 Loop to scrape the pages ####

    for (page in seq_len(totalPages)) {

        cat(
            "\n\nHemnet base page updating to",
            page,
            "of",
            totalPages,
            "total pages.\n\n"
        )

        hemnetBasePageNumber <<- page

        # update base_address and session

        updateHemnetBaseAddress()

        htmlToRead <<-
            hemnetBaseAddress %>%
            getHtml()

        # count the number of links to scrape from

        pagesToScrape <<-
            htmlToRead %>%
            rvest::html_nodes(".item-link-container") %>%
            rvest::html_attr("href")

        cat(length(pagesToScrape), "pages to scrape from this base page.")

        # 06 Inner loop ####

        for (pageToScrape in seq_along(pagesToScrape)) {

            cat(
                paste0(
                    "\n\n"
                    , pageToScrape
                    , " of "
                    , length(pagesToScrape)
                    , " pages on this base page."
                    )
                )

            # update url

            urlToScrape <<- pagesToScrape[pageToScrape]

            htmlToRead <<- tryWaitRetry(getHtml(urlToScrape))

            # 07 Scrape and test all data from the url. Capture fails. ####

            tryScrapeSaveHemnetHtml()
        }
    }
}
