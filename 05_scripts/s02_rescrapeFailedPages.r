# Purpose: Re-scraping failedPages
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c()

installMyPkgs(pkgs)

# 02 Load the failed pages ####
failedPagesToRescrape <-
    outputFolderScrapedParent %>%
    list.dirs %>%
    .[length(.)] %>%
    paste0(., "/failedPages.rds") %>%
    readRDS

# 02 Set where data will be saved ####

outputFolderScraped <<-
    paste0(
        outputFolderScrapedParent,
        "failedPagesRescraped/"
    )

if (!dir.exists(outputFolderScraped)) {
    dir.create(outputFolderScraped)
}

# 03 Loop to scrape the pages ####
totalPages <<- nrow(failedPagesToRescrape)

for (page in seq_len(totalPages)) {
    cat(
        "\n\nRe-scraping page",
        page,
        "of",
        totalPages,
        "total pages.\n\n"
    )

    urlToScrape <<- failedPagesToRescrape[["url"]][page]
    htmlToRead <<- tryWaitRetry(getHtml(urlToScrape))

    # 07 Scrape and test all data from the url. Capture fails. ####

    tryScrapeSaveHemnetHtml()

}
