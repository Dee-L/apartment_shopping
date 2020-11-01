# Purpose: Hemnet specific functions
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "rvest"
    )

activatePkgs(pkgs)

updateHemnetBaseAddress <- function() {
    hemnetBaseAddress <<-
        paste0(
            "https://www.hemnet.se/salda/bostader?",
            "location_ids%5B%5D=",
            hemnetAreaNumber,
            "&item_types%5B%5D=",
            housingType,
            "&rooms_min=",
            minRooms,
            "&fee_max=",
            maxAvgift,
            "&selling_price_min=",
            currentMinPrice,
            "&selling_price_max=",
            currentMaxPrice,
            "&sold_age=",
            daysToLookBack,
            "d",
            "&by=",
            sortingFeature,
            "&order=",
            sortingDirection,
            "&page=",
            hemnetBasePageNumber
        )
}


# 02 Gather results ####

saveHemnetObject <- function() {
    soldObject <- data.frame(
        sellingPrice = naIfEmpty(sellingPrice)
        , askingPrice = naIfEmpty(askingPrice)
        , rooms = naIfEmpty(rooms)
        , kvm = naIfEmpty(kvm)
        , floor = naIfEmpty(floor)
        , avgift = naIfEmpty(avgift)
        , runningCosts = naIfEmpty(runningCosts)
        , city = naIfEmpty(city)
        , area = naIfEmpty(area)
        , street = naIfEmpty(street)
        , dayOfMonthSold = naIfEmpty(dayOfMonthSold)
        , monthSoldSwedish = naIfEmpty(monthSoldSwedish)
        , yearSold = naIfEmpty(yearSold)
        , yearBuilt = naIfEmpty(yearBuilt)
        , agentName = naIfEmpty(agentName)
        , agency = naIfEmpty(agency)
        , url = naIfEmpty(urlToScrape)
    )

    # 03 Create an object name ####

    objectName <-
        paste0(
            "o"
            , yearSold
            , left(monthSoldSwedish, 3)
            , dayOfMonthSold
            , "_"
            , city
            , "_"
            , street
            , "_"
            , streetNumber
            , ".rda"
        )

    assign(objectName, soldObject)

    # 04 Save the object ####

    save(
        list = objectName
        , file = paste0(outputFolderScraped, objectName)
        )

    if (objectName %notIn% list.files(outputFolderScraped)) {
        message(
            paste0(
                objectName
                , " not found in "
                , outputFolderScraped
                , ". Some kind of untracked failure with "
                , urlToScrape))
    }

}

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
                floor()
            cat("Max price is now: ", currentMaxPrice, "\n\n")
        } else if (
            all(
                minNPages < totalPages,
                totalPages < maxNPages
            )
        ) {
            break
        }
    }
}

# 02 Start function definition ####

scrapeManyFromHemnetBase <- function() {

    # 03 Set the range for how many pages the base page should point to ####

    limitHemnetBaseAddressHits(minNPages = 1, maxNPages = 50)

    # 04 Set where data will be saved ####

    outputFolderScraped <<-
        paste0(
            outputFolderScrapedParent,
            "price_",
            currentMinPrice,
            "_to_",
            currentMaxPrice,
            "/"
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
                    "\n\n",
                    pageToScrape,
                    " of ",
                    length(pagesToScrape),
                    " pages on this base page."
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
                        url = urlToScrape,
                        errorMessage = errorMessage
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
