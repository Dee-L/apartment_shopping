# Purpose: Save hemnet object after scraping
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