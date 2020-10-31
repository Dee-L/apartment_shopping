# Purpose: Update Hemnet base address
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-18
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c()

installMyPkgs(pkgs)

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
