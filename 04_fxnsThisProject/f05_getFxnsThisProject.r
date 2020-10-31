# Purpose: Get functions for this project
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-13
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "rvest"
        , "xml2"
    )

installMyPkgs(pkgs)

# 02 Get html ####

getHtml <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html()
}

# 03 Getting data from Hemnet html ####

getAddress <- function(html) {
    html %>%
        rvest::html_nodes(".sold-property__address") %>%
        rvest::html_text() %>%
        gsub("[^-[:alnum:]\\+/&\\., ]", "", .) %>%
        gsub("  Slutpris  ", "", .) %>%
        tolower
}

getStreetNumber <- function(html) {
    address <- getAddress(html)

    streetNumber <-
        regmatches(address, gregexpr("[[:digit:]]+", address))[[1]][1] %>%
            naIfEmpty %>%
            as.numeric

    if (
        any(
            is.na(streetNumber)
            , streetNumber < 0
            )
     ) {
        streetNumber <- NA
    }

    streetNumber
}

getStreet <- function(html) {
    address <- getAddress(html)
    streetNumber <- getStreetNumber(html)

    substr(
        address
        , 1
        , gregexpr(as.character(streetNumber), address)[[1]][1] - 2) %>%
        gsub(" ", "", .) %>%
        naIfEmpty
}

getFloor <- function(html) {
    address <- getAddress(html)

    streetNumber <- getStreetNumber(html)
    # Cannot use normal getStreet because it removes spaces which
    # would throw off the character count when extracting the floor info
    street <- substr(
        address
        , 1
        , gregexpr(as.character(streetNumber), address)[[1]][1] - 2) %>%
        naIfEmpty

    floorInfo <-
        substr(
            address
            , nchar(street) + nchar(streetNumber) + 3
            , nchar(address)
            )

    # Remove "strand" from floor info if it exits
    floorInfo %<>% gsub("strand", "", .)

    # Replace "och" with dash to get in-between floors
    floorInfo %<>%
        gsub("&", "och", .) %>%
        gsub(" och ", "och", .) %>%
        gsub("och", "-", .)

    floorInfo %<>%
        gsub("/", "av", .)

    if (grepl("av", floorInfo)) {
      floorInfo %<>%
        substr(., 1, gregexpr("av", .)[[1]][1] - 1)
    }

    floor <-
        # See if text indicates it's a floor number
        if (
            any(
                (grepl("v.n", floorInfo)),
                (grepl("tr", floorInfo))
                )
            ) {
                # Make it 0 floor if bv
                if (grepl("bv", floorInfo)) {
                    as.numeric(0)
                } else {

                    # If comma before a digit, change to point
                    if (grepl(",\\d", floorInfo)) {
                        floorInfo %<>%
                            gsub(",", ".", .)
                    }

                    # Also remove any text after "lg"
                    floorInfo %<>%
                        gsub("lg.{1,}", "", .)

                    # If no dash, plus, nor slash, keep digits
                    if (!grepl("\\+|-|/", floorInfo)) {
                        floorInfo %>% gsub("[^0-9.-]", "", .) %>% as.numeric
                    # If dash, plus, or slash, convert to half floor
                    } else {
                        regmatches(
                            floorInfo,
                            gregexpr(
                                "[[:digit:]]+",
                                floorInfo)
                            ) %>%
                            .[[1]] %>%
                            .[length(.) - 1] %>%
                            as.numeric %>%
                            sum(0.5)
                    }
                }
            }

    floor %>%
        naIfEmpty

}

getTypeAreaDate <- function(html) {
    html %>%
        rvest::html_nodes(".sold-property__metadata") %>%
        rvest::html_text() %>%
        gsub("\\s+", " ", .) %>%
        strsplit(., "-") %>%
        .[[1]] %>%
        sapply(
            .,
            function(x) strsplit(as.character(x), ",")[[1]]) %>%
        unlist() %>%
        trimws() %>%
        tolower() %>%
        paste0()
}

getType <- function(html) {
    typeAreaDate <- getTypeAreaDate(html)

    typeAreaDate[[1]] %>%
        naIfEmpty
}

getArea <- function(html) {
    typeAreaDate <- getTypeAreaDate(html)

    indexOfCity <-
        typeAreaDate %>%
        grep("kommun", .)

    # Drop first and last index since belong to type and city
    indicesOfArea <-
        seq_len(indexOfCity - 1)[-1]

    paste0(typeAreaDate[indicesOfArea], collapse = "") %>%
        naIfEmpty %>%
        as.character

}

getCity <- function(html) {
    typeAreaDate <- getTypeAreaDate(html)

    indexOfCity <-
        typeAreaDate %>%
            grep("kommun", .)

    city <-
        typeAreaDate %>%
            .[[indexOfCity]] %>%
            gsub(" ", "", .) %>%
            gsub("kommun", "", .) %>%
            naIfEmpty

    citiesSwedish <-
        c(
        "stockholm", "solna", "uppsala"
        )

    if (
        any(
            grep(paste(citiesSwedish, collapse = "|"), city) < 1
            )
     ) {
        city <- NA
    }

    city %>%
        as.character
}

getDateSold <- function(html) {
    typeAreaDate <- getTypeAreaDate(html)

    indexOfDate <-
        typeAreaDate %>%
            grep(" den", .)

    typeAreaDate[[indexOfDate]] %>%
        strsplit(., " ") %>%
        .[[1]] %>%
        tolower
}

getDayOfMonthSold <- function(html) {
    dateSold <- getDateSold(html)

    dayOfMonthSold <-
        dateSold[3] %>%
            naIfEmpty %>%
            as.numeric

    if (
        any(
            dayOfMonthSold < 1
            , dayOfMonthSold > 31
            )
     ) {
        dayOfMonthSold <- NA
    }

    dayOfMonthSold
}

getMonthSoldSwedish <- function(html) {
    dateSold <- getDateSold(html)

    monthSoldSwedish <-
        dateSold[4] %>%
            naIfEmpty

    monthsSwedish <-
        c(
        "jan", "feb", "mar", "apr", "maj", "jun",
        "jul", "aug", "sep", "okt", "nov", "dec"
        )

    if (
        any(
            grep(paste(monthsSwedish, collapse = "|"), monthSoldSwedish) < 1
            )
     ) {
        monthSoldSwedish <- NA
    }

    monthSoldSwedish %>%
        as.character
}

getYearSold <- function(html) {
    dateSold <- getDateSold(html)

    yearSold <-
        dateSold[5] %>%
            naIfEmpty %>%
            as.numeric

    yearSold

}

getSellingPrice <- function(html) {
    sellingPrice <-
        html %>%
            rvest::html_nodes(".sold-property__price-value") %>%
            rvest::html_text() %>%
            gsub("[^[:alnum:] ]| -|,", "", .) %>%
            gsub("\\s+", " ", .) %>%
            strsplit(., " ") %>%
            .[[1]] %>%
            .[1] %>%
            naIfEmpty %>%
            as.numeric

    if (
        any(
            sellingPrice < firstMinPrice
            , sellingPrice > finalMaxPrice
            )
     ) {
        sellingPrice <- NA
    }

    sellingPrice

}

getAskingPrice <- function(html) {
    priceStatsVector <-
        html %>%
            rvest::html_nodes(".sold-property__price-stats") %>%
            rvest::html_text() %>%
            gsub("[^[:alnum:] ]| -|,", "", .) %>%
            gsub("\\s+", " ", .) %>%
            strsplit(., " ") %>%
            .[[1]]

    askingPrice <-
        priceStatsVector %>%
            .[which(. == "pris") + 1] %>%
            naIfEmpty %>%
            as.numeric

    askingPrice

}

getPropertyAttributes <- function(html) {
    html %>%
        rvest::html_nodes(".sold-property__attributes") %>%
        rvest::html_text() %>%
        gsub(",", ".", .) %>%
        strsplit(., "\\s+\\s+") %>%
        .[[1]]
}

getRooms <- function(html) {
    propertyAttributes <- getPropertyAttributes(html)

    rooms <-
        propertyAttributes %>%
            .[which(. == "Antal rum") + 1] %>%
            gsub(" rum", "", .) %>%
            naIfEmpty %>%
            as.numeric

    rooms
}

getKvm <- function(html) {
    propertyAttributes <- getPropertyAttributes(html)

    kvm <-
        propertyAttributes %>%
            .[which(. == "Boarea") + 1] %>%
            gsub(" m.", "", .) %>%
            naIfEmpty %>%
            as.numeric

    kvm

}

getAvgift <- function(html) {
    propertyAttributes <- getPropertyAttributes(html)

    avgift <-
        propertyAttributes %>%
            # .[which(. == "Avgift/månad") + 1] %>%
            .[grep("Avgift/m.nad", .) + 1] %>%
            # gsub(" kr/mån", "", .) %>%
            gsub(" kr/m.n", "", .) %>%
            gsub("\\s", "", .) %>%
            naIfEmpty %>%
            as.numeric

    avgift

}

getRunningCosts <- function(html) {
    propertyAttributes <- getPropertyAttributes(html)

    runningCosts <-
        propertyAttributes %>%
            .[which(. == "Driftskostnad") + 1] %>%
            # gsub(" kr/år", "", .) %>%
            gsub(" kr/.r", "", .) %>%
            gsub("\\s", "", .) %>%
            naIfEmpty %>%
            as.numeric

    runningCosts

}

getYearBuilt <- function(html) {
    propertyAttributes <- getPropertyAttributes(html)

    yearBuilt <-
        propertyAttributes %>%
            # .[which(. == "Byggår") + 1] %>%
            .[grep("Bygg.r", .) + 1] %>%
            naIfEmpty %>%
            as.numeric

    yearBuilt
}

getAgentName <- function(html) {
    html %>%
        rvest::html_nodes("strong") %>%
        rvest::html_text() %>%
        gsub("[^[:alnum:] ]| -|,", "", .) %>%
        gsub("\\s+", " ", .) %>%
        tolower %>%
        gsub(" ", "", .) %>%
        naIfEmpty
}

getAgency <- function(html) {
    html %>%
        rvest::html_nodes(".qa-broker-name+ .broker-card__text") %>%
        rvest::html_text() %>%
        gsub("[^[:alnum:] ]| -|,", "", .) %>%
        gsub("\\s+", " ", .) %>%
        tolower %>%
        gsub(" ", "", .) %>%
        naIfEmpty
}

# 04 Get all required scraped data ####

getAllVariables <- function(html) {

    assign(
        "streetNumber"
        , getStreetNumber(html)
        , envir = .GlobalEnv)
    assign(
        "street"
        , getStreet(html)
        , envir = .GlobalEnv)
    assign(
        "floor"
        , getFloor(html)
        , envir = .GlobalEnv)
    assign(
        "type"
        , getType(html)
        , envir = .GlobalEnv)
    assign(
        "area"
        , getArea(html)
        , envir = .GlobalEnv)
    assign(
        "city"
        , getCity(html)
        , envir = .GlobalEnv)
    assign(
        "dayOfMonthSold"
        , getDayOfMonthSold(html)
        , envir = .GlobalEnv)
    assign(
        "monthSoldSwedish"
        , getMonthSoldSwedish(html)
        , envir = .GlobalEnv)
    assign(
        "yearSold"
        , getYearSold(html)
        , envir = .GlobalEnv)
    assign(
        "sellingPrice"
        , getSellingPrice(html)
        , envir = .GlobalEnv)
    assign(
        "askingPrice"
        , getAskingPrice(html)
        , envir = .GlobalEnv)
    assign(
        "rooms"
        , getRooms(html)
        , envir = .GlobalEnv)
    assign(
        "kvm"
        , getKvm(html)
        , envir = .GlobalEnv)
    assign(
        "avgift"
        , getAvgift(html)
        , envir = .GlobalEnv)
    assign(
        "runningCosts"
        , getRunningCosts(html)
        , envir = .GlobalEnv)
    assign(
        "yearBuilt"
        , getYearBuilt(html)
        , envir = .GlobalEnv)

    assign(
        "agentName"
        , getAgentName(html)
        , envir = .GlobalEnv)

    assign(
        "agency"
        , getAgency(html)
        , envir = .GlobalEnv)
}
