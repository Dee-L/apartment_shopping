# Purpose: Get functions for this project
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-13
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this scripts are installed ####
pkgs <-
    c(
        "rvest"
        , "xml2"
    )

install_my_pkgs(pkgs)

# 02 Get html ####

get_html <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html()
}

# 03 Getting data from Hemnet html ####

#### These will all run faster if they run from a session instead
# of a html and I will hit Hemnet less frequently

get_address <- function(html) {
    html %>%
        rvest::html_nodes(".sold-property__address") %>%
        rvest::html_text() %>%
        gsub("/", " av ", .) %>%
        gsub("[^[:alnum:] ]", "", .) %>%
        gsub("  Slutpris  ", "", .) %>%
        tolower
}

get_street_number <- function(html) {
    address <- get_address(html)

    street_number <-
        regmatches(address, gregexpr("[[:digit:]]+", address))[[1]][1] %>%
            na_if_empty %>%
            as.numeric

    if (
        any(
            street_number < 0
            )
     ) {
        street_number <- NA
    }

    street_number
}

get_street <- function(html) {
    address <- get_address(html)
    street_number <- get_street_number(html)

    substr(
        address
        , 1
        , gregexpr(as.character(street_number), address)[[1]][1] - 2) %>%
        gsub(" ", "", .) %>%
        na_if_empty
}

# This function is not working for the following examples:
# 35 floor, 34 floor, 25 floor, 23 floor, 15 floor, 12 floor, 5 floor (instead of 0.5)?
# https://www.hemnet.se/salda/lagenhet-3,5rum-kungsholmen-fredhall-stockholms-kommun-adlerbethsgatan-17,-3-4-tr-72289
# https://www.hemnet.se/salda/lagenhet-3,5rum-vasastan-stockholms-kommun-vegagatan-6,-3-4tr-974312
# https://www.hemnet.se/salda/lagenhet-4rum-kungsholmen-stockholms-kommun-scheelegatan-26,-3,5tr-891071
# https://www.hemnet.se/salda/lagenhet-5,5rum-odenplan-stockholms-kommun-vastmannagatan-48,-1tr-lght-17-471617
# https://www.hemnet.se/salda/lagenhet-3rum-vasastan-stockholms-kommun-upplandsgatan-72,-1,5-tr-1199784
# After fixing, should make a list of URLS that previously scraped wrong, then re-scrape them

get_floor_in_building <- function(html) {
    address <- get_address(html)
    street_number <- get_street_number(html)
    street <- get_street(html)

    floor_info <-
        substr(
            address
            , nchar(street) + nchar(street_number) + 3
            , nchar(address)
            )

    if (grepl("av", floor_info)) {
      floor_info %<>%
        substr(., 1, gregexpr("av", .)[[1]][1] - 1)
    }

    floor_in_building <-
        if (
            any(
                (grepl("v.n", floor_info)),
                (grepl("tr", floor_info))
                )
            ) {
            regmatches(
                floor_info,
                gregexpr(
                    "[[:digit:]]+",
                    floor_info)
                    ) %>%
                .[[1]] %>%
                .[length(.)] %>%
                as.numeric
            } else if (
                grepl("bv", floor_info)
            ) {
                as.numeric(0)
            }

    if (
        any(
            floor_in_building < 0
            , floor_in_building > 40
            )
     ) {
        floor_in_building <- NA
    }

    floor_in_building %>%
        na_if_empty

}

get_type_area_date <- function(html) {
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

get_type <- function(html) {
    type_area_date <- get_type_area_date(html)

    type_area_date[[1]] %>%
        na_if_empty
}

get_area <- function(html) {
    type_area_date <- get_type_area_date(html)

    index_of_city <-
        type_area_date %>%
        grep("kommun", .)

    # Drop first and last index since belong to type and city
    indices_of_area <-
        seq_len(index_of_city - 1)[-1]

    paste0(type_area_date[indices_of_area], collapse = "") %>%
        na_if_empty %>%
        as.character

}

get_city <- function(html) {
    type_area_date <- get_type_area_date(html)

    index_of_city <-
        type_area_date %>%
            grep("kommun", .)

    city <-
        type_area_date %>%
            .[[index_of_city]] %>%
            gsub(" ", "", .) %>%
            gsub("kommun", "", .) %>%
            na_if_empty

    cities_swedish <-
        c(
        "stockholm", "solna", "uppsala"
        )

    if (
        any(
            grep(paste(cities_swedish, collapse = "|"), city) < 1
            )
     ) {
        city <- NA
    }

    city %>%
        as.character
}

get_date_sold <- function(html) {
    type_area_date <- get_type_area_date(html)

    index_of_date <-
        type_area_date %>%
            grep(" den", .)

    type_area_date[[index_of_date]] %>%
        strsplit(., " ") %>%
        .[[1]] %>%
        tolower
}

get_day_of_month_sold <- function(html) {
    date_sold <- get_date_sold(html)

    day_of_month_sold <-
        date_sold[3] %>%
            na_if_empty %>%
            as.numeric

    if (
        any(
            day_of_month_sold < 1
            , day_of_month_sold > 31
            )
     ) {
        day_of_month_sold <- NA
    }

    day_of_month_sold
}

get_month_sold_swedish <- function(html) {
    date_sold <- get_date_sold(html)

    month_sold_swedish <-
        date_sold[4] %>%
            na_if_empty

    months_swedish <-
        c(
        "jan", "feb", "mar", "apr", "maj", "jun",
        "jul", "aug", "sep", "okt", "nov", "dec"
        )

    if (
        any(
            grep(paste(months_swedish, collapse = "|"), month_sold_swedish) < 1
            )
     ) {
        month_sold_swedish <- NA
    }

    month_sold_swedish %>%
        as.character
}

get_year_sold <- function(html) {
    date_sold <- get_date_sold(html)

    year_sold <-
        date_sold[5] %>%
            na_if_empty %>%
            as.numeric

    if (
        any(
            year_sold < 2010
            , year_sold > 2020
            )
     ) {
        year_sold <- NA
    }

    year_sold

}

get_final_price <- function(html) {
    final_price <-
        html %>%
            rvest::html_nodes(".sold-property__price-value") %>%
            rvest::html_text() %>%
            gsub("[^[:alnum:] ]| -|,", "", .) %>%
            gsub("\\s+", " ", .) %>%
            strsplit(., " ") %>%
            .[[1]] %>%
            .[1] %>%
            na_if_empty %>%
            as.numeric

    if (
        any(
            final_price < first_min_price
            , final_price > final_max_price
            )
     ) {
        final_price <- NA
    }

    final_price

}

get_asking_price <- function(html) {
    price_stats_vector <-
        html %>%
            rvest::html_nodes(".sold-property__price-stats") %>%
            rvest::html_text() %>%
            gsub("[^[:alnum:] ]| -|,", "", .) %>%
            gsub("\\s+", " ", .) %>%
            strsplit(., " ") %>%
            .[[1]]

    asking_price <-
        price_stats_vector %>%
            .[which(. == "pris") + 1] %>%
            na_if_empty %>%
            as.numeric

    if (!is.na(asking_price)) {
        if (
            any(
                asking_price < 500000,
                asking_price > 12000000
            )
        ) {
            asking_price <- NA
        }
    }

    asking_price

}

get_property_attributes <- function(html) {
    html %>%
        rvest::html_nodes(".sold-property__attributes") %>%
        rvest::html_text() %>%
        gsub(",", ".", .) %>%
        strsplit(., "\\s+\\s+") %>%
        .[[1]]
}

get_rooms <- function(html) {
    property_attributes <- get_property_attributes(html)

    rooms <-
        property_attributes %>%
            .[which(. == "Antal rum") + 1] %>%
            gsub(" rum", "", .) %>%
            na_if_empty %>%
            as.numeric

    if (!is.na(rooms)) {
        if (
            any(
                rooms < 1
                , rooms > 10
                )
        ) {
            rooms <- NA
        }
    }

    rooms
}

get_kvm <- function(html) {
    property_attributes <- get_property_attributes(html)

    kvm <-
        property_attributes %>%
            .[which(. == "Boarea") + 1] %>%
            gsub(" m.", "", .) %>%
            na_if_empty %>%
            as.numeric

    if (!is.na(kvm)) {
        if (
            any(
                kvm < 10,
                kvm > 200
            )
        ) {
            kvm <- NA
        }
    }

    kvm

}

get_avgift <- function(html) {
    property_attributes <- get_property_attributes(html)

    avgift <-
        property_attributes %>%
            # .[which(. == "Avgift/månad") + 1] %>%
            .[grep("Avgift/m.nad", .) + 1] %>%
            # gsub(" kr/mån", "", .) %>%
            gsub(" kr/m.n", "", .) %>%
            gsub("\\s", "", .) %>%
            na_if_empty %>%
            as.numeric

    if (!is.na(avgift)) {
        if (
            any(
                avgift < 500,
                avgift > 20000
            )
        ) {
            avgift <- NA
        }
    }

    avgift

}

get_running_costs <- function(html) {
    property_attributes <- get_property_attributes(html)

    running_costs <-
        property_attributes %>%
            .[which(. == "Driftskostnad") + 1] %>%
            # gsub(" kr/år", "", .) %>%
            gsub(" kr/.r", "", .) %>%
            gsub("\\s", "", .) %>%
            na_if_empty %>%
            as.numeric

    if (!is.na(running_costs)) {
        if (
            any(
                running_costs < 500,
                running_costs > 20000
            )
        ) {
            running_costs <- NA
        }
    }

    running_costs

}

get_year_built <- function(html) {
    property_attributes <- get_property_attributes(html)

    year_built <-
        property_attributes %>%
            # .[which(. == "Byggår") + 1] %>%
            .[grep("Bygg.r", .) + 1] %>%
            na_if_empty %>%
            as.numeric

    if (!is.na(year_built)) {
        if (
            any(
                year_built < 1250,
                year_built > 2020
            )
        ) {
            year_built <- NA
        }
    }
    year_built
}

get_agent_name <- function(html) {
    html %>%
        rvest::html_nodes("strong") %>%
        rvest::html_text() %>%
        gsub("[^[:alnum:] ]| -|,", "", .) %>%
        gsub("\\s+", " ", .) %>%
        tolower %>%
        gsub(" ", "", .) %>%
        na_if_empty
}

get_agency <- function(html) {
    html %>%
        rvest::html_nodes(".qa-broker-name+ .broker-card__text") %>%
        rvest::html_text() %>%
        gsub("[^[:alnum:] ]| -|,", "", .) %>%
        gsub("\\s+", " ", .) %>%
        tolower %>%
        gsub(" ", "", .) %>%
        na_if_empty
}

# 04 Get all required scraped data ####

get_all_variables <- function(html) {

    assign(
        "street_number"
        , get_street_number(html)
        , envir = .GlobalEnv)
    assign(
        "street"
        , get_street(html)
        , envir = .GlobalEnv)
    assign(
        "floor_in_building"
        , get_floor_in_building(html)
        , envir = .GlobalEnv)
    assign(
        "type"
        , get_type(html)
        , envir = .GlobalEnv)
    assign(
        "area"
        , get_area(html)
        , envir = .GlobalEnv)
    assign(
        "city"
        , get_city(html)
        , envir = .GlobalEnv)
    assign(
        "day_of_month_sold"
        , get_day_of_month_sold(html)
        , envir = .GlobalEnv)
    assign(
        "month_sold_swedish"
        , get_month_sold_swedish(html)
        , envir = .GlobalEnv)
    assign(
        "year_sold"
        , get_year_sold(html)
        , envir = .GlobalEnv)
    assign(
        "final_price"
        , get_final_price(html)
        , envir = .GlobalEnv)
    assign(
        "asking_price"
        , get_asking_price(html)
        , envir = .GlobalEnv)
    assign(
        "rooms"
        , get_rooms(html)
        , envir = .GlobalEnv)
    assign(
        "kvm"
        , get_kvm(html)
        , envir = .GlobalEnv)
    assign(
        "avgift"
        , get_avgift(html)
        , envir = .GlobalEnv)
    assign(
        "running_costs"
        , get_running_costs(html)
        , envir = .GlobalEnv)
    assign(
        "year_built"
        , get_year_built(html)
        , envir = .GlobalEnv)

    assign(
        "agent_name"
        , get_agent_name(html)
        , envir = .GlobalEnv)

    assign(
        "agency"
        , get_agency(html)
        , envir = .GlobalEnv)
}
