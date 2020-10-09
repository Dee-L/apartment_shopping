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

install_my_pkgs(pkgs)

# 02 Get html ####

get_html <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html()
}

# 03 Getting data from Hemnet html ####

get_address <- function(html) {
    html %>%
        rvest::html_nodes(".sold-property__address") %>%
        rvest::html_text() %>%
        gsub("[^-[:alnum:]\\+/&\\., ]", "", .) %>%
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
            is.na(street_number)
            , street_number < 0
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

get_floor_in_building <- function(html) {
    address <- get_address(html)

    street_number <- get_street_number(html)
    # Cannot use normal get_street because it removes spaces which
    # would through off the character count when extracting the floor info
    street <- substr(
        address
        , 1
        , gregexpr(as.character(street_number), address)[[1]][1] - 2) %>%
        na_if_empty

    floor_info <-
        substr(
            address
            , nchar(street) + nchar(street_number) + 3
            , nchar(address)
            )

    # Remove "strand" from floor info if it exits
    floor_info %<>% gsub("strand", "", .)

    # Replace "och" with dash to get in-between floors
    floor_info %<>%
        gsub("&", "och", .) %>%
        gsub(" och ", "och", .) %>%
        gsub("och", "-", .)

    floor_info %<>%
        gsub("/", "av", .)

    if (grepl("av", floor_info)) {
      floor_info %<>%
        substr(., 1, gregexpr("av", .)[[1]][1] - 1)
    }

    floor_in_building <-
        # See if text indicates it's a floor number
        if (
            any(
                (grepl("v.n", floor_info)),
                (grepl("tr", floor_info))
                )
            ) {
                # Make it 0 floor if bv
                if (grepl("bv", floor_info)) {
                    as.numeric(0)
                } else {

                    # If comma before a digit, change to point
                    if (grepl(",\\d", floor_info)) {
                        floor_info %<>%
                            gsub(",", ".", .)
                    }

                    # Also remove any text after "lg"
                    floor_info %<>%
                        gsub("lg.{1,}", "", .)

                    # If no dash, plus, nor slash, keep digits
                    if (!grepl("\\+|-|/", floor_info)) {
                        floor_info %>% gsub("[^0-9.-]", "", .) %>% as.numeric
                    # If dash, plus, or slash, convert to half floor
                    } else {
                        regmatches(
                            floor_info,
                            gregexpr(
                                "[[:digit:]]+",
                                floor_info)
                            ) %>%
                            .[[1]] %>%
                            .[length(.) - 1] %>%
                            as.numeric %>%
                            sum(0.5)
                    }
                }
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

get_dayofmonth_sold <- function(html) {
    date_sold <- get_date_sold(html)

    dayofmonth_sold <-
        date_sold[3] %>%
            na_if_empty %>%
            as.numeric

    if (
        any(
            dayofmonth_sold < 1
            , dayofmonth_sold > 31
            )
     ) {
        dayofmonth_sold <- NA
    }

    dayofmonth_sold
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

    year_sold

}

get_selling_price <- function(html) {
    selling_price <-
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
            selling_price < first_min_price
            , selling_price > final_max_price
            )
     ) {
        selling_price <- NA
    }

    selling_price

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
        "dayofmonth_sold"
        , get_dayofmonth_sold(html)
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
        "selling_price"
        , get_selling_price(html)
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
