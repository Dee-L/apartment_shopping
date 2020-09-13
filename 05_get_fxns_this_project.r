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

# 02 Getting data from Hemnet url ####

get_address <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html() %>%
        rvest::html_nodes(
          ".sold-property__address"
        ) %>%
        rvest::html_text() %>%
        gsub("/", " av ", .) %>%
        gsub("[^[:alnum:] ]", "", .) %>%
        gsub("  Slutpris  ", "", .)
}

get_street_number <- function(a = address) {
    regmatches(a, gregexpr("[[:digit:]]+", a)) %>%
        .[[1]] %>%
        .[1] %>%
        as.numeric
}

get_street <- function(a = address, s_n = street_number) {
    substr(
        a,
        1,
        gregexpr(
          as.character(s_n),
          a)[[1]][1] - 2
      ) %>%
        tolower %>%
        gsub(" ", "", .)
}

get_floor_in_building <-
    function(
        a = address
        , s = street
        , s_n = street_number) {
        floor_info <- substr(address,
            nchar(s) +
            nchar(s_n) +
            3,
            nchar(a)) %>% tolower

    if (grepl("av", floor_info)) {
      floor_info %<>%
        substr(
          .,
          1,
          gregexpr(
            "av",
            .)[[1]][1] - 1)
    }

    if (
        any(
            (grepl("vå", floor_info)),
            (grepl("tr", floor_info))
        )) {
        regmatches(
            floor_info,
            gregexpr("[[:digit:]]+",
            floor_info)) %>%
            .[[1]] %>%
            .[length(.)] %>%
            as.numeric()
          } else if (
            grepl("bv", floor_info)
          ) {
            as.numeric(0)
          } else {
            as.numeric(NA)
          }
}

get_type_area_date <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html() %>%
        rvest::html_nodes(".sold-property__metadata") %>%
        rvest::html_text() %>%
        gsub("\\s+", " ", .) %>%
        # strsplit on '-' may not work, see:
        # https://www.hemnet.se/salda/lagenhet-1rum-vasastan-birkastan-stockholms-kommun-rorstrandsgatan-23,-3tr-1250012
        # split evertyhing before first "-" as type
        # split everything from såld onwards as d
        # split the rest as area/city to be split later
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

get_type <- function(t_a_d = type_area_date) {
    t_a_d[[1]]
}

get_area <- function(t_a_d = type_area_date) {
    t_a_d[[2]]
}

get_city <- function(t_a_d = type_area_date) {
    t_a_d[[3]] %>%
        gsub(" ", "", .) %>%
        gsub("kommun", "", .)
}

get_date_sold <- function(t_a_d = type_area_date) {
    t_a_d[[4]] %>%
            strsplit(., " ") %>%
            .[[1]]
}

get_day_of_month_sold <- function(d_s = date_sold) {
    d_s[3] %>% as.numeric()
}

get_month_sold_swedish <- function(d_s = date_sold) {
    d_s[4] %>% tolower()
}

get_year_sold <- function(d_s) {
    d_s[5] %>% as.numeric()
}

get_final_price <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html() %>%
        rvest::html_nodes(
            ".sold-property__price-value"
        ) %>%
        rvest::html_text() %>%
        gsub("[^[:alnum:] ]| -|,", "", .) %>%
        gsub("\\s+", " ", .) %>%
        strsplit(., " ") %>%
        .[[1]] %>%
        .[1] %>%
        as.numeric()
}

get_asking_price <- function(url) {
    price_stats_vector <-
        url %>%
            rvest::html_session() %>%
            xml2::read_html() %>%
            rvest::html_nodes(".sold-property__price-stats") %>%
            rvest::html_text() %>%
            gsub("[^[:alnum:] ]| -|,", "", .) %>%
            gsub("\\s+", " ", .) %>%
            strsplit(., " ") %>%
            .[[1]]

    pricestats_vector %>%
        .[which(. == "pris") + 1] %>%
        as.numeric()
}

get_property_attributes <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html() %>%
        rvest::html_nodes(".sold-property__attributes") %>%
        rvest::html_text() %>%
        gsub(",", ".", .) %>%
        strsplit(., "\\s+\\s+") %>%
        .[[1]]
}

get_rooms <- function(p_a = property_attributes) {

    p_a %>%
        .[which(. == "Antal rum") + 1] %>%
        gsub(" rum", "", .) %>%
        as.numeric()
}

get_kvm <- function(p_a = property_attributes) {
    p_a %>%
        .[which(. == "Boarea") + 1] %>%
        gsub(" m²", "", .) %>%
        as.numeric()
}

get_avgift <- function(p_a = property_attributes) {
    p_a %>%
        .[which(. == "Avgift/månad") + 1] %>%
        gsub(" kr/mån", "", .) %>%
        gsub("\\s", "", .) %>%
        as.numeric()
}

get_running_costs <- function(p_a = property_attributes) {
    p_a %>%
        .[which(. == "Driftskostnad") + 1] %>%
        gsub(" kr/år", "", .) %>%
        gsub("\\s", "", .) %>%
        as.numeric()
}

get_year_built <- function(p_a = property_attributes) {
    p_a %>%
        .[which(. == "Byggår") + 1] %>%
        as.numeric()
}

get_agent_info <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html() %>%
        rvest::html_nodes(
            "strong") %>%
        rvest::html_text() %>%
        gsub("[^[:alnum:] ]| -|,", "", .) %>%
        gsub("\\s+", " ", .) %>%
        tolower %>%
        gsub(" ", "", .)
}

get_agency <- function(url) {
    url %>%
        rvest::html_session() %>%
        xml2::read_html() %>%
        rvest::html_nodes(
            ".qa-broker-name+ .broker-card__text") %>%
        rvest::html_text() %>%
        gsub("[^[:alnum:] ]| -|,", "", .) %>%
        gsub("\\s+", " ", .) %>%
        tolower %>%
        gsub(" ", "", .)
}

# 03 Get all required scraped data ####

get_all_variables <- function(url) {

    assign(
        "address"
        , get_address(url)
        , envir = .GlobalEnv)
    assign(
        "street_number"
        , get_street_number()
        , envir = .GlobalEnv)
    assign(
        "street"
        , get_street()
        , envir = .GlobalEnv)
    assign(
        "floor_in_building"
        , get_floor_in_building()
        , envir = .GlobalEnv)

    assign(
        "type_area_date"
        , get_type_area_date(url)
        , envir = .GlobalEnv)
    assign(
        "type"
        , get_type()
        , envir = .GlobalEnv)
    assign(
        "area"
        , get_area()
        , envir = .GlobalEnv)
    assign(
        "city"
        , get_city()
        , envir = .GlobalEnv)
    assign(
        "date_sold"
        , get_date_sold()
        , envir = .GlobalEnv)
    assign(
        "day_of_month_sold"
        , get_day_of_month_sold()
        , envir = .GlobalEnv)
    assign(
        "month_sold_swedish"
        , get_month_sold_swedish()
        , envir = .GlobalEnv)
    assign(
        "year_sold"
        , get_year_sold()
        , envir = .GlobalEnv)

    assign(
        "final_price"
        , get_final_price(url)
        , envir = .GlobalEnv)

    assign(
        "asking_price"
        , get_asking_price(url)
        , envir = .GlobalEnv)

    assign(
        "property_attributes"
        , get_property_attributes(url)
        , envir = .GlobalEnv)
    assign(
        "rooms"
        , get_rooms()
        , envir = .GlobalEnv)
    assign(
        "kvm"
        , get_kvm()
        , envir = .GlobalEnv)
    assign(
        "avgift"
        , get_avgift()
        , envir = .GlobalEnv)
    assign(
        "running_costs"
        , get_running_costs()
        , envir = .GlobalEnv)
    assign(
        "year_built"
        , get_year_built()
        , envir = .GlobalEnv)

    assign(
        "agent_info"
        , get_agent_info(url)
        , envir = .GlobalEnv)

    assign(
        "agency"
        , get_agency(url)
        , envir = .GlobalEnv)
}
