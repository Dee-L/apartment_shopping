# Purpose: Scrape data from Hemnet for later use in building models
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this scripts are installed ####
pkgs <-
  c(
    "lubridate",
    "openxlsx",
    "rvest",
    "xml2",
    "stringr"
  )

install_my_pkgs(pkgs)

# 02 Set where data will be saved ####

output_folder_scraped <- paste0(output_folder, "01_scraped_new/")
dir.create(output_folder_scraped)

# 03 Pull in data for area names and numbers for creating URLs to scrape ####

# pull in key:value data from Hemnet manually gathered in an excel

area_mappings <-
  openxlsx::read.xlsx(
    xlsxFile = paste0(input_folder, "i02_mappings.xlsx"),
    sheet = "used_mappings")

# create string for location search on Hemnet

hemnet_area_number <-
  paste(area_mappings[["Hemnet_number"]],
  collapse = "&location_ids%5B%5D=")

# Initialize limits for scraping

type <- "bostadsratt"
min_rooms <- 1
max_avgift <- 10000
min_price <- 1000000
max_price <- 10000000
years_to_look_back <- 10
sorting_feature <- "sale_date"
sorting_direction <- "desc"
first_page <- 1

hemnet_base_address <-
  paste0(
    "https://www.hemnet.se/salda/bostader?",
    "location_ids%5B%5D=",
    hemnet_area_number,
    "&item_types%5B%5D=",
    type,
    "&rooms_min=",
    min_rooms,
    "&fee_max=",
    max_avgift,
    "&selling_price_min=",
    min_price,
    "&selling_price_max=",
    max_price,
    "&sold_age=",
    years_to_look_back,
    "y",
    "&by=",
    sorting_feature,
    "&order=",
    sorting_direction,
    "&page=",
    first_page
  )

# Create html session for navigating and scraping

session <-
  hemnet_base_address %>%
  rvest::html_session()

# Calculate the number of pages from the number of objects returned in the query

total_pages <-
  session %>%
  xml2::read_html() %>%
  rvest::html_nodes("#result .clear-children .centered") %>%
  rvest::html_text() %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("Visar150av", "", .) %>%
  as.numeric() %>%
  prod(., 1 / 50) %>%
  ceiling(.)

# 04 My fxns this program only ####

# Scraper function for hemnet variables

scrape_or_skip <-
    function(
      name_of_target,
      scrape_and_test_expression,
      my_session = session) {
        
        results <- try(expr = scrape_and_test_expression, silent = TRUE)
        if (inherits(results, "try-error")) {
            message("Exception with ",
                    paste0(deparse(substitute(name_of_target))),
                    " on page:\n\n",
                    paste0(my_session$url),
                    "\n\nCustom error message was:\n\n",
                    error_message,
                    "\n\nSkipping to next page.\n\n")
            next
        } else {
            assign(
              deparse(substitute(name_of_target)),
              scrape_and_test_expression,
              envir = parent.frame())
        }
    }

# Custom error message and stop processing

stop_and_notify <- function(error_message) {
  assign("error_message", error_message, envir = .GlobalEnv)
  stop(error_message)
}

# 05 Loop to scrape the pages ####

for (page in seq_len(total_pages)) {

  # initiate a counter for naming files

  if(exists("counter")) {
    counter <- counter + 1
  } else {
    counter <- 1
  }
  # update base_address

  hemnet_base_address <-
    paste0(
      "https://www.hemnet.se/salda/bostader?by=sale_date&fee_max=",
      max_avgift,
      "&item_types%5B%5D=bostadsratt",
      "&item_types%5B%5D=radhus&item_types%5B%5D=villa&location_ids%5B%5D=",
      hemnet_area_number,
      "&new_construction=include&order=desc&page=",
      page,
      "&selling_price_max=",
      max_price,
      "&objectType=&sold_age=",
      years_to_look_back,
      "y"
    )

  # update session

  session <-
    hemnet_base_address %>%
    rvest::html_session()

  cat(
      "Hemnet base page is ",
      page,
      " of ",
      total_pages,
      ".\n\n"
  )

  # count the number of links to scrape from

  pages_to_scrape <-
    session %>%
    rvest::html_nodes(".item-link-container") %>%
    rvest::html_attr("href")

  cat(
    paste0(
      length(pages_to_scrape),
      " pages to scrape from this base page."),
    "\n\n"
  )

  for (page_to_scrape in seq_along(pages_to_scrape)) {

    cat(
      paste0(
        page_to_scrape,
        " of ",
        length(pages_to_scrape),
        " pages on this base page."),
      "\n\n")

    try_wait_retry({

      # update session

      session <-
        pages_to_scrape[page_to_scrape] %>%
        rvest::html_session()

      # Scraping data

      # Address data from the header
      scrape_or_skip(
        name_of_target = address,
        scrape_and_test_expression = {
          a <-
            session %>%
              xml2::read_html() %>%
              rvest::html_nodes(
                ".sold-property__address"
              ) %>%
              rvest::html_text() %>%
              gsub("[^[:alnum:] ]", "", .) %>%
              gsub("  Slutpris  ", "", .)

          if(
            any(
              (length(a) == 0)
              , (!is.character(a))
              , (!(length(a)> 0))
              )
            ) {
              stop_and_notify("Impossible address data from top of screen")
            }

          assign(
            "street_number",
            (regmatches(a, gregexpr("[[:digit:]]+", a)) %>%
              .[[1]] %>%
              .[1] %>%
              as.numeric()),
            envir = parent.frame())

          if(
            any(
              (length(street_number) == 0)
              , !(street_number > 0)
              )
            ) {
              stop_and_notify("Impossible street number")
            }

          assign(
            "street",
            (substr(
              a,
              1,
              gregexpr(
                as.character(street_number),
                a)[[1]][1] - 2
            )) %>%
              tolower %>%
              gsub(" ", "", .),
            envir = parent.frame())

          if(
            any(
              (length(street) == 0)
              , (!is.character(street))
              , (!(length(street) > 0))
              )
           ) {
              stop_and_notify("Impossible street")
            }

          floor_info <-
            substr(a,
             nchar(street) +
               nchar(street_number) +
               3,
             nchar(a)) %>% tolower
          
          if(grepl("av", floor_info)) {
            floor_info %<>%
              substr(
                .,
                1,
                gregexpr(
                  "av",
                  .)[[1]][1] - 1)
            }

          assign(
            "floor",
            dplyr::case_when(
              (grepl("vå", floor_info) |
                grepl("tr", floor_info)
                ) ~ 
                  regmatches(
                    floor_info,
                    gregexpr("[[:digit:]]+", floor_info)
                    ) %>%
                as.numeric,
              grepl("bv", floor_info) ~ 0,
              TRUE ~ as.numeric(NA)
              ),
            envir = parent.frame()
          )

          if(!is.na(floor)) {
            if(
              any(
                (length(floor) == 0)
                , (floor < 1)
                , (floor > 40)
                )
              ) {
                stop_and_notify("Impossible floor")
              }
            }
          a
        }
        )

      # Cleaning memory since harvested street and street_number

      rm(address)

      # Metadata from the top of the web page

      scrape_or_skip(
        name_of_target = metadata_vector,
        scrape_and_test_expression = {
          m <-
            session %>%
              xml2::read_html() %>%
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

          if(
            any(
              (length(m) == 0)
              , !is.character(m)
              )
            ) {
              stop_and_notify("Impossible metadata on top of page.")
            }

          type <- m[[1]]

          if(
            any(
              (length(type) == 0)
              , (grep("lägenhet", type) < 1)
              )
            ) {
            stop_and_notify("Not a lägenhet")
           }

          assign(
            "area",
            m[[2]],
            envir = parent.frame())

          assign(
            "city",
            m[[3]] %>%
            gsub(" ", "", .) %>%
            gsub("kommun", "", .),
            envir = parent.frame())

          if(
            any(
              (length(city) == 0)
              , (grep(
                  paste(
                    c("stockholm", "solna", "uppsala"),
                    collapse="|"),
                  city) < 1)
              )
            ) {
            stop_and_notify("Not in Stockholm, Solna, or Uppsala")
          }

          d <-
            m[[4]] %>%
              strsplit(., " ") %>%
              .[[1]]

          assign(
            "day_of_month_sold",
            d[3] %>% as.numeric,
            envir = parent.frame())

          if(
            any(
              (length(day_of_month_sold) == 0)
              , (day_of_month_sold <= 0)
              , (day_of_month_sold > 31)
              )
            ) {
            stop_and_notify("Impossible day of month sold")
           }

          assign(
            "month_sold_swedish",
            d[4] %>% tolower,
            envir = parent.frame())

          if(
            any(
              (length(month_sold_swedish) == 0)
              , (
                  grep(
                    paste(
                      c(
                        "jan", "feb", "mar",
                        "apr", "maj", "jun",
                        "jul", "aug", "sep",
                        "okt", "nov", "dec"
                        ),
                      collapse="|"),
                    month_sold_swedish) < 1)
                )
                ) {
            stop_and_notify("Impossible month sold")
          }
          
          assign(
            "year_sold",
            d[5] %>% as.numeric,
            envir = parent.frame())

          if(
            any(
              (length(year_sold) == 0)
              ,  (year_sold < 2010)
              , (day_of_month_sold > 20)
            )) {
            stop_and_notify("Impossible year sold")
           }
           m
           }
      )

      # Cleaning memory since harvested area, city, day_of_month_sold,
      # month_sold_swedish, year_sold

      rm(metadata_vector)

      # Sales price from middle of the page

      scrape_or_skip(
        name_of_target = price,
        scrape_and_test_expression = {
          p <-
            session %>%
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

          if(
            any(
              (length(p) == 0)
              , (p < 1000000)
              , (p > 10000000)
              )
            ) {
              stop_and_notify("Impossible price")
        }
        p
        }
        )

      # Asking price from the middle of the page

      scrape_or_skip(
        name_of_target = asking_price,
        scrape_and_test_expression = {
          pricestats_vector <-
            session %>%
              xml2::read_html() %>%
              rvest::html_nodes(".sold-property__price-stats") %>%
              rvest::html_text() %>%
              gsub("[^[:alnum:] ]| -|,", "", .) %>%
              gsub("\\s+", " ", .) %>%
              strsplit(., " ") %>%
              .[[1]]
          
          ap <-
            pricestats_vector[which(pricestats_vector == "pris") + 1] %>%
              as.numeric()

          if(
            any(
              (length(ap) == 0)
              , (ap < 0)
              , (ap > 12000000)
              )
            ) {
              stop_and_notify("Impossible asking_price")
          }
          ap
          }
          )

      # Property attributes from the middle of the page

      scrape_or_skip(
        name_of_target = attributes_vector,
        scrape_and_test_expression = {
          a <-
            session %>%
              xml2::read_html() %>%
              rvest::html_nodes(".sold-property__attributes") %>%
              rvest::html_text() %>%
              gsub(",", ".", .) %>%
              strsplit(., "\\s+\\s+") %>%
              .[[1]]

          assign(
            "rooms",
            a[which(a == "Antal rum") + 1] %>%
              gsub(" rum", "", .) %>%
              as.numeric(),
            envir = parent.frame())

          if(
            any(
              (length(rooms) == 0)
              , (rooms < 1)
              , (rooms > 10)
              )
            ) {
            stop_and_notify("Impossible number of rooms")
           }

          assign(
            "kvm",
            a[which(a == "Boarea") + 1] %>%
              gsub(" m²", "", .) %>%
              as.numeric(),
            envir = parent.frame())

          if(
            any(
              (length(kvm) == 0)
              , (kvm < 10)
              , (kvm > 200)
              , (kvm <= rooms)
              )
            ) {
            stop_and_notify("Impossible kvm")
           }

          assign(
            "avgift",
            a[which(a == "Avgift/månad") + 1] %>%
              gsub(" kr/mån", "", .) %>%
              gsub("\\s", "", .) %>%
              as.numeric(),
            envir = parent.frame())

          if(
            any(
              (length(avgift) == 0)
              , (avgift < 500)
              , (avgift > 20000)
              )
            ) {
            stop_and_notify("Impossible avgift")
           }

          assign(
            "running_costs",
            a[which(a == "Driftskostnad") + 1] %>%
              gsub(" kr/år", "", .) %>%
              gsub("\\s", "", .) %>%
              as.numeric(),
            envir = parent.frame())

          if(length(running_costs) == 0) {
            assign("running_costs",
            as.numeric(NA),
            envir = parent.frame())
          }

          if(!is.na(floor)) {
            if(
              any(
                (length(running_costs) == 0)
                , (running_costs < 500)
                , (running_costs > 20000)
                )
              ) {
              stop_and_notify("Impossible running costs")
            }
          }

          assign(
            "year_built",
            a[which(a == "Byggår") + 1] %>%
              as.numeric(),
            envir = parent.frame())

          if(
            any(
              (length(year_built) == 0)
              , (year_built < 1850)
              , (year_built > 2020)
              , (year_built > year_sold)
              )
            ) {
            stop_and_notify("Impossible year built")
           }
          a
        }
        )

      # Cleaning memory since harvested rooms, kvm, avgift,
      # running_costs, year_built

      rm(attributes_vector)
      
      # Broker contact from the bottom of the page

      scrape_or_skip(
        name_of_target = agent_name,
        scrape_and_test_expression = {
          an <- session %>%
              xml2::read_html() %>%
              rvest::html_nodes(
                "strong") %>%
              rvest::html_text() %>%
              gsub("[^[:alnum:] ]| -|,", "", .) %>%
              gsub("\\s+", " ", .) %>%
              tolower %>%
              gsub(" ", "", .)

          if(length(an) == 0) {
            an <- as.character(NA)
          }
          an
          }
          )

      scrape_or_skip(
        name_of_target = agency,
        scrape_and_test_expression = {
          ac <- session %>%
              xml2::read_html() %>%
              rvest::html_nodes(
                ".qa-broker-name+ .broker-card__text") %>%
              rvest::html_text() %>%
              gsub("[^[:alnum:] ]| -|,", "", .) %>%
              gsub("\\s+", " ", .) %>%
              tolower %>%
              gsub(" ", "", .)

          if(length(ac) == 0) {
            ac <- as.character(NA)
          }
          ac
          }
          )

      # Creating the df object for saving

      sold_object <- data.frame(
        unique_id = counter,
        price = na_if_empty(price),
        asking_price = na_if_empty(asking_price),
        rooms = na_if_empty(rooms),
        kvm = na_if_empty(kvm),
        floor = na_if_empty(floor),
        avgift = na_if_empty(avgift),
        running_costs = na_if_empty(running_costs),
        city = na_if_empty(city),
        area = na_if_empty(area),
        street = na_if_empty(street),
        day_of_month_sold = na_if_empty(day_of_month_sold),
        month_sold_swedish = na_if_empty(month_sold_swedish),
        year_sold = na_if_empty(year_sold),
        year_built = na_if_empty(year_built),
        agent_name = na_if_empty(agent_name),
        agency = na_if_empty(agency),
        url = session$url
      )

      # Saving the object

      object_name <-
        paste0(
          "o",
          stringr::str_pad(counter, 5, pad = "0"),
          "_",
          year_sold,
          "_",
          city,
          "_",
          street,
          "_",
          street_number)

      assign(object_name, sold_object)
      save(
        list = object_name,
        file = paste0(output_folder_scraped, object_name))

    })
  }
}
