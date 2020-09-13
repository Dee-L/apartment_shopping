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
    "lubridate"
    , "openxlsx"
    , "rvest"
    , "xml2"
    , "dplyr"
    , "stringr"
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
max_price <- 15500000
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

# 04 Loop to scrape the pages ####

for (page in seq_len(total_pages)) {

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

# 05 Inner loop ####

  for (page_to_scrape in seq_along(pages_to_scrape)) {

    # initiate or update counter for naming files

    if (exists("counter")) {
      counter <- counter + 1
    } else {
      assign("counter", 1, envir = .GlobalEnv)
    }

    cat(
      paste0(
        page_to_scrape,
        " of ",
        length(pages_to_scrape),
        " pages on this base page."),
      "\n\n")

    # update url

    url_to_scrape <- pages_to_scrape[page_to_scrape]

    # 06 tryCatch for scraping data ####

    # tryCatch( {

    # 07 Scrape all data from the url ####
    get_all_variables()

    # 08 Testing all scraped data ####
    test_all()

    # 09 Creating the df object for saving ####

    sold_object <- data.frame(
      price = price,
      asking_price = asking_price,
      rooms = rooms,
      kvm = kvm,
      floor_in_building = floor_in_building,
      avgift = avgift,
      running_costs = running_costs,
      city = city,
      area = area,
      street = street,
      day_of_month_sold = day_of_month_sold,
      month_sold_swedish = month_sold_swedish,
      year_sold = year_sold,
      year_built = year_built,
      agent_name = agent_name,
      agency = agency,
      url = url_to_scrape
    )

    # Saving the object

    object_name <-
      paste0(
        "o",
        stringr::str_pad(counter, 6, pad = "0"),
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
  },

  # 14 Condition if error ####

  error = function(error_message) {
    message("There was an error. It was: ")
    message(error_message)
    # message("Custom error message was: ")
    # message(custom_error_message)
    
    if (exists("failed_pages")) {
      failed_page <- 
        data.frame(
          counter = counter
          , url = session$url
          , error_message = error_message
          # , custom_error_message = custom_error_message
        )
      failed_pages %<>% rbind(., failed_page)
    } else {
      assign(
        "failed_pages"
        , data.frame(
          counter = counter
          , url = session$url
          , error_message = error_message
          # , custom_error_message = custom_error_message
        )
        , envir = .GlobalEnv)
    
      failed_pages_file_name <- 
        paste0(output_folder_scraped, "failed_pages")
      save(
        failed_pages
        , file = failed_pages_file_name)
    )
  }
  )
  }
}
