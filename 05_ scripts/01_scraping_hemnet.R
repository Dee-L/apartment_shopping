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

# 02 Pull in data for area names and numbers for creating URLs to scrape ####

# pull in key:value data from Hemnet manually gathered in an excel

area_mappings <-
  openxlsx::read.xlsx(
    xlsxFile = mappings_xlsx,
    sheet = "used_mappings")

# create string for location search on Hemnet

hemnet_area_number <-
  paste(area_mappings[["Hemnet_number"]],
  collapse = "&location_ids%5B%5D=")

# Initialize limits for scraping

# Website only gives 50 pages worth, or 2500 hits, although there are 33923
# in the search space. I have to divide my searches so that I capture more data
# Perhaps dividing the price range into 200 buckets
# Estimated time to complete: 50 searches on one page takes about 5 minutes
# 5 minutes per page * 50 pages per bucket * 200 buckets
# 50000 minutes, which is 833.3333 hours, which is 34.7 days

# want to use "repeat" loop to keep doing work until max_price >= 15500000
# want to use some kind of loop to increase min_price each time

# 03 Initialize first search attempt ####

type <- "bostadsratt"
min_rooms <- 1
max_avgift <- 10000
min_price <- 1000000
max_price <- min_price + 1000000
years_to_look_back <- 10
sorting_feature <- "sale_date"
sorting_direction <- "desc"
first_page <- 1

# repeat {
#   # all code below goes in here
#   if (max_price >= 15500000) {
#     break
#   } else {
#     min_price <- max_price + 1
#   }
# }

scrape_many_from_hemnet_base()

# 04 Update price parameters so that I don't have too many pages to scrape ####

repeat {
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

  html_to_read <-
    hemnet_base_address %>%
    get_html

  # Calculate the number of pages from the page landed on

  total_pages <-
    html_to_read %>%
    rvest::html_nodes("#result .clear-children .centered") %>%
    rvest::html_text() %>%
    gsub("[^[:alnum:] ]| ", "", .) %>%
    gsub("Visar150av", "", .) %>%
    as.numeric() %>%
    prod(., 1 / 50) %>%
    ceiling(.)

  cat(
    "total pages are: "
    , total_pages
    , "\n\n"
  )

  # Adjust max price if too few / too many pages to search
  if (total_pages < 35) {
    cat(
      "Increasing max_price.\n\nMax price was: "
      , max_price
      , "\n\n"
      )
    max_price <- max_price + 1000000
    cat("Max price is now: ", max_price, "\n\n")
  } else if (total_pages == 50) {
    cat(
      "Decreasing max_price.\n\nMax price was: "
      , max_price
      , "\n\n"
    )
    max_price <- max_price - max_price / 10
    cat("Max price is now: ", max_price, "\n\n")
  } else if (
    all(
      35 < total_pages
      , total_pages < 50
      )
    ) {
    break
  }
}

# 05 Set where data will be saved ####

output_folder_scraped <-
  paste0(
    output_folder,
    "01_scraped_new/",
    "price_",
    min_price,
    "_to_",
    max_price,
    "/"
  )

dir.create(output_folder_scraped)

# 06 Loop to scrape the pages ####

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

  html_to_read <-
    hemnet_base_address %>%
    get_html

  cat(
      "Hemnet base page is",
      page,
      "of",
      total_pages,
      "\n\n"
  )

  # count the number of links to scrape from

  pages_to_scrape <-
    html_to_read %>%
    rvest::html_nodes(".item-link-container") %>%
    rvest::html_attr("href")

  cat(length(pages_to_scrape), "pages to scrape from this base page.")

# 07 Inner loop ####

  for (page_to_scrape in seq_along(pages_to_scrape)) {

    cat(
      paste0(
        page_to_scrape,
        " of ",
        length(pages_to_scrape),
        " pages on this base page."),
      "\n\n")

    # update url

    url_to_scrape <- pages_to_scrape[page_to_scrape]

    html_to_read <- try_wait_retry(get_html(url_to_scrape))

# 08 Scrape and test all data from the url. If fail, capture fails. ####

    try_scrape_save_hemnet_html()
  }
  }