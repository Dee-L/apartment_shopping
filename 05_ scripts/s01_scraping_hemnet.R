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

# 03 Initialize parameters for first hemnet "base" page ####

housing_type <- "bostadsratt"
min_rooms <- 1
max_avgift <- 10000
final_min_price <- 1000000
current_min_price <- final_min_price
final_max_price <- 15500000
current_max_price <- final_max_price
years_to_look_back <- 10
sorting_feature <- "sale_date"
sorting_direction <- "desc"
hemnet_base_page_number <- 1

repeat {

  # 04 Scrape all pages from a given hemnet "base" page ####

  # This is working on the first round, but then only finding 1 page when
  # trying to update

  scrape_many_from_hemnet_base()

  # 05 After scraping, stop scraping if max price is too high ####

  if (current_max_price >= final_max_price) {
    break

  # 06 If max price not too high, re-establish prices for new "base" page ####

  } else {
    current_min_price <<- current_max_price + 1
    current_max_price <<- final_max_price
    hemnet_base_page_number <<- 1
  }
}


# Cycling is working. Now to QC the data being pulled