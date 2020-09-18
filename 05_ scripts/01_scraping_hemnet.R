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

type <- "bostadsratt"
min_rooms <- 1
max_avgift <- 10000
min_price <- 1000000
max_price <- min_price + 1000000
years_to_look_back <- 10
sorting_feature <- "sale_date"
sorting_direction <- "desc"
first_page <- 1

repeat {

  # 04 Scrape all pages from a given hemnet "base" page ####

  scrape_many_from_hemnet_base()

  # 05 After scraping, stop scraping if max price is too high ####

  if (max_price >= 15500000) {
    break

  # 06 If max price not too high, re-establish prices for new "base" page ####

  } else {
    min_price <<- max_price + 1
    max_price <<- min_price + 1000000
  }
}


#Not working, did not cycle a second time.
# Got error "Current page doesn't appear to be html"
# Max price wasn't getting updated by the inner script -
# it should have become 1458000, but it was still 2000000
  # I since updated the assignment to use << instead of < in scrape_many_from...
