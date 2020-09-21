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
    "openxlsx"
  )

install_my_pkgs(pkgs)

# 02 create directory for scraping data today ####
output_folder_scraped_gparent <<-
  paste0(
    output_folder,
    "01_scraped/"
  )

if (!dir.exists(output_folder_scraped_gparent)) {
  dir.create(output_folder_scraped_gparent)
}

output_folder_scraped_parent <<-
  paste0(
    output_folder_scraped_gparent,
    "date_",
    gsub("-", "", Sys.Date()),
    "/"
  )

if (!dir.exists(output_folder_scraped_parent)) {
  dir.create(output_folder_scraped_parent)
}

# 03 Choose days to look back based on previous scrapes ####
last_day_scraped <-
  output_folder_scraped_gparent %>%
    list.dirs(recursive = F) %>%
    .[length(.)] %>%
    right(8) %>%
    as.numeric

today <-
  Sys.Date() %>%
    gsub("-", "", .) %>%
    as.numeric

max_days_back_to_look <- (today - last_day_scraped)

# Only execute the rest of the script if there is at least a week of new data

if (max_days_back_to_look >= 7) {

  # 03 Pull in data for area names and numbers for creating URLs to scrape ####

  # pull in key:value data from Hemnet manually gathered in an excel

  area_mappings <-
    openxlsx::read.xlsx(
      xlsxFile = mappings_xlsx,
      sheet = "used_mappings"
    )

  # create string for location search on Hemnet

  hemnet_area_number <-
    paste(area_mappings[["Hemnet_number"]],
      collapse = "&location_ids%5B%5D="
    )

  # 04 Initialize parameters for first hemnet "base" page ####

  housing_type <- "bostadsratt"
  min_rooms <- 1
  max_avgift <- 10000
  first_min_price <- 1000000
  current_min_price <- first_min_price
  final_max_price <- 15500000
  current_max_price <- final_max_price
  days_to_look_back <- max_days_back_to_look
  sorting_feature <- "sale_date"
  sorting_direction <- "desc"
  hemnet_base_page_number <- 1

  repeat {

    # 05 Scrape all pages from a given hemnet "base" page ####

    # This is working on the first round, but then only finding 1 page when
    # trying to update

    scrape_many_from_hemnet_base()

    # 06 After scraping, stop scraping if max price is too high ####

    if (current_max_price >= final_max_price) {
      break

    # 06 If max price not too high, re-establish prices for new "base" page ####
    } else {
      current_min_price <<- current_max_price + 1
      current_max_price <<- final_max_price
      hemnet_base_page_number <<- 1
    }
  }
}
