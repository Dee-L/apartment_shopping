# Purpose: Scrape data from Hemnet for later use in building models
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
    "openxlsx"
  )

install_my_pkgs(pkgs)

# 02 create directory for catching all scraped data ####
output_folder_scraped_gparent <<-
  paste0(
    output_folder,
    "01_scraped"
  )

if (!dir.exists(output_folder_scraped_gparent)) {
  dir.create(output_folder_scraped_gparent)
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

# 04 If never scraped before, set max_days_back_to_look to 10 years ago ####
if (length(last_day_scraped) == 0) {
  max_days_back_to_look <- ceiling(10 * 365.25)
} else {
  max_days_back_to_look <- (today - last_day_scraped)
}

# 05 Only execute the rest if there is at least a week of new data ####

if (max_days_back_to_look >= 7) {

  # 06 create directory for scraping data today ####
  output_folder_scraped_parent <<-
    paste0(
      output_folder_scraped_gparent,
      "/date_",
      today_8digit(),
      "/"
    )

  if (!dir.exists(output_folder_scraped_parent)) {
    dir.create(output_folder_scraped_parent)
  }

  # 07 pull in key:value data from Hemnet manually gathered in an excel ####

  area_mappings <-
    openxlsx::read.xlsx(
      xlsxFile = mappings_xlsx,
      sheet = "used_mappings"
    )

  # 08 create string for location search on Hemnet ####

  hemnet_area_number <-
    paste(area_mappings[["Hemnet_number"]],
      collapse = "&location_ids%5B%5D="
    )

  # 09 Initialize parameters for first hemnet "base" page ####

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

    # 10 Scrape all pages from a given hemnet "base" page ####

    scrape_many_from_hemnet_base()

    # 11 After scraping, stop scraping if max price is too high ####

    if (current_max_price >= final_max_price) {
      break

    # 12 If max price not too high, re-establish prices for new "base" page ####
    } else {
      current_min_price <<- current_max_price + 1
      current_max_price <<- final_max_price
      hemnet_base_page_number <<- 1
    }
  }
}
