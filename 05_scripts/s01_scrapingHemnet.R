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

installMyPkgs(pkgs)

# 02 create directory for catching all scraped data ####
outputFolderScrapedGparent <<-
  paste0(
    outputFolder,
    "01_scraped"
  )

if (!dir.exists(outputFolderScrapedGparent)) {
  dir.create(outputFolderScrapedGparent)
}

# 03 Choose days to look back based on previous scrapes ####
lastDayScraped <-
  outputFolderScrapedGparent %>%
    list.dirs(recursive = F) %>%
    .[length(.)] %>%
    right(8) %>%
    as.numeric

today <-
  Sys.Date() %>%
    gsub("-", "", .) %>%
    as.numeric

# 04 If never scraped before, set maxDaysBackToLook to 10 years ago ####
if (length(lastDayScraped) == 0) {
  maxDaysBackToLook <- ceiling(10 * 365.25)
} else {
  maxDaysBackToLook <- (today - lastDayScraped)
}

# 05 Only execute the rest if there is at least a week of new data ####

if (maxDaysBackToLook >= 7) {

  # 06 create directory for scraping data today ####
  outputFolderScrapedParent <<-
    paste0(
      outputFolderScrapedGparent,
      "/date_",
      today8Digit(),
      "/"
    )

  if (!dir.exists(outputFolderScrapedParent)) {
    dir.create(outputFolderScrapedParent)
  }

  # 07 pull in key:value data from Hemnet manually gathered in an excel ####

  areaMappings <-
    openxlsx::read.xlsx(
      xlsxFile = mappingsXlsx,
      sheet = "usedMappings"
    )

  # 08 create string for location search on Hemnet ####

  hemnetAreaNumber <-
    paste(areaMappings[["hemnetNumber"]],
      collapse = "&location_ids%5B%5D="
    )

  # 09 Initialize parameters for first hemnet "base" page ####

  housingType <- "bostadsratt"
  minRooms <- 1
  maxAvgift <- 10000
  firstMinPrice <- 1000000
  currentMinPrice <- firstMinPrice
  finalMaxPrice <- 15500000
  currentMaxPrice <- finalMaxPrice
  daysToLookBack <- maxDaysBackToLook
  sortingFeature <- "sale_date"
  sortingDirection <- "desc"
  hemnetBasePageNumber <- 1

  repeat {

    # 10 Scrape all pages from a given hemnet "base" page ####

    scrapeManyFromHemnetBase()

    # 11 After scraping, stop scraping if max price is too high ####

    if (currentMaxPrice >= finalMaxPrice) {
      break

    # 12 If max price not too high, re-establish prices for new "base" page ####
    } else {
      currentMinPrice <<- currentMaxPrice + 1
      currentMaxPrice <<- finalMaxPrice
      hemnetBasePageNumber <<- 1
    }
  }
}
