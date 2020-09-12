# Started building a way to scrape for Booli before
# realizing their site uses JS, so cannot easily
# scrape with xml2::read_html. Perhaps RSelenium and then rvest would be useful for converting the webpages into html for scraping?

# extract from excel file df column, paste with comma separation
# Booli_area <- paste(area_mappings[["Booli_name"]] %>% unique, collapse = ",")
# Booli_area_number <- paste(area_mappings[["Booli_number"]] %>% unique, collapse = ",")
#
# #Initialize variables for scraping
# core_address <- "https://www.booli.se/slutpriser/"
# max_price <- 2500000
# minSoldDate <- "2010-01-01"
# objectType <- "L%C3%A4genhet%2CVilla%2CParhus%2CRadhus%2CKedjehus"
# page <- 1
# sort <- "soldDate"
#
# base_address <-
#   paste0(core_address,
#          Booli_area, "/",
#          Booli_area_number, "/?maxRent=",
#          max_avgift, "&maxSoldPrice=",
#          max_price, "&minSoldDate=",
#          minSoldDate, "&objectType=",
#          objectType, "&page=",
#          page, "&sort=",
#          sort)