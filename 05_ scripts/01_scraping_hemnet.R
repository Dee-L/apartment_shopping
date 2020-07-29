source("D:/Coding/R/Public/pkgs_and_fxns/load_pkgs_and_fxns.R")

setwd("../../Apartment shopping/script output/") #setwd

dir.create("01 scraped")

#initialize df to populate via webscraping
sold_properties <-
  data.frame(
    city = character(),
    area = character(),
    street = character(),
    street_number = numeric(),
    address = character(),
    rooms = numeric(),
    kvm = numeric(),
    price = numeric(),
    day_of_month_sold = character(),
    month_sold_swedish = character(),
    year_sold = character(),
    asking_price = numeric(),
    avgift = numeric(),
    type = character(),
    floor = numeric(),
    running_costs = numeric(),
    year_built = numeric(),
    hoa = character(),
    agent_name = character(),
    agency = character()
  )

write.csv(x = sold_properties,
          file = paste0("01 scraped/Sold_on_Hemnet ", now() %>% as.Date, ".csv"),
          row.names = F)

#Pull in data for area names and numbers for creating URLs to scrape
area_mappings <- #create df for looking up cc values
  read.xlsx("../source data/Area Mappings.xlsx", sheet = "used_mappings")

#extract from excel file df column, paste with comma separation
Hemnet_area_number <-
  paste(area_mappings[["Hemnet_number"]] %>% unique, collapse = "&location_ids%5B%5D=")

#Initialize variables for scraping
type <- "bostadsratt"
minRooms <- 2
maxAvgift <- 5000
minPrice <- 1000000
maxPrice <- 2500000
years_to_look_back <- 10
sorting_feature <- "sale_date"
sorting_direction <- "desc"
first_page <- 1


Hemnet_base_address <-
  paste0(
    "https://www.hemnet.se/salda/bostader?",
    "location_ids%5B%5D=",
    Hemnet_area_number,
    "&item_types%5B%5D=",
    type,
    "&rooms_min=",
    minRooms,
    "&fee_max=",
    maxAvgift,
    "&selling_price_min=",
    minPrice,
    "&selling_price_max=",
    maxPrice,
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

#Create html session for navigating and scraping
session <-
  Hemnet_base_address %>%
  html_session

#Calculate the number of pages from the number of
#objects returned in the query
total_pages <-
  session %>%
  read_html %>%
  html_nodes("#result .clear-children .centered") %>%
  html_text %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("Visar150av", "", .) %>%
  as.numeric

total_pages <-
  ceiling(total_pages / 50) %>% ceiling

for (page in 1 : total_pages) {
  #update base_address
  Hemnet_base_address <-
    paste0(
      "https://www.hemnet.se/salda/bostader?by=sale_date&fee_max=",
      maxAvgift,
      "&item_types%5B%5D=bostadsratt&item_types%5B%5D=radhus&item_types%5B%5D=villa&location_ids%5B%5D=",
      Hemnet_area_number,
      "&new_construction=include&order=desc&page=",
      page,
      "&selling_price_max=",
      maxPrice,
      "&objectType=&sold_age=",
      years_to_look_back,
      "y"
    )
  
  #update session
  session <-
    Hemnet_base_address %>%
    html_session
  
  print(
    paste0(
      "Hemnet base page is ",
      page,
      " of ",
      total_pages,
      "."))
  
  #count the number of links to scrape from
  pages_to_scrape <-
    session %>%
    html_nodes(".item-link-container") %>%
    html_attr("href")
  
  print(
    paste0(
      length(pages_to_scrape),
      " pages to scrape from this base page."))
  
  for (page_to_scrape in 1 : length(pages_to_scrape)){
    print(paste0(page_to_scrape, " of ", length(pages_to_scrape), " pages on this base page."))
    try_wait_retry({
    #update session
    session <-
      pages_to_scrape[page_to_scrape] %>%
      html_session
    
    #Scraping data
    
    #Address data from the header
    address <-
      try_2_expressions(
        expr1 = {
          session %>%
            read_html %>%
            html_nodes(
              ".sold-property__address") %>%
            html_text %>%
            gsub("[^[:alnum:] ]", "", .) %>%
            gsub("  Slutpris  ", "", .)
        },
        expr2 = NA)
    
    street_number <-
      try_2_expressions(
        expr1 = {
          regmatches(address,
                     gregexpr(
                       "[[:digit:]]+",
                       address)) %>%
            .[[1]] %>%
            .[1] %>%
            as.numeric
        },
        expr2 = NA)
    
    street <-
      try_2_expressions(
        expr1 = {
          substr(address,
                 1,
                 gregexpr(
                   as.character(street_number),
                   address)[[1]][1] - 2)
          },
        expr2 = NA)
    
    apt_floor_signifier <-
      substr(address,
             nchar(street) +
               nchar(street_number) +
               3,
             nchar(address))
    
    if(grepl("av", apt_floor_signifier)) {
      apt_floor_signifier %<>%
      substr(.,
             1,
             gregexpr(
               "av",
               .)[[1]][1] - 1)
    }
    
    floor <-
      try_2_expressions(
        expr1 = {
          case_when(
            (grepl("vå", apt_floor_signifier) |
               grepl("Vå", apt_floor_signifier) |
               grepl("tr", apt_floor_signifier)) ~ 
              regmatches(apt_floor_signifier,
                         gregexpr(
                           "[[:digit:]]+",
                           apt_floor_signifier)) %>%
              as.numeric,
            grepl("bv", apt_floor_signifier) ~ 0,
            TRUE ~ as.numeric(NA)
          )
        },
        expr2 = NA)
    
    #Metadata from the top of the page
    metadata_vector <-
      session %>%
      read_html %>%
      html_nodes(".sold-property__metadata") %>%
      html_text %>%
      # gsub("[^[:alnum:] ]| -|,", "", .) %>%
      gsub("\\s+", " ", .) %>%
      gsub(",", "", .) %>%
      strsplit(., " ") %>%
      .[[1]]
    
    type <-
      try_2_expressions(
        expr1 = {
          metadata_vector[2]
        },
        expr2 = NA)
    
    area <-
      try_2_expressions(
        expr1 = {
          metadata_vector[
            which(metadata_vector == "-")[1] + 1]
        },
        expr2 = NA)
    
    city <-
      try_2_expressions(
        expr1 = {
          metadata_vector[
            which(metadata_vector == "kommun") - 1]
        },
        expr2 = NA)
    
    #time from the top of the page
    time_vector <-
      session %>%
      read_html %>%
      html_nodes("time") %>%
      html_text %>%
      gsub("[^[:alnum:] ]| -|,", "", .) %>%
      gsub("\\s+", " ", .) %>%
      strsplit(., " ") %>%
      .[[1]]
    
    day_of_month_sold <-
      try_2_expressions(
        expr1 = {
          time_vector[2]
        },
        expr2 = NA)
    
    month_sold_swedish <-
      try_2_expressions(
        expr1 = {
          time_vector[3]
        },
        expr2 = NA)
    
    year_sold <-
      try_2_expressions(
        expr1 = {
          time_vector[4]
        },
        expr2 = NA)
    
    #Sales price from middle of the page
    price <-
      try_2_expressions(
        expr1 = {
          session %>%
            read_html %>%
            html_nodes(
              ".sold-property__price-value") %>%
            html_text %>%
            gsub("[^[:alnum:] ]| -|,", "", .) %>%
            gsub("\\s+", " ", .) %>%
            strsplit(., " ") %>%
            .[[1]] %>%
            .[1] %>%
            as.numeric
        },
        expr2 = NA)
    
    #Property price-stats from the middle of the page
    pricestats_vector <-
      session %>%
      read_html %>%
      html_nodes(".sold-property__price-stats") %>%
      html_text %>%
      gsub("[^[:alnum:] ]| -|,", "", .) %>%
      gsub("\\s+", " ", .) %>%
      strsplit(., " ") %>%
      .[[1]]
    
    asking_price <-
      try_2_expressions(
        expr1 = {
          pricestats_vector[
            which(pricestats_vector == "pris") + 1
          ] %>% as.numeric
        },
        expr2 = NA)
    
    #Property attributes from the middle of the page
    attributes_vector <-
      session %>%
      read_html %>%
      html_nodes(".sold-property__attributes") %>%
      html_text %>%
      gsub(",", ".", .) %>%
      strsplit(., "\\s+\\s+") %>%
      .[[1]]
    
    rooms <-
      try_2_expressions(
        expr1 = {
          attributes_vector[
            which(
              attributes_vector == "Antal rum") + 1
            ] %>%
            gsub(" rum", "", .) %>%
            as.numeric
          },
        expr2 = NA)
    
    kvm <-
      try_2_expressions(
        expr1 = {
          attributes_vector[
            which(attributes_vector == "Boarea") + 1
            ] %>%
            gsub(" m²", "", .) %>%
            as.numeric
        },
        expr2 = NA)
    
    avgift <-
      try_2_expressions(
        expr1 = {
          attributes_vector[
            which(attributes_vector ==
                    "Avgift/månad") + 1
            ] %>%
            gsub(" kr/mån", "", .) %>%
            gsub("\\s", "", .) %>%
            as.numeric
        },
        expr2 = NA)
    
    running_costs <-
      try_2_expressions(
        expr1 = {
          attributes_vector[
            which(attributes_vector ==
                    "Driftskostnad") + 1
            ] %>%
            gsub(" kr/år", "", .) %>%
            gsub("\\s", "", .) %>%
            as.numeric
        },
        expr2 = NA)

    year_built <-
      try_2_expressions(
        expr1 = {
          attributes_vector[
            which(attributes_vector == "Byggår") + 1
            ] %>% as.numeric
        },
        expr2 = NA)

    hoa <-
      try_2_expressions(
        expr1 = {
          attributes_vector[
            which(attributes_vector == "Förening") + 1
            ]
        },
        expr2 = NA)
    
    #Broker contact from the bottom of the page
    agent_name <-
      try_2_expressions(
        expr1 = {
          session %>%
            read_html %>%
            html_nodes(".broker-contact-card__information > p:nth-child(1)") %>%
            html_text %>%
            gsub("[^[:alnum:] ]| -|,", "", .) %>%
            gsub("\\s+", " ", .)
        },
        expr2 = NA)
    
    agency <-
      try_2_expressions(
        expr1 = {
          session %>%
            read_html %>%
            html_nodes(".broker-contact-card__information > p+ p") %>%
            html_text %>%
            gsub("[^[:alnum:] ]| -|,", "", .) %>%
            gsub("\\s+", " ", .)
        },
        expr2 = NA)
    
    temp_df <- data.frame(
      city = NA_if_empty(city),
      area = NA_if_empty(area),
      street = NA_if_empty(street),
      street_number = NA_if_empty(street_number),
      address = NA_if_empty(address),
      rooms = NA_if_empty(rooms),
      kvm = NA_if_empty(kvm),
      price = NA_if_empty(price),
      day_of_month_sold = NA_if_empty(day_of_month_sold),
      month_sold_swedish = NA_if_empty(month_sold_swedish),
      year_sold = NA_if_empty(year_sold),
      asking_price = NA_if_empty(asking_price),
      avgift = NA_if_empty(avgift),
      type = NA_if_empty(type),
      floor = NA_if_empty(floor),
      running_costs = NA_if_empty(running_costs),
      year_built = NA_if_empty(year_built),
      hoa = NA_if_empty(hoa),
      agent_name = NA_if_empty(agent_name),
      agency = NA_if_empty(agency)
    )
    
    sold_properties %<>%
      rbind(temp_df)
    
    write.csv(x = sold_properties,
              file = paste0("01 scraped/Sold_on_Hemnet ", now() %>% as.Date, ".csv"),
              row.names = F)
    })
  }
  
}



#Started building a way to scrape for Booli before
#realizing their site uses JS, so cannot easily
#scrape with read_html. Perhaps RSelenium and then rvest would be useful for converting the webpages into html for scraping?

#extract from excel file df column, paste with comma separation
# Booli_area <- paste(area_mappings[["Booli_name"]] %>% unique, collapse = ",") 
# Booli_area_number <- paste(area_mappings[["Booli_number"]] %>% unique, collapse = ",")
# 
# #Initialize variables for scraping
# core_address <- "https://www.booli.se/slutpriser/"
# maxPrice <- 2500000 
# minSoldDate <- "2010-01-01"
# objectType <- "L%C3%A4genhet%2CVilla%2CParhus%2CRadhus%2CKedjehus"
# page <- 1
# sort <- "soldDate"
# 
# base_address <-
#   paste0(core_address,
#          Booli_area, "/",
#          Booli_area_number, "/?maxRent=",
#          maxAvgift, "&maxSoldPrice=",
#          maxPrice, "&minSoldDate=",
#          minSoldDate, "&objectType=",
#          objectType, "&page=",
#          page, "&sort=",
#          sort)