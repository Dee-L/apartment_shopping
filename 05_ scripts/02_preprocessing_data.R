source("D:/Coding/R/Public/pkgs_and_fxns/load_pkgs_and_fxns.R")

setwd("D:/Coding/R/Apartment shopping/script output/")

scraped_data <-
  read.csv(file = "01 scraped/Sold_on_Hemnet 2019-07-30.csv", header = T)

working_data <- scraped_data

###Data cleaning



##price


my_summary(working_data, "price")

cat("The scraping was meant to exclude all under 1m SEK.\n\n")

#Find the cases
extreme_prices <-
  working_data["price"] %>%
  subset(price < 1000000) %>%
  .[["price"]]

#Examine the cases
working_data %>%
  .[.[["price"]] %in% extreme_prices, ]

cat("I will replace these values with NA in case the other data is correct.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["price"]] <-
  ifelse(working_data[["price"]] %in% extreme_prices,
         as.numeric(NA),
         working_data[["price"]])

#Re-examine
summary(working_data_restore_point[["price"]])

my_summary(working_data, "price")


##asking_price


my_summary(working_data, "asking_price")

cat("I think some of the very low asking prices were probably keyed in wrong by the agent.\n\n")

#Find the cases
extreme_asking_prices <-
  working_data["asking_price"] %>%
  subset(asking_price < 800000) %>%
  .[["asking_price"]]

#Examine the cases
working_data %>%
  .[.[["asking_price"]] %in% extreme_asking_prices, ]

cat("I will replace these values with NA in case the other data is correct.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["asking_price"]] <-
  ifelse(working_data[["asking_price"]] %in% extreme_asking_prices,
         as.numeric(NA),
         working_data[["asking_price"]])

#Re-examine
summary(working_data_restore_point[["asking_price"]])

my_summary(working_data, "asking_price")


##avgift


my_summary(working_data, "avgift")

cat("There are some low and NA avgifts to examine.\n\n")

#Find the cases
low_avgift <-
  working_data["avgift"] %>%
  subset(avgift == 0 | is.na(avgift)) %>%
  .[["avgift"]]

#Examine the cases
working_data %>%
  .[.[["avgift"]] %in% low_avgift, ]

cat("I will replace these values with NA in case the other data is correct.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["avgift"]] <-
  ifelse(working_data[["avgift"]] %in% low_avgift,
         as.numeric(NA),
         working_data[["avgift"]])

#Re-examine
summary(working_data_restore_point[["avgift"]])

my_summary(working_data, "avgift")


##running_costs


my_summary(working_data, "running_costs")

cat("There are some low and high running_costs to examine.\n\n")

#Find the cases
extreme_running_costs <-
  working_data["running_costs"] %>%
  subset(running_costs > 30000) %>%
  .[["running_costs"]]

#Examine the cases
working_data %>%
  .[.[["running_costs"]] %in% extreme_running_costs, ]

cat("I will replace these values with NA in case the other data is correct.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["running_costs"]] <-
  ifelse(working_data[["running_costs"]] %in% extreme_running_costs,
         as.numeric(NA),
         working_data[["running_costs"]])

#Re-examine
summary(working_data_restore_point[["running_costs"]])

my_summary(working_data, "running_costs")


##city


my_summary(working_data, "city")

cat("I have over 1000 datapoints from each city.\n\n")


##year_sold


my_summary(working_data, "year_sold")

cat("I have over 200 datapoints from each year_sold.\n\n")


##month_sold_swedish


my_summary(working_data, "month_sold_swedish")

cat("I have over 400 datapoints from each month_sold_swedish.\n\n")


##day_of_month_sold


my_summary(working_data, "day_of_month_sold")

cat("I have over 100 datapoints from each day_of_month_sold.\n\n")


##year_built

my_summary(working_data, "year_built", 10)

cat("Some samples have incorrect 'year_built' info. Some probably entered as 0 by the agent. Others may be scraping problems or simply incorrect info (how likely is it that an apartment was built in 1400?)\n\n")

#Find the cases
year_built_extreme <-
  working_data["year_built"] %>%
  subset(year_built < 1850 | year_built > 2019 | year_built == 0) %>%
  .[["year_built"]]

#Examine the cases
working_data %>%
  .[.[["year_built"]] %in% year_built_extreme, ]

cat("Assuming the other data is correct, I will replace these values with NA.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["year_built"]] <-
  ifelse(working_data[["year_built"]] %in% year_built_extreme,
         as.numeric(NA),
         working_data[["year_built"]])


#Re-examine
summary(working_data_restore_point[["year_built"]])

my_summary(working_data, "year_built")


##type


my_summary(working_data, "type")

cat("The scraping pulled out more than just apartments.\n\n")


#Find the cases
type_to_keep <- "LägenhetLägenhet"

#Examine the cases
working_data %>%
  .[.[["type"]] %not_in% type_to_keep, ]

cat("Assuming the other data are correct, I will replace these with 'apartment'.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["type"]] %<>% as.character

working_data[["type"]] <-
  ifelse(working_data[["type"]] %in% type_to_keep,
         "apartment",
         as.character(NA))

working_data[["type"]] %<>% as.factor

#Re-examine
summary(working_data_restore_point[["type"]])

my_summary(working_data, "type")


##floor


my_summary(working_data, "floor")

cat("Considering no residence in Stockholm is higher than the 40th floor, there are some extreme values.\n\n")

#Find the cases
floor_too_high <-
  subset(working_data, floor > 40) %>%
  .[ , "floor"] %>%
  unique

#Examine the cases
working_data %>%
  .[.[["floor"]] %in% floor_too_high, ]

cat("Since these numbers must not be correct, I'll presume the agent hit too many keys while entering the data and I'll simply take the median of the digits.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["floor"]] <-
  ifelse(working_data[["floor"]] %in% floor_too_high,
         median(c(
           working_data[["floor"]] %>% as.character %>% left(1) %>% as.numeric,
           working_data[["floor"]] %>% as.character %>% right(1) %>% as.numeric
         )) %>% round,
         working_data[["floor"]])

#Re-examine
summary(working_data_restore_point[["floor"]])

my_summary(working_data, "floor")


##rooms


my_summary(working_data, "rooms")

cat("There is a single data point with more than 4 rooms.\n\n")


##kvm

my_summary(working_data, "kvm")

cat("There are a few apartments with very low or very high kvm.\n\n")


##area


my_summary(working_data, "area")

cat("Most of the areas show up in my data set less than 20 times.\n\n")

#Find the cases
working_data[["area"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

areas_with_low_representation <-
  sqldf("select area, count(*) as count
        from working_data
        group by area
        having count(*) < 20") %>%
  .[ , "area"] %>%
  droplevels

#Examine the cases
working_data %>%
  .[.[["area"]] %in% areas_with_low_representation, ]

cat("These data are probably okay, but since I do not have at least 20 samples in any of these areas points, I will convert them to a larger geographical region based on a mapping in an excel file I made. This will help prevent my model from overfitting.\n\n")

write_clip(areas_with_low_representation)

#Fix the cases
create_object_restore(working_data)

mapping_table <- read.xlsx("../source data/Mappings.xlsx", sheet = "areas")

working_data[["area"]] <-
  mapping_table[["area_mapped_to_larger_region"]][
    match(working_data[["area"]],
          mapping_table[["scraped"]])]

working_data[["area"]] %<>% as.factor %>% droplevels

#Re-examine
summary(working_data_restore_point[["area"]])

my_summary(working_data, "area")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
areas_with_low_representation <-
  sqldf("select area, count(*) as count
        from working_data
        where area != '<NA>'
        group by area
        order by count desc") %>%
  .[51 : nrow(.) , "area"]

#Examine the cases
working_data %>%
  .[.[["area"]] %in% areas_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'area' with 'city' for the current cases that have counts lower than the top 50 unique 'areas'.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["area"]] %<>% as.character

working_data[["area"]] <-
  ifelse(working_data[["area"]] %in% areas_with_low_representation,
         working_data[["city"]] %>% tolower,
         working_data[["area"]])

working_data[["area"]] %<>% as.factor %>% droplevels

#Re-examine
summary(working_data_restore_point[["area"]])

my_summary(working_data, "area")


##hoa


my_summary(working_data, "hoa")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
working_data[["hoa"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

hoas_with_low_representation <-
  sqldf("select hoa, count(*) as count
        from working_data
        where hoa != '<NA>'
        group by hoa
        order by count desc") %>%
  .[50 : nrow(.) , "hoa"]

#Examine the cases
working_data %>%
  .[.[["hoa"]] %in% hoas_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'hoa' with 'other' for the current cases that have counts lower than the top 50 unique 'hoas'.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["hoa"]] %<>% as.character

working_data[["hoa"]] <-
  ifelse(working_data[["hoa"]] %in% hoas_with_low_representation,
         "other",
         working_data[["hoa"]])

working_data[["hoa"]] %<>% as.factor %>% droplevels

#Re-examine
summary(working_data_restore_point[["hoa"]])

my_summary(working_data, "hoa")


##agent_name


my_summary(working_data, "agent_name")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
working_data[["agent_name"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  gsub("emailprotected", as.character(NA), .) %>%
  as.factor

agent_names_with_low_representation <-
  sqldf("select agent_name, count(*) as count
        from working_data
        where agent_name != '<NA>'
        group by agent_name
        order by count desc") %>%
  .[50 : nrow(.) , "agent_name"] %>%
  .[complete.cases(.)]

#Examine the cases
working_data %>%
  .[.[["agent_name"]] %in% agent_names_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'agent_name' with 'other' for the current cases that have counts lower than the top 50 unique 'agent_names'.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["agent_name"]] %<>% as.character

working_data[["agent_name"]] <-
  ifelse(working_data[["agent_name"]] %in% agent_names_with_low_representation,
         "other",
         working_data[["agent_name"]])

working_data[["agent_name"]] %<>% as.factor %>% droplevels

#Re-examine
summary(working_data_restore_point[["agent_name"]])

my_summary(working_data, "agent_name")


##agency


my_summary(working_data, "agency")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
working_data[["agency"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

agencys_with_low_representation <-
  sqldf("select agency, count(*) as count
        from working_data
        group by agency
        order by count desc") %>%
  .[50 : nrow(.) , "agency"]

#Examine the cases
working_data %>%
  .[.[["agency"]] %in% agencys_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'agency' with 'other' for the current cases that have counts lower than the top 50 unique 'agencys'.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["agency"]] %<>% as.character

working_data[["agency"]] <-
  ifelse(working_data[["agency"]] %in% agencys_with_low_representation,
         "other",
         working_data[["agency"]])

working_data[["agency"]] %<>% as.factor %>% droplevels

#Re-examine
summary(working_data_restore_point[["agency"]])

my_summary(working_data, "agency")


##street


my_summary(working_data, "street")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
working_data[["street"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

streets_with_low_representation <-
  sqldf("select street, count(*) as count
        from working_data
        where street != '<NA>'
        group by street
        order by count desc") %>%
  .[50 : nrow(.) , "street"]

#Examine the cases
working_data %>%
  .[.[["street"]] %in% streets_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'street' with 'other' for the current cases that have counts lower than the top 50 unique 'streets'.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["street"]] %<>% as.character

working_data[["street"]] <-
  ifelse(working_data[["street"]] %in% streets_with_low_representation,
         "other",
         working_data[["street"]])

working_data[["street"]] %<>% as.factor %>% droplevels

#Re-examine
summary(working_data_restore_point[["street"]])

my_summary(working_data, "street")


##address


my_summary(working_data, "address")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
working_data[["address"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

addresss_with_low_representation <-
  sqldf("select address, count(*) as count
        from working_data
        where address != '<NA>'
        group by address
        order by count desc") %>%
  .[50 : nrow(.) , "address"]

#Examine the cases
working_data %>%
  .[.[["address"]] %in% addresss_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'address' with 'other' for the current cases that have counts lower than the top 50 unique 'addresss'.\n\n")

#Fix the cases
create_object_restore(working_data)

working_data[["address"]] %<>% as.character

working_data[["address"]] <-
  ifelse(working_data[["address"]] %in% addresss_with_low_representation,
         "other",
         working_data[["address"]])

working_data[["address"]] %<>% as.factor %>% droplevels

#Re-examine
summary(working_data_restore_point[["address"]])

my_summary(working_data, "address")



###Saving data

dir.create("02 preprocessed")

write.csv(x = working_data,
          file = paste0("02 preprocessed/", now() %>% as.Date, ".csv"),
          row.names = F)