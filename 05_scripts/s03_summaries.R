# Purpose: Look at summaries of data and further pre=processing
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this scripts are installed ####
pkgs <-
    c()

install_my_pkgs(pkgs)

# 02 load latest compiled data ####

compiled_data <- 
  paste0(
    output_folder_compiled
    , list.files(output_folder_compiled) %>%
      .[length(.)]) %>%
    readRDS

# 03 specify where to save capture of preprocessing QA ####
output_folder_summaries <<-
paste0(
    output_folder,
    "03_summaries/"
)

if (!dir.exists(output_folder_summaries)) {
dir.create(output_folder_summaries)
}

# 03 Check if need to make new summaries ####

latest_compiled_results <-
    output_folder_compiled %>%
    list.files %>%
    .[length(.)] %>%
    gsub(".rds", "", .) %>%
    right(8) %>%
    as.numeric
    
latest_summaries <-
    output_folder_summaries %>%
    list.files %>%
    .[length(.)] %>%
    gsub(".rds", "", .) %>%
    right(8) %>%
    as.numeric

# 04 Only proceed if compiled results are older than scraped results ####
if (latest_summaries < latest_compiled_results) {

  # 05 Start logging ####
  log_name <-
    paste0(
      output_folder_summaries
      , "date_"
      , today_8digit()
      , ".md"
    )

  sink(log_name)

  # 06 Generate summaries ####
  for (column_name in names(compiled_data)) {
  my_summary(compiled_data, column_name, 10)
  }

  # 07 Stop logging

  sink()

}


# 08 Likely can delete most of what is below ####



# 04

cat("The scraping was meant to exclude all under 1m SEK.\n\n")

#Find the cases
extreme_prices <-
  compiled_data["price"] %>%
  subset(price < 1000000) %>%
  .[["price"]]

#Examine the cases
compiled_data %>%
  .[.[["price"]] %in% extreme_prices, ]

cat("I will replace these values with NA in case the other data is correct.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["price"]] <-
  ifelse(compiled_data[["price"]] %in% extreme_prices,
         as.numeric(NA),
         compiled_data[["price"]])

#Re-examine
summary(compiled_data_restore_point[["price"]])

my_summary(compiled_data, "price")


##asking_price


my_summary(compiled_data, "asking_price")

cat("I think some of the very low asking prices were probably keyed in wrong by the agent.\n\n")

#Find the cases
extreme_asking_prices <-
  compiled_data["asking_price"] %>%
  subset(asking_price < 800000) %>%
  .[["asking_price"]]

#Examine the cases
compiled_data %>%
  .[.[["asking_price"]] %in% extreme_asking_prices, ]

cat("I will replace these values with NA in case the other data is correct.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["asking_price"]] <-
  ifelse(compiled_data[["asking_price"]] %in% extreme_asking_prices,
         as.numeric(NA),
         compiled_data[["asking_price"]])

#Re-examine
summary(compiled_data_restore_point[["asking_price"]])

my_summary(compiled_data, "asking_price")


##avgift


my_summary(compiled_data, "avgift")

cat("There are some low and NA avgifts to examine.\n\n")

#Find the cases
low_avgift <-
  compiled_data["avgift"] %>%
  subset(avgift == 0 | is.na(avgift)) %>%
  .[["avgift"]]

#Examine the cases
compiled_data %>%
  .[.[["avgift"]] %in% low_avgift, ]

cat("I will replace these values with NA in case the other data is correct.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["avgift"]] <-
  ifelse(compiled_data[["avgift"]] %in% low_avgift,
         as.numeric(NA),
         compiled_data[["avgift"]])

#Re-examine
summary(compiled_data_restore_point[["avgift"]])

my_summary(compiled_data, "avgift")


##running_costs


my_summary(compiled_data, "running_costs")

cat("There are some low and high running_costs to examine.\n\n")

#Find the cases
extreme_running_costs <-
  compiled_data["running_costs"] %>%
  subset(running_costs > 30000) %>%
  .[["running_costs"]]

#Examine the cases
compiled_data %>%
  .[.[["running_costs"]] %in% extreme_running_costs, ]

cat("I will replace these values with NA in case the other data is correct.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["running_costs"]] <-
  ifelse(compiled_data[["running_costs"]] %in% extreme_running_costs,
         as.numeric(NA),
         compiled_data[["running_costs"]])

#Re-examine
summary(compiled_data_restore_point[["running_costs"]])

my_summary(compiled_data, "running_costs")


##city


my_summary(compiled_data, "city")

cat("I have over 1000 datapoints from each city.\n\n")


##year_sold


my_summary(compiled_data, "year_sold")

cat("I have over 200 datapoints from each year_sold.\n\n")


##month_sold_swedish


my_summary(compiled_data, "month_sold_swedish")

cat("I have over 400 datapoints from each month_sold_swedish.\n\n")


##day_of_month_sold


my_summary(compiled_data, "day_of_month_sold")

cat("I have over 100 datapoints from each day_of_month_sold.\n\n")


##year_built

my_summary(compiled_data, "year_built", 10)

cat("Some samples have incorrect 'year_built' info. Some probably entered as 0 by the agent. Others may be scraping problems or simply incorrect info (how likely is it that an apartment was built in 1400?)\n\n")

#Find the cases
year_built_extreme <-
  compiled_data["year_built"] %>%
  subset(year_built < 1850 | year_built > 2019 | year_built == 0) %>%
  .[["year_built"]]

#Examine the cases
compiled_data %>%
  .[.[["year_built"]] %in% year_built_extreme, ]

cat("Assuming the other data is correct, I will replace these values with NA.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["year_built"]] <-
  ifelse(compiled_data[["year_built"]] %in% year_built_extreme,
         as.numeric(NA),
         compiled_data[["year_built"]])


#Re-examine
summary(compiled_data_restore_point[["year_built"]])

my_summary(compiled_data, "year_built")


##type


my_summary(compiled_data, "type")

cat("The scraping pulled out more than just apartments.\n\n")


#Find the cases
type_to_keep <- "LägenhetLägenhet"

#Examine the cases
compiled_data %>%
  .[.[["type"]] %not_in% type_to_keep, ]

cat("Assuming the other data are correct, I will replace these with 'apartment'.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["type"]] %<>% as.character

compiled_data[["type"]] <-
  ifelse(compiled_data[["type"]] %in% type_to_keep,
         "apartment",
         as.character(NA))

compiled_data[["type"]] %<>% as.factor

#Re-examine
summary(compiled_data_restore_point[["type"]])

my_summary(compiled_data, "type")


##floor


my_summary(compiled_data, "floor")

cat("Considering no residence in Stockholm is higher than the 40th floor, there are some extreme values.\n\n")

#Find the cases
floor_too_high <-
  subset(compiled_data, floor > 40) %>%
  .[ , "floor"] %>%
  unique

#Examine the cases
compiled_data %>%
  .[.[["floor"]] %in% floor_too_high, ]

cat("Since these numbers must not be correct, I'll presume the agent hit too many keys while entering the data and I'll simply take the median of the digits.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["floor"]] <-
  ifelse(compiled_data[["floor"]] %in% floor_too_high,
         median(c(
           compiled_data[["floor"]] %>% as.character %>% left(1) %>% as.numeric,
           compiled_data[["floor"]] %>% as.character %>% right(1) %>% as.numeric
         )) %>% round,
         compiled_data[["floor"]])

#Re-examine
summary(compiled_data_restore_point[["floor"]])

my_summary(compiled_data, "floor")


##rooms


my_summary(compiled_data, "rooms")

cat("There is a single data point with more than 4 rooms.\n\n")


##kvm

my_summary(compiled_data, "kvm")

cat("There are a few apartments with very low or very high kvm.\n\n")


##area


my_summary(compiled_data, "area")

cat("Most of the areas show up in my data set less than 20 times.\n\n")

#Find the cases
compiled_data[["area"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

areas_with_low_representation <-
  sqldf("select area, count(*) as count
        from compiled_data
        group by area
        having count(*) < 20") %>%
  .[ , "area"] %>%
  droplevels

#Examine the cases
compiled_data %>%
  .[.[["area"]] %in% areas_with_low_representation, ]

cat("These data are probably okay, but since I do not have at least 20 samples in any of these areas points, I will convert them to a larger geographical region based on a mapping in an excel file I made. This will help prevent my model from overfitting.\n\n")

write_clip(areas_with_low_representation)

#Fix the cases
create_object_restore(compiled_data)

mapping_table <- read.xlsx("../source data/Mappings.xlsx", sheet = "areas")

compiled_data[["area"]] <-
  mapping_table[["area_mapped_to_larger_region"]][
    match(compiled_data[["area"]],
          mapping_table[["scraped"]])]

compiled_data[["area"]] %<>% as.factor %>% droplevels

#Re-examine
summary(compiled_data_restore_point[["area"]])

my_summary(compiled_data, "area")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
areas_with_low_representation <-
  sqldf("select area, count(*) as count
        from compiled_data
        where area != '<NA>'
        group by area
        order by count desc") %>%
  .[51 : nrow(.) , "area"]

#Examine the cases
compiled_data %>%
  .[.[["area"]] %in% areas_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'area' with 'city' for the current cases that have counts lower than the top 50 unique 'areas'.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["area"]] %<>% as.character

compiled_data[["area"]] <-
  ifelse(compiled_data[["area"]] %in% areas_with_low_representation,
         compiled_data[["city"]] %>% tolower,
         compiled_data[["area"]])

compiled_data[["area"]] %<>% as.factor %>% droplevels

#Re-examine
summary(compiled_data_restore_point[["area"]])

my_summary(compiled_data, "area")


##hoa


my_summary(compiled_data, "hoa")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
compiled_data[["hoa"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

hoas_with_low_representation <-
  sqldf("select hoa, count(*) as count
        from compiled_data
        where hoa != '<NA>'
        group by hoa
        order by count desc") %>%
  .[50 : nrow(.) , "hoa"]

#Examine the cases
compiled_data %>%
  .[.[["hoa"]] %in% hoas_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'hoa' with 'other' for the current cases that have counts lower than the top 50 unique 'hoas'.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["hoa"]] %<>% as.character

compiled_data[["hoa"]] <-
  ifelse(compiled_data[["hoa"]] %in% hoas_with_low_representation,
         "other",
         compiled_data[["hoa"]])

compiled_data[["hoa"]] %<>% as.factor %>% droplevels

#Re-examine
summary(compiled_data_restore_point[["hoa"]])

my_summary(compiled_data, "hoa")


##agent_name


my_summary(compiled_data, "agent_name")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
compiled_data[["agent_name"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  gsub("emailprotected", as.character(NA), .) %>%
  as.factor

agent_names_with_low_representation <-
  sqldf("select agent_name, count(*) as count
        from compiled_data
        where agent_name != '<NA>'
        group by agent_name
        order by count desc") %>%
  .[50 : nrow(.) , "agent_name"] %>%
  .[complete.cases(.)]

#Examine the cases
compiled_data %>%
  .[.[["agent_name"]] %in% agent_names_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'agent_name' with 'other' for the current cases that have counts lower than the top 50 unique 'agent_names'.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["agent_name"]] %<>% as.character

compiled_data[["agent_name"]] <-
  ifelse(compiled_data[["agent_name"]] %in% agent_names_with_low_representation,
         "other",
         compiled_data[["agent_name"]])

compiled_data[["agent_name"]] %<>% as.factor %>% droplevels

#Re-examine
summary(compiled_data_restore_point[["agent_name"]])

my_summary(compiled_data, "agent_name")


##agency


my_summary(compiled_data, "agency")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
compiled_data[["agency"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

agencys_with_low_representation <-
  sqldf("select agency, count(*) as count
        from compiled_data
        group by agency
        order by count desc") %>%
  .[50 : nrow(.) , "agency"]

#Examine the cases
compiled_data %>%
  .[.[["agency"]] %in% agencys_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'agency' with 'other' for the current cases that have counts lower than the top 50 unique 'agencys'.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["agency"]] %<>% as.character

compiled_data[["agency"]] <-
  ifelse(compiled_data[["agency"]] %in% agencys_with_low_representation,
         "other",
         compiled_data[["agency"]])

compiled_data[["agency"]] %<>% as.factor %>% droplevels

#Re-examine
summary(compiled_data_restore_point[["agency"]])

my_summary(compiled_data, "agency")


##street


my_summary(compiled_data, "street")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
compiled_data[["street"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

streets_with_low_representation <-
  sqldf("select street, count(*) as count
        from compiled_data
        where street != '<NA>'
        group by street
        order by count desc") %>%
  .[50 : nrow(.) , "street"]

#Examine the cases
compiled_data %>%
  .[.[["street"]] %in% streets_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'street' with 'other' for the current cases that have counts lower than the top 50 unique 'streets'.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["street"]] %<>% as.character

compiled_data[["street"]] <-
  ifelse(compiled_data[["street"]] %in% streets_with_low_representation,
         "other",
         compiled_data[["street"]])

compiled_data[["street"]] %<>% as.factor %>% droplevels

#Re-examine
summary(compiled_data_restore_point[["street"]])

my_summary(compiled_data, "street")


##address


my_summary(compiled_data, "address")

cat("For imputation later, I need 50 or fewer unique levels (not counting NA's, which can later be imputed), but I currently have more.\n\n")

#Find the cases
compiled_data[["address"]] %<>%
  as.character %>%
  tolower %>%
  gsub("[^[:alnum:] ]| ", "", .) %>%
  gsub("\\s+", " ", .) %>%
  as.factor

addresss_with_low_representation <-
  sqldf("select address, count(*) as count
        from compiled_data
        where address != '<NA>'
        group by address
        order by count desc") %>%
  .[50 : nrow(.) , "address"]

#Examine the cases
compiled_data %>%
  .[.[["address"]] %in% addresss_with_low_representation, ]

cat("These data are probably okay, but since I need to have 50 or fewer unique levels for imputation later, I will replace 'address' with 'other' for the current cases that have counts lower than the top 50 unique 'addresss'.\n\n")

#Fix the cases
create_object_restore(compiled_data)

compiled_data[["address"]] %<>% as.character

compiled_data[["address"]] <-
  ifelse(compiled_data[["address"]] %in% addresss_with_low_representation,
         "other",
         compiled_data[["address"]])

compiled_data[["address"]] %<>% as.factor %>% droplevels

#Re-examine
summary(compiled_data_restore_point[["address"]])

my_summary(compiled_data, "address")



###Saving data

dir.create("02 preprocessed")

write.csv(x = compiled_data,
          file = paste0("02 preprocessed/", now() %>% as.Date, ".csv"),
          row.names = F)