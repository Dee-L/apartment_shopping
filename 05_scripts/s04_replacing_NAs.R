source("D:/Coding/R/Public/pkgs_and_fxns/load_pkgs_and_fxns.R")

setwd("D:/Coding/R/Apartment shopping/script output/")

pre_processed <-
  read.csv(file = "02 preprocessed/2019-08-19.csv", header = T)

working_data <- pre_processed

#Prepare for making max n_iterations of random forests to impute missing data
n_iterations <- 
  sapply(working_data, function(x) {is.na(x) %>% sum}) %>%
  max(.) / nrow(working_data) * 100

n_iterations %<>% ceiling

#Impute missing data
missForest_object <-
  missForest(xmis = working_data, maxiter = n_iterations)

#Store the imputed values
working_data_imputed <- missForest_object$ximp

#Round numerical data where needed
cols_to_make_integers <-
  c("street_number", "price", "day_of_month_sold", "year_sold", "asking_price", "avgift", "floor", "running_costs", "year_built")

for (col in cols_to_make_integers) {
  working_data_imputed[[paste0(col)]] %<>%
    round %>%
    as.integer
}

cols_to_round_to_1_dig_past_decimal <-
  c("kvm")

for (col in cols_to_round_to_1_dig_past_decimal) {
  working_data_imputed[[paste0(col)]] %<>%
    round(1) 
}

cols_to_round_to_nearest_0.5 <-
  c("rooms")

for (col in cols_to_round_to_nearest_0.5) {
  working_data_imputed[[paste0(col)]] %<>%
    sapply(FUN =  function(x) round(x/0.5)*0.5)
}

dir.create("03 NAs replaced")

write.csv(x = working_data_imputed,
          file = paste0("03 NAs replaced/", now() %>% as.Date, ".csv"),
          row.names = F)