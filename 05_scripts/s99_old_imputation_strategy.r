# If I impute, must first leave out final_price so that it is not used
# to predict what I'm imputing

# For imputing missing values:
# https://towardsdatascience.com/how-to-handle-missing-data-8646b18db0d4
# If you think data are missing not at random (MNAR), then imputation
# instead of case deletion is preferred so as not to bias the model
# For categorical variables, it's probably best just to assign them to own/new
# category "missing/unknown"

# I will use a simple version of imputing: sample the known data points,
# and pick from those at random

# Prepare for making max n_iterations of random forests to impute missing data
n_iterations <-
    sapply(compiled_data, function(x) {
        is.na(x) %>% sum()
    }) %>%
    max(.) / nrow(compiled_data) * 100

n_iterations %<>% ceiling

# Impute missing data
missForest_object <-
    missForest(xmis = compiled_data, maxiter = n_iterations)

# Store the imputed values
compiled_data_imputed <- missForest_object$ximp

# Round numerical data where needed
cols_to_make_integers <-
    c("street_number", "price", "day_of_month_sold", "year_sold", "asking_price", "avgift", "floor", "running_costs", "year_built")

for (col in cols_to_make_integers) {
    compiled_data_imputed[[paste0(col)]] %<>%
        round %>%
        as.integer()
}

cols_to_round_to_1_dig_past_decimal <-
    c("kvm")

for (col in cols_to_round_to_1_dig_past_decimal) {
    compiled_data_imputed[[paste0(col)]] %<>%
        round(1)
}

cols_to_round_to_nearest_0.5 <-
    c("rooms")

for (col in cols_to_round_to_nearest_0.5) {
    compiled_data_imputed[[paste0(col)]] %<>%
        sapply(FUN = function(x) round(x / 0.5) * 0.5)
}