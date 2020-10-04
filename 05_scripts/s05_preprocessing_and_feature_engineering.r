# Purpose: Preprocess and engineer new features
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
      "sqldf"
      , "dplyr"
      , "lubridate"
      , "RcppRoll"
      , "caret"
      )

install_my_pkgs(pkgs)

# 02 load latest compiled data ####

compiled_data <-
  paste0(
    output_folder_compiled
    , list.files(output_folder_compiled) %>%
      .[length(.)]) %>%
    readRDS

# 03 Renaming "final_price" column to "selling_price" to avoid confusion ####

if ("final_price" %in% names(compiled_data)) {
  index_for_final_price_column <- which(names(compiled_data) %in% "final_price")

  names_for_all_others <- names(compiled_data) %>% .[. %not_in% "final_price"]
  compiled_data[["selling_price"]] <- compiled_data[["final_price"]]

  compiled_data <- compiled_data[, c("selling_price", names_for_all_others)]

}

# 04 Gathering lists of variables ####
rawdata_variables <- colnames(compiled_data)

categorical_variables <-
  c(
    "city"
    , "area"
    , "street"
    , "agent_name"
    , "agency"
  )

for (variable in categorical_variables) {

  # 05 Specify new variable name and count missing ####
  new_var_name <- paste0(variable, "_missing_replaced")

  n_missing <-
    sum((compiled_data[[variable]] == "") | (is.na(compiled_data[[variable]])))

  cat("\nVariable is:", variable, "and n_missing is:", n_missing, "\n")

  # 06 If any missing, replace with "missing" ####
  if (n_missing > 0) {

    compiled_data[[new_var_name]] <-
      ifelse(
        (
          (compiled_data[[variable]] == "")
          | is.na(compiled_data[[variable]])
          )
        , "missing"
        , compiled_data[[variable]]
      )

    n_missing_new_variable <-
      sum((compiled_data[[new_var_name]] == "") |
      (is.na(compiled_data[[new_var_name]])))

    cat("\n\tNew variable is:", new_var_name
    , "and n_missing is:", n_missing_new_variable, "\n")

    # 07 Update categorical variables list ####
    categorical_variables[which(categorical_variables %in% variable)] <-
      new_var_name
  }
}

# 08 Initializing column to populate in the loop ####
compiled_data[["area_consolidated"]] <-
  compiled_data[["area_missing_replaced"]]

# 09 Repeat loop to consolidate areas ####
repeat {
  # 10 Getting the count of unique levels for area ####

  n_unique_areas <-
    compiled_data[["area_consolidated"]] %>%
    unique %>%
    length

  message(
    "\nNumber of unique areas: "
    , n_unique_areas
    , " Trying to consolidate.\n")

  # 11 Find the top 50 areas ####
  replacement_areas <-
    sqldf::sqldf(
      "select area_consolidated, count(*) as count
      from compiled_data
      group by area_consolidated
      order by count desc
      limit 100")

  # 12 Use top hit to try to consolidate lower frequency hits ####
  for (area in replacement_areas[[1]]) {

    # 13 Create areas_consolidated list ####
    if (!exists("areas_consolidated")) {
      areas_consolidated <- c()
    }

    # 14 Skip area if you've already consolidated to it ####
    if (area %in% areas_consolidated) {
      cat("\nArea ", area, "already consolidated. Skipping.\n")
      next
    } else {
      n_area_consolidated <- sum(compiled_data[["area_consolidated"]] == area)

      cat(
        "\nArea is:", area
        , "\n\tn before consolidation:", n_area_consolidated, "\n"
        )

      # 15 Consolidate if text matches, otherwise leave alone ####
      compiled_data[["area_consolidated"]] <-
        ifelse(
          (
            grepl(area, compiled_data[["area_missing_replaced"]]) &
            (compiled_data[["area_consolidated"]] ==
              compiled_data[["area_missing_replaced"]])
          )
          ,
          area,
          compiled_data[["area_consolidated"]]
          )

      n_area_consolidated <- sum(compiled_data[["area_consolidated"]] == area)

      cat("\n\t n after consolidation:", n_area_consolidated, "\n")

      # 16 add area to areas_consolidated so don't consolidate to it again ####
      areas_consolidated %<>% c(., area)

      # 17 after consolidating an area break to go back to repeat level ####
      break
    }

  }
  # 18 Break loop if you have reached the end of the list ####
  if (area == replacement_areas[[1]][nrow(replacement_areas)]) {
    message("Reached the end of the list. No more areas to consolidate.")
    break
  }
}

# 19 Update categorical variables list ####
categorical_variables[
  which(categorical_variables %in% "area_missing_replaced")] <-
  "area_consolidated"

# 20 Making generalized columns for some variables ####
compiled_data <-
  replace_low_freq_with_other(
    compiled_data
    , "area_consolidated"
    , 50
    )

compiled_data <- replace_low_freq_with_other(compiled_data, "street", 50)

compiled_data <- replace_low_freq_with_other(compiled_data, "agent_name", 50)

compiled_data <- replace_low_freq_with_other(compiled_data, "agency", 50)

# 21 Update the list of categorical variables ####
categorical_variables <-
  c(
    "city"
    , "area_consolidated_low_freq_generalized"
    , "street_low_freq_generalized"
    , "agent_name_low_freq_generalized"
    , "agency_low_freq_generalized"
    )


# 22 Adding features that are calculations of known features ####

compiled_data[["age_when_sold"]] <-
  compiled_data[["year_sold"]] - compiled_data[["year_built"]]

compiled_data[["kvm_per_room"]] <-
  compiled_data[["kvm"]] / compiled_data[["rooms"]]

compiled_data[["avgift_per_kvm"]] <-
  compiled_data[["avgift"]] / compiled_data[["kvm"]]

compiled_data[["avgift_per_room"]] <-
  compiled_data[["avgift"]] / compiled_data[["rooms"]]

compiled_data[["avgift_per_asking_price"]] <-
  compiled_data[["avgift"]] / compiled_data[["asking_price"]]

compiled_data[["running_costs_per_kvm"]] <-
  compiled_data[["running_costs"]] / compiled_data[["kvm"]]

compiled_data[["running_costs_per_room"]] <-
  compiled_data[["running_costs"]] / compiled_data[["rooms"]]

compiled_data[["running_costs_per_asking_price"]] <-
  compiled_data[["running_costs"]] / compiled_data[["asking_price"]]

# 23 Imputing where have NAs for numerical variables ####
set.seed(412)

numerical_variables <-
  c(
    "asking_price"
    , "rooms"
    , "kvm"
    , "floor_in_building"
    , "avgift"
    , "running_costs"
    , "age_when_sold"
    , "kvm_per_room"
    , "avgift_per_kvm"
    , "avgift_per_room"
    , "avgift_per_asking_price"
    , "running_costs_per_kvm"
    , "running_costs_per_room"
    , "running_costs_per_asking_price"
  )

# 24 Replace NAs with a random sample from the real values in new variable ####
for (variable in numerical_variables) {

  new_variable_name <-
    paste0(
      variable
      , "_imputed_from_random_sample"
    )

  non_missing <- compiled_data[[variable]] %>% .[!is.na(.)]

  length_missing <- compiled_data[[variable]] %>% .[is.na(.)] %>% length

  compiled_data[[eval(new_variable_name)]] <-
  ifelse(
    is.na(compiled_data[[variable]])
    , sample(non_missing, length_missing)
    , compiled_data[[variable]]
  )

  # 25 Update numerical variables list ####
  numerical_variables[which(numerical_variables %in% variable)] <-
    new_variable_name
}

# 26 Setting Swedish months to English ####
compiled_data[["month_sold_english"]] <-
  dplyr::case_when(
    compiled_data[["month_sold_swedish"]] ==  "januari" ~ "jan",
    compiled_data[["month_sold_swedish"]] ==  "februari" ~ "feb",
    compiled_data[["month_sold_swedish"]] ==  "mars" ~ "mar",
    compiled_data[["month_sold_swedish"]] ==  "april" ~ "apr",
    compiled_data[["month_sold_swedish"]] ==  "maj" ~ "may",
    compiled_data[["month_sold_swedish"]] ==  "juni" ~ "jun",
    compiled_data[["month_sold_swedish"]] ==  "juli" ~ "jul",
    compiled_data[["month_sold_swedish"]] ==  "augusti" ~ "aug",
    compiled_data[["month_sold_swedish"]] ==  "september" ~ "sep",
    compiled_data[["month_sold_swedish"]] ==  "oktober" ~ "oct",
    compiled_data[["month_sold_swedish"]] ==  "november" ~ "nov",
    compiled_data[["month_sold_swedish"]] ==  "december" ~ "dec"
             )

# 27 Concatenating to lubridate format ####
compiled_data[["date_sold"]] <-
  paste(
    compiled_data[["year_sold"]]
    , compiled_data[["month_sold_english"]]
    , compiled_data[["dayofmonth_sold"]]
    , sep = "-") %>%
    lubridate::ymd()

# 28 Adding several levels of date information for time series analysis ####
compiled_data %<>% add_date_data_for_tsa("date_sold")

# 29 Adding Swedish holiday date information ####
compiled_data %<>% add_swedish_days_off_data("date_sold")

# 30 Engineering time-series analysis features ####

# 31 Aggregating by each categorical variable ####
for (variable in categorical_variables) {

  # 32 Aggregating by unique level within the categorical variables ####
  unique_levels <- unique(compiled_data[[variable]])
  column_prefix <- variable
  sql_column_names <-
    c("median_selling_price", "mean_selling_price", "sum_selling_price") %>%
      paste0(column_prefix, "_", .)

  # 33 Create a dates_table to join to ####
  min_date <-
    compiled_data[["date_sold"]] %>%
    as.Date %>%
    min

  max_date <-
    compiled_data[["date_sold"]] %>%
    as.Date %>%
    max

  dates_df <-
    data.frame(
      date_sold = c(min_date:max_date) %>% as.Date(origin = "1970-01-01")
      , categorical_variable = variable
      )

  for (unique_level in unique_levels) {

    # 34 Add column to dates_df based on the level for joining ####
    dates_df[["level"]] <- unique_level

    # 35 Create temp_df with aggregate fxns by joining to dates_df ####
    temp_df <-
      sqldf::sqldf(
        paste0(
          "select
            d.date_sold
            , level as ", variable,
            ", median(selling_price) as ", sql_column_names[1],
            ", avg(selling_price) as ", sql_column_names[2],
            ", sum(selling_price) as ", sql_column_names[3],
          " from dates_df as d
            left join compiled_data as c
              on d.date_sold = c.date_sold and
                d.level = c.", variable,
          " group by d.date_sold
          order by d.date_sold desc"
          )
        )

    # 36 Interpolate prices (linearly) for dates not in the raw data ####
    for (agg_column in sql_column_names) {

      temp_df %<>%
        interpolate_for_missing_dates(agg_column, "date_sold")

      column_for_further_aggs <- paste0(agg_column, "_interpolated")

      # 37 Will do rolling and lag fxns at several timeframes ####
      timeframes_to_look <- c(7, 31, 91, 365)
        for (days_back in timeframes_to_look) {
          cat(
            "Working on: ", variable
            , "--", unique_level
            , "--", agg_column
            , "--", days_back
            , "\n"
            )

          # 38 Prepping column names ####
          col_name_suf_for_roll_data <-
            paste0("_selling_price_last_", days_back, "_days")

          col_name_suf_for_lag_data <-
            paste0("_selling_price_", days_back, "_days_ago")

          if (grepl("_median_", agg_column)) {
            fxn <- "median"
          } else if (grepl("_mean_", agg_column)) {
            fxn <- "mean"
          } else if (grepl("_sum_", agg_column)) {
            fxn <- "sum"
          }

          column_name_for_rolling_data <-
            paste0(column_prefix, "_", fxn, col_name_suf_for_roll_data)
          column_name_for_lag_data <-
            paste0(column_prefix, "_", fxn, col_name_suf_for_lag_data)

          # 39 Rolling aggregation data ####
          fxn_text_for_rolling_data <- paste0("RcppRoll::roll_", fxn, "l")

          temp_df[[column_name_for_rolling_data]] <-
            eval(parse(text = fxn_text_for_rolling_data))(
              temp_df[[column_for_further_aggs]]
              , days_back
              , fill = NA)

          temp_df %<>%
            interpolate_for_missing_dates(
              column_name_for_rolling_data, "date_sold")

          # 40 Lag data ####
          temp_df[[column_name_for_lag_data]] <-
            dplyr::lead(
              temp_df[[column_for_further_aggs]]
              , days_back
              )

          temp_df %<>%
            interpolate_for_missing_dates(
              column_name_for_lag_data
              , "date_sold"
              )

        # 41 Engineering more features that are lag + rolling ####

        # 42 Lag+rolling features only if done with last timeframe ####

        if (days_back == (timeframes_to_look %>% .[length(.)])) {

        timeframe_permutations <-
          data.frame(
            roll_day = c(
              rep(timeframes_to_look, length(timeframes_to_look)) %>% sort
              )
            , lag_day = c(timeframes_to_look)
            )

        # 43 set the rolling and lag days ####
        for (row_n in seq_len(nrow(timeframe_permutations))) {
          roll_day <- timeframe_permutations[row_n, 1]
          lag_day <- timeframe_permutations[row_n, 2]

          col_name_suf_for_roll_lag_data <-
                  paste0(
                    "_selling_price_over_"
                    , roll_day
                    , "_days_"
                    , lag_day
                    , "_days_ago"
                    )

          # 44 Name the columns ####
          col_name_for_roll_lag_data <-
            paste0(column_prefix, "_"
              , fxn
              , col_name_suf_for_roll_lag_data
              )

          column_name_for_source_data <-
            paste0(
              column_prefix
              , "_"
              , fxn
              , "_selling_price_last_"
              , roll_day
              , "_days"
              )

          # 45 Create the columns ####
          temp_df[[col_name_for_roll_lag_data]] <-
            dplyr::lead(
              temp_df[[column_name_for_source_data]]
              , lag_day
              )

          temp_df %<>%
            interpolate_for_missing_dates(
              col_name_for_roll_lag_data
              , "date_sold"
              )
        }
      }
      }
    }

    # 46 Creating the join_df ####
    if (unique_level == unique_levels[1]) {
      join_df <<- temp_df
    } else {
      join_df %<>% rbind(temp_df)
    }
  }

  # 47 Adding the new columns to the compiled_data ####
  compiled_data_columns_for_sql <-
    names(compiled_data) %>% paste0("c.", ., collapse = ", ")

  new_columns_for_sql <-
    names(join_df)[3 : ncol(join_df)] %>% paste0(collapse = ", ")

  compiled_data <-
    sqldf::sqldf(
      paste0(
        "select ", compiled_data_columns_for_sql,
        ", ", new_columns_for_sql,
        " from compiled_data as c
            left join join_df as j
              on c.date_sold = j.date_sold and
                c.", variable, " = j.", variable
        )
      )

  message(
    "Compiled data dimensions: ", dim(compiled_data)[1]
    , " by ", dim(compiled_data)[2])

}

# 48 One-hot-encoding for non-interval/ratio features ####
features_for_ohe <-
  c(
    "city"
    , "area_consolidated_low_freq_generalized"
    , "street_low_freq_generalized"
    , "agent_name_low_freq_generalized"
    , "agency_low_freq_generalized"
    , "dayofmonth_sold"
    , "month_sold_english"
    , "quarterofyear_sold"
    , "monthofyear_sold"
    , "monthofquarter_sold"
    , "weekofyear_sold"
    , "weekofquarter_sold"
    , "weekofmonth_sold"
    , "dayofyear_sold"
    , "dayofquarter_sold"
    , "dayofmonth_sold"
    , "dayofweek_sold"
    )

# 49 Create df for making the ohe features ####
df_for_ohe <- compiled_data[, features_for_ohe]

# 50 Convert all to factor so one-hot encoding will work on numeric data ####
df_for_ohe[] <- lapply(df_for_ohe, as.factor)

# 51 Make the ohe columns ####
library(caret)
df_for_ohe %<>%
  caret::dummyVars(" ~ .", data = ., sep = "_") %>%
    predict(newdata = df_for_ohe) %>%
    data.frame

# 52 add prefix for easier sorting later ####
names(df_for_ohe) %<>% paste0("ohe_", .)

# 53 Add columns back to compiled_data ####
compiled_data %<>%
  cbind(df_for_ohe)

# 54 Converting what could be numerical or character into factor data ####
column_identifiers <- c("dayof", "weekof", "monthof", "quarterof", "ohe_")

for (id in column_identifiers) {
  for (clmn in names(compiled_data)) {
    if ((grepl(id, clmn)) | (class(compiled_data[[clmn]]) == "character")) {
      compiled_data[[clmn]] %<>% as.factor
    }
  }
}

# 55 Reordering some factors for display purposes ####
compiled_data[["dayofweek_sold"]] %<>%
  factor(x = ., levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))

compiled_data[["month_sold_swedish"]] %<>%
  factor(
    x = .,
    levels = c(
      "januari",
      "februari",
      "mars",
      "april",
      "maj",
      "juni",
      "juli",
      "augusti",
      "september",
      "oktober",
      "november",
      "december"
    )
  )

compiled_data[["month_sold_english"]] %<>%
  factor(
    x = .,
    levels = c(
      "jan",
      "feb",
      "mar",
      "apr",
      "may",
      "jun",
      "jul",
      "aug",
      "sep",
      "oct",
      "nov",
      "dec"
    )
  )


# 56 Identifying processed data variables ####
psddata_variables <-
  colnames(compiled_data)[colnames(compiled_data) %not_in% rawdata_variables]

# 56 Adding "rawdata_" suffix to all original variables ####
rawdata_variable_names <- paste0("rawdata_", rawdata_variables)

old_colnames_to_replace <-
  match(colnames(compiled_data), rawdata_variables, nomatch = 0)

colnames(compiled_data)[colnames(compiled_data) %in% rawdata_variables] <-
  rawdata_variable_names[old_colnames_to_replace]

# 57 Adding "psddata_" suffix to all new variables ####
psddata_variable_names <- paste0("psddata_", psddata_variables)

new_colnames_to_replace <-
  match(colnames(compiled_data), psddata_variables, nomatch = 0)

colnames(compiled_data)[colnames(compiled_data) %in% psddata_variables] <-
  psddata_variable_names[new_colnames_to_replace]

# 58 save the object ####
name_of_results_df <-
    paste0(
        "date_"
        , today_8digit()
    )

saveRDS(
  compiled_data
  , paste0(
    out_folder_preprocessed_data
    , eval(name_of_results_df)
    )
  )