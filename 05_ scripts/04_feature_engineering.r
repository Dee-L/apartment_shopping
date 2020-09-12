source("D:/Coding/R/Public/pkgs_and_fxns/load_pkgs_and_fxns.R")

setwd("D:/Coding/R/Apartment shopping/script output/") #setwd

NAs_replaced <-
  read.csv(file = "03 NAs replaced/2019-08-19.csv", header = T)

working_data <- NAs_replaced


#Adding date information

working_data[["month_sold_english"]] <-
  case_when(
    working_data[["month_sold_swedish"]] ==  "januari" ~ "jan",
    working_data[["month_sold_swedish"]] ==  "februari" ~ "feb",
    working_data[["month_sold_swedish"]] ==  "mars" ~ "mar",
    working_data[["month_sold_swedish"]] ==  "april" ~ "apr",
    working_data[["month_sold_swedish"]] ==  "maj" ~ "may",
    working_data[["month_sold_swedish"]] ==  "juni" ~ "jun",
    working_data[["month_sold_swedish"]] ==  "juli" ~ "jul",
    working_data[["month_sold_swedish"]] ==  "augusti" ~ "aug",
    working_data[["month_sold_swedish"]] ==  "september" ~ "sep",
    working_data[["month_sold_swedish"]] ==  "oktober" ~ "oct",
    working_data[["month_sold_swedish"]] ==  "november" ~ "nov",
    working_data[["month_sold_swedish"]] ==  "december" ~ "dec"
             )

working_data[["date_sold"]] <-
  paste(working_data[["year_sold"]], working_data[["month_sold_english"]], working_data[["day_of_month_sold"]], sep = "-") %>% ymd

# Will create other tables that have potential predictors by aggregating at different levels.

# Categories (each becomes column 1 in own table)

categories_df <-
  data.frame(category = c("city", "area", "street", "street_number", "address", "hoa", "agency", "agent_name"))

for (cat in categories_df[["category"]]) {

  temp_df <- data.frame(category = NAs_replaced[[cat]] %>% unique)

  assign(x = paste0(cat, "_df"), value = temp_df)
  
}

# Timeframe (becomes column 2 in each table above)

min_date <-
  working_data[["date_sold"]] %>%
  as.Date %>%
  min

max_date <-
  working_data[["date_sold"]] %>%
  as.Date %>%
  max

dates_sold <- as.Date(NA)
 
for (d in min_date:max_date) {
  dates_sold %<>% rbind(as.Date(d))
}

dates_sold %<>% .[complete.cases(.), ]  

dates_sold %<>% as.Date

date_df <- data.frame(date_sold = dates_sold)

date_df[["quarterofyear_sold"]] <-
  quarter(date_df[["date_sold"]])

date_df[["monthofyear_sold"]] <-
  month(date_df[["date_sold"]])

date_df[["monthofquarter_sold"]] <-
  (floor((month(date_df[["date_sold"]]) - 1) / 4) + 1)

date_df[["weekofyear_sold"]] <-
  week(date_df[["date_sold"]])

date_df[["weekofquarter_sold"]] <-
  (floor((week(date_df[["date_sold"]]) - 1) / 4) + 1)

date_df[["weekofmonth_sold"]] <-
  (floor((mday(date_df[["date_sold"]]) - 1) / 7) + 1)

date_df[["dayofyear_sold"]] <-
  yday(date_df[["date_sold"]])

date_df[["dayofquarter_sold"]] <-
  qday(date_df[["date_sold"]])

date_df[["dayofmonth_sold"]] <-
  mday(date_df[["date_sold"]])

date_df[["dayofweek_sold"]] <-
  case_when(
    wday(date_df[["date_sold"]]) ==  1 ~ "sun",
    wday(date_df[["date_sold"]]) ==  2 ~ "mon",
    wday(date_df[["date_sold"]]) ==  3 ~ "tue",
    wday(date_df[["date_sold"]]) ==  4 ~ "wed",
    wday(date_df[["date_sold"]]) ==  5 ~ "thu",
    wday(date_df[["date_sold"]]) ==  6 ~ "fri",
    wday(date_df[["date_sold"]]) ==  7 ~ "sat",
  )

date_df[["swedish_red_or_pink_day"]] <- NA

for (d in 1 : nrow(date_df)) {
  date_df[["swedish_red_or_pink_day"]][d] <-
    ifelse(is_swedish_red_or_pink_day(date_df[["date_sold"]][d]),
           1,
           0)
}

date_df[["swedish_pinch_day"]] <- NA

for (d in 1 : nrow(date_df)) {
  date_df[["swedish_pinch_day"]][d] <-
    ifelse(is_swedish_pinch_day(date_df[["date_sold"]][d]),
           1,
           0)
}

date_df[["swedish_day_off"]] <-
  ifelse(
    (date_df[["swedish_red_or_pink_day"]] == 1) |
      (date_df[["swedish_pinch_day"]] == 1),
    1,
    0)

# Predictors to aggregate (become columns 3 : ... in each table above)
predictors_df <-
  data.frame(predictor = c("price"))

for (pred in predictors_df[["predictor"]]) {

  temp_df <- data.frame(predictor = NAs_replaced[[cat]] %>% unique)
  
  assign(x = paste0(pred, "_df"), value = temp_df)
  
}

 
# Aggregation functions (each becomes a column for each of the predictors in the list immediately before this one)
aggregations_df <-
  data.frame(aggregation = c("avg", "median", "sum"), roll_fxn = c("roll_mean", "roll_median", "roll_sum"))

#Aggregating values into 'dailyaggby...' dfs

#Build the query that will be looped
final_cols_in_sql_query <- ""

for (i in 1 : nrow(predictors_df)) {
  for (j in 1 : nrow(aggregations_df)) {
    final_cols_in_sql_query %<>%
      c(", ",
        aggregations_df[["aggregation"]][j] %>%
          as.character,
        "(", predictors_df[["predictor"]][i] %>%
          as.character, ")") %>%
      paste(collapse = "")
  }
}

categories_df[["agg_df"]] <- ""

#Create the calendar to bring in missing days
calendar_df <-
  data.frame(day = c(min_date:max_date) %>% as.Date)

#Specify how many days back to look for lag and rolling aggregation functions
lag_and_rolling_fxn_days <-
  c(7, 30, 90, 365)

temp_df_source <- working_data

#The big loop to engineer all of the columns into multiple different dataframes - one dataframe per categorical predictor variable.
for (i in 1 : nrow(categories_df)) {

  cat(
    paste0(
      "Engineering features for category ",
      i,
      " of ",
      nrow(categories_df)
      ),
    "\n\n")

  category <-
    categories_df[["category"]][i] %>%
    as.character
  
  cat(paste0("Category is: ", category), "\n\n")
  
  
  cat_df <- data.frame(
    category = unique(temp_df_source[[category]]))
  
  #Aggregate the data using the sql query pre-written earlier
  temp_df <-
    sqldf(
      paste0(
        "select ",
        ifelse(category == "",
               " date_sold",
               paste0(category, ", date_sold")),
        final_cols_in_sql_query,
        "from temp_df_source
      group by ",
        ifelse(category == "",
               "1",
               "1, 2")))
  
  #Match all levels of the category to all days
  joining_table <-
    sqldf("select *
          from cat_df
          cross join calendar_df")
  
  #Bring in all of the missing days
  temp_df %<>%
    merge(x = .,
      y = joining_table,
      by.x = c(as.character(category),
        "date_sold"),
      by.y = c("category", "day"),
      all.y = T)
  
  #Interpolate NAs from the missing days
  temp_df %<>%
    na.interpolation(option = "spline")
  
  tries <- 1
  while (tries > 0 & tries < 10) {
    cat(paste0("Try: ", tries), "\n\n")
    
    for (co in 3 : ncol(temp_df)) {
      temp_df[which(temp_df[co] < 0), co] <-
        NA
    }
    
    op <-
      ifelse(tries == 1,
       "stine",
       "linear")
    
    temp_df %<>%
      na.interpolation(option = op)
    
    ifelse(any(temp_df[ , 3:ncol(temp_df)] < 0),
           {tries <- tries + 1},
           {tries <- 0})
  }
  
  #Engineer predictor columns based on lag function going back x number of days
  pre_lag_n_cols <- ncol(temp_df)
  
  temp_df <-
    sqldf(
      paste0("select *
             from temp_df
             order by ", category, ", date_sold asc"))
  
  for (d in lag_and_rolling_fxn_days){
  
    for (clmn in ((2 + 1) : pre_lag_n_cols)) {
  
      new_clmn_name <-
        paste0(colnames(temp_df)[clmn], "_", d, "_days_ago_this_", category)
      
      temp_array <-
        tapply(X = temp_df[ , clmn],
               INDEX = temp_df[ , category],
               FUN = lag, d)
      
      array_names <- names(temp_array) %>% sort
      
      new_vector <- numeric()
      
      for (nm in array_names) {
        new_vector %<>% c(temp_array[[nm]])
      }
      
      temp_df[new_clmn_name] <- new_vector
      
      }
    
  }
  
  #Backfill NAs from the lag functions
  #Interpolate NAs from the missing days
  temp_df %<>%
    na.interpolation(option = "spline")
  
  tries <- 1
  while (tries > 0 & tries < 10) {
    cat(paste0("Try: ", tries), "\n\n")
    
    for (co in 3 : ncol(temp_df)) {
      temp_df[which(temp_df[co] < 0), co] <-
        NA
    }
    
    op <-
      ifelse(tries == 1,
             "stine",
             "linear")
    
    temp_df %<>%
      na.interpolation(option = op)
    
    ifelse(any(temp_df[ , 3 : ncol(temp_df)] < 0),
           {tries <- tries + 1},
           {tries <- 0})
  }
  
  #Rolling aggregations (become new columns for each of the engineered columns above except for the lag columns)
  
  temp_df <-
    sqldf(
      paste0("select *
             from temp_df
             order by ", category, ", date_sold asc"))
  
  for (d in lag_and_rolling_fxn_days) {
  
    for (clmn in ((2 + 1) : pre_lag_n_cols)) {
  
      new_clmn_name <-
        paste0("rolling_", colnames(temp_df)[clmn], "_previous_", d, "_days_this_", category)
      
      fxn_to_use <-
        ifelse(grepl("avg", colnames(temp_df)[clmn]),
               copy(roll_mean),
               ifelse(grepl("median", colnames(temp_df)[clmn]),
                      copy(roll_median),
                      ifelse(grepl("sum", colnames(temp_df)[clmn]),
                             copy(roll_sum),
                             "error")))
      
      temp_array <-
        tapply(X = temp_df[ , clmn],
               INDEX = temp_df[ , category],
               FUN = function(x) {
                 c(rep(NA, d), fxn_to_use(x, d)) %>%
                   .[1 : length(.) - 1]
                 })
        
      array_names <- names(temp_array) %>% sort
      
      new_vector <- numeric()
      
      for (nm in array_names) {
        new_vector %<>% c(temp_array[[nm]])
      }
      
      temp_df[new_clmn_name] <- new_vector

    }
    
  }
  
  #Backfill NAs from the rolling aggregation functions
  #Interpolate NAs from the missing days
  temp_df %<>%
    na.interpolation(option = "spline")
  
  tries <- 1
  while (tries > 0 & tries < 10) {
    cat(paste0("Try: ", tries), "\n\n")
    
    for (co in 3 : ncol(temp_df)) {
      temp_df[which(temp_df[co] < 0), co] <-
        NA
    }
    
    op <-
      ifelse(tries == 1,
             "stine",
             "linear")
    
    temp_df %<>%
      na.interpolation(option = op)
    
    ifelse(any(temp_df[ , 3 : ncol(temp_df)] < 0),
           {tries <- tries + 1},
           {tries <- 0})
  }
  
  #Join back to the working_data
  working_data %<>%
    merge(y = temp_df, all.x = T) %>%
    .[ , colnames(.) %not_in% c("avg(price)", "median(price)", "sum(price)")]
  
}

#Bring in the date_df data
working_data %<>%
  merge(y = date_df, all.x = T)

#Perform encode dummy variables from factor variables
working_data[["type"]] <- NULL
working_data[["month_sold_swedish"]] <- NULL
working_data[["month_sold_english"]] <- NULL

working_data %<>% encode_dummy_variables


#Adding a few more features that are composites of others and may not be captured by interaction effects.

working_data[["age_when_sold"]] <-
  working_data[["year_sold"]] - working_data[["year_built"]]

working_data[["kvm_per_room"]] <-
  working_data[["kvm"]] / working_data[["rooms"]]

working_data[["avgift_per_kvm"]] <-
  working_data[["avgift"]] / working_data[["kvm"]]

working_data[["avgift_per_rooms"]] <-
  working_data[["avgift"]] / working_data[["rooms"]]

working_data[["avgift_per_asking_price"]] <-
  working_data[["avgift"]] / working_data[["asking_price"]]

working_data[["running_costs_per_kvm"]] <-
  working_data[["running_costs"]] / working_data[["kvm"]]

working_data[["running_costs_per_rooms"]] <-
  working_data[["running_costs"]] / working_data[["rooms"]]

working_data[["running_costs_per_asking_price"]] <-
  working_data[["running_costs"]] / working_data[["asking_price"]]



dir.create("04 features engineered")

write.csv(x = working_data,
          file = paste0("04 features engineered/", now() %>% as_date, ".csv"),
          row.names = F)