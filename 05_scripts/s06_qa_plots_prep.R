# Purpose: Plot some of my features to get a sense of the data
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
    "htmlwidgets"
    , "sqldf"
  )

install_my_pkgs(pkgs)

# 02 load latest preprocessed data ####

preprocessed_data <-
  paste0(
    out_folder_preprocessed_data,
    list.files(out_folder_preprocessed_data) %>%
      .[length(.)]
  ) %>%
  readRDS()

# 03 Saving preprocessed data as csv for Orange ####


# 04 Creating subsets of output variables for shiny apps ####
all_vars <-
  names(preprocessed_data)

all_numeric_vars <-
  preprocessed_data %>%
    dplyr::select_if(is.numeric) %>%
    names

all_factor_vars <-
  preprocessed_data %>%
    dplyr::select_if(is.factor) %>%
    names

all_other_vars <-
  preprocessed_data %>%
  select(!c(all_numeric_vars, all_factor_vars)) %>%
  names

if (length(all_other_vars) > 0) message("Some variables are misclassified")

original_vars <-
  stringr::str_subset(all_vars, "_rawdata")

original_numeric_vars <-
  intersect(original_vars, all_numeric_vars)

original_factor_vars <-
  intersect(original_vars, all_factor_vars)

# 05 specifying engineered variable subsets for plotting ####
engineered_vars <-
  stringr::str_subset(all_vars, "_psddata")

engineered_numeric_vars <-
  intersect(engineered_vars, all_numeric_vars)

engineered_factor_vars <-
  intersect(engineered_vars, all_factor_vars)

engineered_numeric_agg_by_cat <-
  stringr::str_subset(engineered_numeric_vars, "_mean_|_median_|_sum_")

engineered_numeric_not_agg_by_cat <-
  setdiff(engineered_numeric_vars, engineered_numeric_agg_by_cat)

numeric_vars_for_density_plots <-
  sort(c(original_numeric_vars, engineered_numeric_not_agg_by_cat))

cat_vars_for_plots <-
  engineered_factor_vars %>%
  stringr::str_subset("ohe_", negate = T) %>%
  stringr::str_subset("month_sold_english", negate = T) %>%
  stringr::str_subset("generalized|sold") %>%
  c("city_rawdata", .)

cont_vars_for_scatter_plots <-
  stringr::str_subset(engineered_numeric_not_agg_by_cat, "imputed|date_sold") %>% head(100)

cont_vars_for_aggs <-
  engineered_numeric_vars %>%
  stringr::str_subset("mean|median|sum") %>%
  stringr::str_subset("ago_psddata|days_psddata", negate = T)

# Paused here

#Heatmap - not working - too many SQL variables
dayofweek_sold_by_weekofmonth_sold_median_heatmap <- my_heatmap(
  df = preprocessed_data,
  x = "dayofweek_sold_psddata",
  y = "weekofmonth_sold_psddata",
  z = "selling_price_rawdata",
  aggfxn = "median",
  title = "Median price is lowest Week 5 Saturday or Monday.
  Prices tend to be lowest in Week 5 and highest in week 2.")

dayofweek_sold_by_weekofmonth_sold_median_heatmap



##price by weekofmonth_sold and monthofquarter_sold


#Heatmap
# weekofmonth_sold_by_monthofquarter_sold_median_heatmap <- my_heatmap(
  # df = preprocessed_data,
  # x = "weekofmonth_sold",
  # y = "monthofquarter_sold",
  # z = "selling_price",
  # aggfxn = "median",
  # title = "Median price is lowest in Week 5, beginning of quarter. Prices tend
  # to be higher week 2, lower week 1.")
# 
# weekofmonth_sold_by_monthofquarter_sold_median_heatmap
# 
# save_plot_as_html(weekofmonth_sold_by_monthofquarter_sold_median_heatmap)


##price by monthofquarter_sold and quarterofyear_sold


#Heatmap
# monthofquarter_sold_by_quarterofyear_sold_median_heatmap <- my_heatmap(
  # df = preprocessed_data,
  # x = "monthofquarter_sold",
  # y = "quarterofyear_sold",
  # z = "selling_price",
  # aggfxn = "median",
  # title = "Median price is highest third quarter, lowest in the first quarter.")
# 
# monthofquarter_sold_by_quarterofyear_sold_median_heatmap
# 
# save_plot_as_html(monthofquarter_sold_by_quarterofyear_sold_median_heatmap)


##price by dayofweek_sold, weekofmonth_sold, monthofyear_sold and year_sold


#Calendar heatmap
# calendar_heatmap <- my_calendar_heatmap(
  # df = preprocessed_data,
  # day_col = "dayofweek_sold",
  # week_col = "weekofmonth_sold",
  # month_col = "monthofyear_sold",
  # year_col = "year_sold",
  # z = "selling_price",
  # title = "No very obvious trends.")
# 
# calendar_heatmap
# 
# save_plot_as_html(calendar_heatmap)


##price by rooms and floor


#Heatmap
# rooms_by_floor_median_heatmap <- my_heatmap(
  # df = preprocessed_data,
  # x = "rooms",
  # y = "floor",
  # z = "selling_price",
  # aggfxn = "median",
  # title = "Median price is lowest for 1.5 rooms on the 22 floor. Prices tend to
  # be lowest for 1-room apartments, although there are some inexpensive at 3 rooms.")
# 
# rooms_by_floor_median_heatmap
# 
# save_plot_as_html(rooms_by_floor_median_heatmap)

















#Considerations:
#After doing some apartment touring, maybe I can match certain föreningen to quality on a 1-5 pt scale
#
#Other critical data I am missing is # of bathrooms, although this will probably only be 1 for any apartment I am looking at anyway.
#
#Another piece of data not scraped from hemnet is if the apartment has washer/dryer hookups.
#


#Things to do:

#transform/normalize skewed data by transformations (cube root, log, box-cox)
#scale/standardize the data by minmax (x-min/(max-min)) or z-transformation (scale function)
#
#In doing elastic net, see if can program it to regress on all predictors as well as all two-way interactions (if not all possible interactions)
#
#Questions to answer:
#What is predicted selling price of an apartment?
#What is predicted selling price of same apartment in 1-5 years?
#Which apartments on the market now have the greatest potential increase in value over 1-5 years?
#Are there agencies/agents that consistently sell apartments above/below predicted value?
#Which day of the week sells highest/lowest?
#Which week of the month sells highest/lowest?
#Which month of the quarter sells highest/lowest?
#Which quarter of the year sells highest/lowest?
#Is there an effect of Swedish day off (holiday or pinch day) on sales price?
#Perhaps if my model is not great overall, it may still work in certain areas (Kungsholmen or Solna, for example). Check out doing subset analysis.
#
#How many of the datapoints I harvested are from apartments that have been bought and re-sold (same address and city) and how did their price development follow what I would have predicted?
#
#Can I get more information that would improve the model?:
#Was apartment sold before/after viewings (and is there an effect on price)?
#What was the date the the apartment was announced, and thereby the time on the market (date sold minus date announced)?
#How many bathrooms in the apartment?
#Time/distance to a T-bana


