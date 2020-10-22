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

# 03 Testing data classes ####
all_numeric_vars <-
  preprocessed_data %>%
    dplyr::select_if(is.numeric) %>%
    names

all_other_vars <-
  preprocessed_data %>%
  select(!c(all_numeric_vars, all_factor_vars)) %>%
  names

if (length(all_other_vars) > 0) message("Some variables are misclassified")

# I don't think I'm using this one
# original_factor_vars <-
#   intersect(original_vars, all_factor_vars)

# 05 specifying engrd variable subsets for plotting ####


# 06 Variables for density plots ####
all_vars <-
  names(preprocessed_data)

original_vars <-
  stringr::str_subset(all_vars, "_rawdata")

original_numeric_vars <-
  intersect(original_vars, all_numeric_vars)

numeric_vars_for_density_plots <-
   sort(c(original_numeric_vars, engrd_numeric_not_agg_by_cat))

# 07 Cat variables for violin, strip, and conditional density plots ####
engrd_vars <-
  stringr::str_subset(all_vars, "_psddata")

all_factor_vars <-
  preprocessed_data %>%
  dplyr::select_if(is.factor) %>%
  names()

engrd_factor_vars <-
  intersect(engrd_vars, all_factor_vars)

cat_vars_for_plots <-
  engrd_factor_vars %>%
  stringr::str_subset("ohe_", negate = T) %>%
  stringr::str_subset("month_sold_english", negate = T) %>%
  stringr::str_subset("generalized|sold") %>%
  c("city_rawdata", .)

# 08 Con variables for violin, strip, and conditional density plots ####
engrd_numeric_vars <-
  intersect(engrd_vars, all_numeric_vars)

engrd_numeric_agg_by_cat <-
  stringr::str_subset(engrd_numeric_vars, "_mean_|_median_|_sum_")

engrd_numeric_not_agg_by_cat <-
  setdiff(engrd_numeric_vars, engrd_numeric_agg_by_cat)

# Paused here, need to make sure capturing nonhalf floor dropped only in
# all relevant var sets that are being plotted later. I started
# working on removing the floors that don't have nonhalf floors dropped
# below but didn't finish making it or incorporating it into
# con_vars_for_plots

engnrd_nonhalf_not_dropped <-
  engrd_numeric_not_agg_by_cat %>%
    stringr::str_subset("floor_") %>%
    stringr::str_subset("nonhalf", negate = T)

con_vars_for_plots <-
  stringr::str_subset(engrd_numeric_not_agg_by_cat, "imputed") %>%
  setdiff(engnrd_nonhalf_not_dropped) %>%
  sort %>%
  c("selling_price_rawdata", .)


# 08 Variables for heatmaps ####
discrete_vars_for_heatmaps <-
  stringr::str_subset(con_vars_for_plots, "kvm|rooms|floor") %>%
  c(cat_vars_for_plots)



con_vars_for_aggs <-
  engrd_numeric_vars %>%
  stringr::str_subset("mean|median|sum") %>%
  stringr::str_subset("ago_psddata|days_psddata", negate = T)

cat_vars_for_agg_plots <-
  cat_vars_for_plots %>%
  stringr::str_subset(categorical_variables_for_tsa)

# Paused here - want to generalize the heatmap function to a shiny app

x_var <- discrete_vars_for_heatmaps[1]
y_var <- discrete_vars_for_heatmaps[2]
color_var <- con_vars_for_plots[1]

my_heatmap(
  df = preprocessed_data,
  x = cat_vars_for_plots,
  y = y_var,
  color = color_var,
  aggfxn = "median",
  title = "Heatmap: Price by day of week and week of month"
)


#Heatmap - not working - too many SQL variables
  my_heatmap(
    df = preprocessed_data,
    x = "dayofweek_sold_psddata",
    y = "weekofmonth_sold_psddata",
    color = "selling_price_rawdata",
    aggfxn = "median",
    title = "Heatmap: Price by day of week and week of month")




#Heatmap
  my_heatmap(
    df = preprocessed_data,
    x = "weekofmonth_sold_psddata",
    y = "monthofquarter_sold_psddata",
    color = "selling_price_rawdata",
    aggfxn = "median",
    title = "Heatmap: Price by week of month and month of quarter")

#Heatmap
  my_heatmap(
    df = preprocessed_data,
    x = "monthofquarter_sold_psddata",
    y = "quarterofyear_sold_psddata",
    color = "selling_price_rawdata",
    aggfxn = "median",
    title = "Heatmap: Price by month of quarter and quarter of year")



##price by dayofweek_sold, weekofmonth_sold, monthofyear_sold and year_sold


#Calendar heatmap
my_calendar_heatmap(
  df = preprocessed_data,
  day_col = "dayofweek_sold_psddata",
  week_col = "weekofmonth_sold_psddata",
  month_col = "monthofyear_sold_psddata",
  year_col = "year_sold_rawdata",
  color = "selling_price_rawdata",
  title = "Heatmap: Price over the years")

#Heatmap
my_heatmap(
  df = preprocessed_data,
  x = "rooms_imputed_from_random_sample_psddata",
  y = "floor_in_building_imputed_from_random_sample_psddata",
  color = "selling_price_rawdata",
  aggfxn = "median",
  title = "Heatmap: Price by rooms and floor")

my_heatmap(
  df = preprocessed_data,
  x = "rooms_imputed_from_random_sample_psddata",
  y = "nonhalf_floors_dropped_imputed_from_random_sample_psddata",
  color = "selling_price_rawdata",
  aggfxn = "median",
  title = "Heatmap: Price by rooms and floor"
)

















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


