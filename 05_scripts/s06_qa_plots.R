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

# 03 Creating subsets of output variables for shiny apps ####

#### THIS SECTION SHOULD BE CHANGED SINCE I ADDED SUFFIXES "rawdata_"

original_vars <-
  names(preprocessed_data) %>%
  .[which(. == "selling_price") : which(. == "url")]

original_numeric_vars <-
  original_vars %>%
  .[(
    preprocessed_data[, .] %>%
      sapply(is.numeric)
  )]

original_factor_vars <-
  original_vars %>%
  .[. %not_in% original_numeric_vars]

# 12 specifying engineered variable subsets for plotting ####
engineered_vars <-
  names(preprocessed_data) %>%
  .[. %not_in% original_vars]

engineered_numeric_vars <-
  engineered_vars %>%
  .[(
    preprocessed_data[, .] %>%
      sapply(is.numeric)
  )]

engineered_factor_vars <-
  engineered_vars %>%
  .[. %not_in% engineered_numeric_vars]

engnrd_fctr_vars_seeded_nmrc <-
  engineered_factor_vars %>%
  .[which(. == "area_missing_replaced") : which(. == "dayofweek_sold")]

grouped_engineered_numeric_vars <-
  engineered_numeric_vars %>%
  .[grepl(paste0(c("mean","median","sum"), collapse = "|"), .)]

engineered_numeric_vars %<>%
  .[. %not_in% grouped_engineered_numeric_vars]



# PAUSED HERE

# Can probably delete much of below after moving to the shiny apps






### PAUSED HERE



##price by dayofweek_sold and weekofmonth_sold


#Heatmap
dayofweek_sold_by_weekofmonth_sold_median_heatmap <- my_heatmap(
  df = preprocessed_data,
  x = "dayofweek_sold",
  y = "weekofmonth_sold",
  z = "selling_price",
  aggfxn = "median",
  title = "Median price is lowest Week 5 Saturday or Monday.
  Prices tend to be lowest in Week 5 and highest in week 2.")

dayofweek_sold_by_weekofmonth_sold_median_heatmap

save_plot_as_html(dayofweek_sold_by_weekofmonth_sold_median_heatmap)


##price by weekofmonth_sold and monthofquarter_sold


#Heatmap
weekofmonth_sold_by_monthofquarter_sold_median_heatmap <- my_heatmap(
  df = preprocessed_data,
  x = "weekofmonth_sold",
  y = "monthofquarter_sold",
  z = "selling_price",
  aggfxn = "median",
  title = "Median price is lowest in Week 5, beginning of quarter. Prices tend
  to be higher week 2, lower week 1.")

weekofmonth_sold_by_monthofquarter_sold_median_heatmap

save_plot_as_html(weekofmonth_sold_by_monthofquarter_sold_median_heatmap)


##price by monthofquarter_sold and quarterofyear_sold


#Heatmap
monthofquarter_sold_by_quarterofyear_sold_median_heatmap <- my_heatmap(
  df = preprocessed_data,
  x = "monthofquarter_sold",
  y = "quarterofyear_sold",
  z = "selling_price",
  aggfxn = "median",
  title = "Median price is highest third quarter, lowest in the first quarter.")

monthofquarter_sold_by_quarterofyear_sold_median_heatmap

save_plot_as_html(monthofquarter_sold_by_quarterofyear_sold_median_heatmap)


##price by dayofweek_sold, weekofmonth_sold, monthofyear_sold and year_sold


#Calendar heatmap
calendar_heatmap <- my_calendar_heatmap(
  df = preprocessed_data,
  day_col = "dayofweek_sold",
  week_col = "weekofmonth_sold",
  month_col = "monthofyear_sold",
  year_col = "year_sold",
  z = "selling_price",
  title = "No very obvious trends.")

calendar_heatmap

save_plot_as_html(calendar_heatmap)


##price by rooms and floor


#Heatmap
rooms_by_floor_median_heatmap <- my_heatmap(
  df = preprocessed_data,
  x = "rooms",
  y = "floor",
  z = "selling_price",
  aggfxn = "median",
  title = "Median price is lowest for 1.5 rooms on the 22 floor. Prices tend to
  be lowest for 1-room apartments, although there are some inexpensive at 3 rooms.")

rooms_by_floor_median_heatmap

save_plot_as_html(rooms_by_floor_median_heatmap)

















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
#Whichc month of the quarter sells highest/lowest?
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


# For 2 var plots, should compare density plots of, for example,
# kvm vs kvm with interpolation and other "corrections"
# Perhaps by working with layered density plot?




#must return to original_wd
setwd(original_wd)