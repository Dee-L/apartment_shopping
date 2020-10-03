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

# 03 Specifying output folders ####
out_folder_qa_plots <-
  paste0(
    output_folder,
    today_8digit(),
    "/05_qa_plots"
    )

if (!dir.exists(out_folder_qa_plots)) {
  dir.create(out_folder_qa_plots)
}

# 04 Creating paths for plots and objects to call the paths ####
vars_to_plot <- c(1 : 3)
feature_types <- c("original", "engineered")
feature_by_feature_resolution <-
  c("cat_by_ir", "ir_by_ir")
for (var in vars_to_plot) {
  name_of_object <-
    paste0(
      "out_folder_"
      , var
      , "_var_plots"
      )

  name_of_folder <-
    paste0(
      out_folder_qa_plots,
      "/0"
      , var
      , "_var_plots"
      )

  # 05 For plots with 1 variable only ####
  if (var == 1) {

    for (ftr_type in feature_types) {
      name_of_object <-
        paste0(
          "out_folder_"
          , var
          , "_var_plots_"
          , ftr_type
          , "_features"
        )

      name_of_folder <-
        paste0(
          out_folder_qa_plots
          , "/0"
          , var
          , "_var_plots/"
          , ftr_type
          , "_features"
        )
    # 06 For plots with 2 variables ####
      }
  } else if (var == 2) {

    for (ftr_ftr_res in feature_by_feature_resolution) {
      name_of_object <-
        paste0(
          "out_folder_",
          var,
          "_var_plots_",
          ftr_ftr_res
        )

      name_of_folder <-
        paste0(
          out_folder_qa_plots,
          "/0",
          var,
          "_var_plots/",
          ftr_ftr_res
        )
      # 07 Saving folder and object for each ftr_ftr_res ####
      assign(eval(name_of_object), name_of_folder)

      if (!dir.exists(name_of_folder)) {
        dir.create(name_of_folder)
      }
    }
  # 08 For plots with 3 variables ####
  } else if (var == 3) {
    name_of_object <-
      paste0(
        "out_folder_",
        var,
        "_var_plots"
      )

    name_of_folder <-
      paste0(
        out_folder_qa_plots,
        "/0",
        var,
        "_var_plots/"
      )
  }

  # 09 Saving folder objects for 1 var and 3 var plots ####
  assign(eval(name_of_object), name_of_folder)

  if (!dir.exists(name_of_folder)) {
    dir.create(name_of_folder)
  }
}

# 10 Changing wd since otherwise will create unnecessary html files on save ####
original_wd <- getwd()

# 11 specifying original variable subsets for plotting ####
original_vars <-
  names(preprocessed_data) %>%
    .[which(. == "selling_price") : which(. == "url")]

original_numeric_vars <-
  original_vars %>%
    .[(
        preprocessed_data[, .] %>%
        head %>%
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
      head() %>%
      sapply(is.numeric)
  )]

engineered_factor_vars <-
  engineered_vars %>%
  .[. %not_in% engineered_numeric_vars]

engnrd_fctr_vars_seeded_nmrc <-
  engineered_factor_vars %>%
    .[which(. == "area_missing_replaced") : which(. == "dayofweek_sold")]

# 13 Plots with 1 variable ####
for (ftr_type in feature_types) {
  if (ftr_type == "original") {
    setwd(out_folder_1_var_plots_original_features)
    vars <- original_numeric_vars
    } else if (ftr_type == "engineered") {
      setwd(out_folder_1_var_plots_engineered_features)
      vars <-
        engineered_numeric_vars %>%
        .[!grepl(
          paste0(c("city", engnrd_fctr_vars_seeded_nmrc), collapse = "|"), .
          )]
    }

  for (var in vars) {
    cat("Building density plot for:", var, "\n\n")
    temp_plot <-
      my_basic_density_plot(
        df = preprocessed_data[!is.na(preprocessed_data[[var]]), ],
        x = var,
        title = paste0("Density plot of ", var)
      )

    htmlwidgets::saveWidget(
      as_widget(temp_plot),
      paste0(var, "_density.html")
    )
  }
  setwd(original_wd)
}

# 14 Plots with 2 variables - categorical and intervalratio ####
setwd(out_folder_2_var_plots_cat_by_ir)

# 15 Only using engineered features since I trust them after 1 var plots ####
vars <- c("city", engnrd_fctr_vars_seeded_nmrc)
plot_types <- c("violin", "conditionaldensity")

for (var in vars) {
  targeted_df <- preprocessed_data[, c("selling_price", var)]
  top_31_in_var <-
    sqldf::sqldf(
      paste0(
        "select ", eval(var), ", count(*) as count
        from targeted_df
        group by ", eval(var), "
         order by count desc
        limit 31"
        )
      )
  targeted_df <- sqldf::sqldf(
    paste0(
      "select a.selling_price, a.", eval(var),
      " from targeted_df as a
      join top_31_in_var as t
        on a.", eval(var), " = t.", eval(var)
      )
    )
  message("Building plots for selling_price by:", var, "\n\n")
  for (plt in plot_types) {

    if (plt == "violin") {

      # 16 Violin plots ####
      cat("Building", plt, "plot.\n\n")
      temp_plot <-
        my_violin_plot(
          df = targeted_df %>% .[!is.na(.[[var]]), ],
          x = var,
          y = "selling_price",
          title = paste0("Violin plot for selling_price by ", var)
          )
    } else if (plt == "conditionaldensity") {

      # 17 Conditional density plots ####
      cat("Building", plt, "plot.\n\n")
      temp_plot <-
        my_conditional_density_plot(
          df = targeted_df %>% .[!is.na(.[[var]]), ],
          x = "selling_price",
          layers = var,
          title = paste0("Conditional density plot for selling_price by ", var)
        )
    }

    # 18 Saving the 2 var plots ####
    htmlwidgets::saveWidget(
      as_widget(temp_plot),
      paste0("selling_price_by_", var, "_", plt, ".html")
    )
  }
}

setwd(original_wd)

# 19 Plots with 2 variables - intervalratio and intervalratio ####
setwd(out_folder_2_var_plots_ir_by_ir)

# 20 Only using engineered features since I trust them after 1 var plots ####
vars <- c(engineered_numeric_vars)

for (var in vars) {

  # 21 Scatter plots ####
  cat("Building scatter plot for selling_price by:", var, "\n\n")
  temp_plot <-
    my_scatter_plot(
      df = preprocessed_data %>% .[!is.na(.[[var]]), ],
      x = var,
      y = "selling_price",
      title = paste0("Scatter plot for selling_price by ", var)
    )

  # 22 Saving the 2 var plots ####
  htmlwidgets::saveWidget(
    as_widget(temp_plot),
    paste0("selling_price_by_", var, "_scatterplot.html")
  )
  }

setwd(original_wd)


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