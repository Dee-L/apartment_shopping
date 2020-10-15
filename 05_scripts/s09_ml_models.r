# Choosing a model:
# Regression enhanced random forest?:
# https://neptune.ai/blog/random-forest-regression-when-does-it-fail-and-why

# Confidence intervals for random forest regression:
# # https://blog.datadive.net/prediction-intervals-for-random-forests/#:~:text=A%20general%20method%20for%20finding,methods%20is%20Quantile%20Regression%20Forests.&text=For%20example%2C%20the%2095%25%20prediction,response%20variables%20in%20the%20leaves.
# Expand trees all the way out - see distribution of real values at each leaf, take 2.5 - 97.5% interval in each leaf as your 95% prediction interval.
# Does not depend on homogeneity of variance, but is empirically derived from your data.


# tidymodels in R?
# https://www.tidymodels.org/

# Hyperparameters to tune with RF:
    # number of trees
    # depth of trees
    # size of subsets
    # number of features at each split

# Hyperparameters to tune with linear regression:
    # alpha / regularization strength
    # lambda / Lasso L1 to Ridge L2


# ARIMA model for time-series analysis?
# Long short-term memory neural network for time-series analysis?
# https://medium.com/analytics-steps/introduction-to-time-series-analysis-time-series-forecasting-machine-learning-methods-models-ecaa76a7b0e3

# More on time-series analysis in R:
# https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/

# https://www.statworx.com/at/blog/time-series-forecasting-with-random-forest/

# source("D:/Coding/R/Public/pkgs_and_fxns/load_pkgs_and_fxns.R")


# Likely need to install:
# 'glmnetUtils'
# cva.glmnet.formula


#Commented out this entire block since Mode won't fit predictions from cva.glmnet
#Loop to create EN models and plots for each of the forecast dates
# for (i in 1 : length(forecast_dfs)) {
#   
#     if (startsWith(forecast_dfs[i], 'data_for_daily')) {
#     y <- 'meetings_this_day'
#     final_models$day[i] <- daily_forecasts_rows$day[i]
#     } else if (startsWith(forecast_dfs[i], 'data_for_weekly')) {
#         y <- 'meetings_next_007days'
#         final_models$day[i] <- weekly_forecast_row$day[i]}  
#   
#   #Taking out the training and testing data.
#   #Unlike for SW model building, the x dataframe for EN must include the outcome.
#   EN_training_days <-
#      get(paste0(forecast_dfs[i], '_EN_training')) %>%
#        .[ , which(names(.) %in% c('day'))]
#
#   EN_training_xy <-
#      get(paste0(forecast_dfs[i], '_EN_training')) %>%
#        .[ , -which(names(.) %in% c('day'))]
#   
#   EN_training_y <-
#     get(paste0(forecast_dfs[i], '_EN_training')) %>%
#     .[ , which(names(.) %in% c(y))]
#
#   EN_testing_days <-
#     get(paste0(forecast_dfs[i], '_EN_testing')) %>%
#     .[ , which(names(.) %in% c('day'))]
#
#   EN_testing_xy <-
#     get(paste0(forecast_dfs[i], '_EN_testing')) %>%
#     .[ , -which(names(.) %in% c('day'))]
#   
#   EN_testing_y <-
#     get(paste0(forecast_dfs[i], '_EN_testing')) %>%
#     .[ , which(names(.) %in% c(y))]
#   
#   #Elastic net regression.
#   #Unlike the SW model, cannot refer to an object here (y), but must 
#   #explicitly state it
#   if (startsWith(forecast_dfs[i], 'data_for_daily')) {
#     EN_model <- cva.glmnet.formula(meetings_this_day~., data = EN_training_xy) 
#     } else if (startsWith(forecast_dfs[i], 'data_for_weekly')) {
#         EN_model <- cva.glmnet.formula(meetings_next_007days~.,
#                                        data = EN_training_xy) }
#   
#   #initialize vector to store mean-squared errors from different alphas models
#   MSEs_for_alphas <- numeric()
# 
#   #Loop to populate vector of MSEs
#   for (j in (1 : length(EN_model$alpha))) {
#     MSEs_for_alphas[j] <-
#       mean(
#         (EN_testing_y -
#            predict(EN_model, EN_testing_xy, EN_model$alpha[j])
#          ) ^ 2, na.rm = T)
#     if (j == length(EN_model$alpha))
#   {rm(j)}
#   }
#   
#   #Determine which alpha to use in the model
#   EN_alpha_to_use <-
#     EN_model$alpha[which(MSEs_for_alphas == min(MSEs_for_alphas))]
#   
#   #Save model
#   assign(paste0(forecast_dfs[i], '_EN_model'), EN_model,
#          envir = .GlobalEnv)
# 
#   #Save model name in df
#   final_models$EN_model[i] <- paste0(forecast_dfs[i], '_EN_model')
# 
#   #Save alpha
#   assign(paste0(forecast_dfs[i], '_EN_alpha_to_use'), EN_alpha_to_use,
#          envir = .GlobalEnv)
#   
#   #Save alpha in df
#   final_models$EN_alpha_to_use[i] <- EN_alpha_to_use
#       
#   #Save MSE in df
#   final_models$EN_MSE[i] <- min(MSEs_for_alphas)
#   
#   #Save the summary
#   assign(paste0(forecast_dfs[i], '_EN_model_summary'), summary(EN_model),
#          envir = .GlobalEnv)
#   
#   #Save model name in df
#   final_models$EN_model_summary[i] <-
#     paste0(forecast_dfs[i], '_EN_model_summary')
#   
#   #Create plot of prediction vs testing data
#   testing_plot <-
#     plot_ly(x = EN_testing_xy$day,
#             marker = list(color = ('blue'))) %>%
#     add_markers(y = EN_testing_y,
#                 name = 'historical data') %>%
#     add_lines(y = predict(EN_model, EN_testing_xy, EN_alpha_to_use),
#               line = list(color = 'green'),
#               name = 'model',
#               marker = NULL) %>%
#     layout(title = paste0('EN ',
#                           final_models$forecast[i],
#                           ': performance on test data'),
#            legend = list(x = 0.05, y = 0.95))
#   
#   #Save the testing plot
#   assign(paste0(forecast_dfs[i], '_EN_testing_plot'), testing_plot,
#          envir = .GlobalEnv)
#   
#   #Save testing plot name in df
#   final_models$EN_testing_plot[i] <- paste0(forecast_dfs[i], '_EN_testing_plot')
#   
#   #Get low and high ends of prediction range
#   estimated_CI <-
#     quantile(EN_testing_y - predict(EN_model, EN_testing_xy, EN_alpha_to_use), na.rm = T,
#              c(0.05, 0.95))
#   
#   final_models$EN_CI_0.05[i] <- estimated_CI[[1]]
#   
#   final_models$EN_CI_0.95[i] <- estimated_CI[[2]]
#   
#   final_models$EN_point_prediction[i] <-
#     predict(get(final_models$EN_model[i]),
#             source_data[source_data$day == final_models$day[i], ],
#             EN_alpha_to_use)[[1]]
#   
#   final_models$EN_low_prediction[i] <-
#    final_models$EN_point_prediction[i] + final_models$EN_CI_0.05[i]
#   
#   final_models$EN_high_prediction[i] <-
#    final_models$EN_point_prediction[i] + final_models$EN_CI_0.95[i]
#   
#   if (startsWith(final_models$forecast[i], 'data_for_daily')) {
#     
#     final_models$EN_message[i] <-
#     paste0('Predicted number of meetings in Sweden for ',
#            final_models$day[i],
#            ' is ',
#            round(final_models$EN_point_prediction[i], 0),
#            '. It should be between ',
#            round(final_models$EN_low_prediction[i], 0),
#            ' and ',
#            round(final_models$EN_high_prediction[i], 0))
#     
#     } else {
#       
#       final_models$EN_message[i] <-
#     paste0('Predicted number of meetings in Sweden from ',
#            Sys.Date(),
#            ' to ',
#            Sys.Date() + 6,
#            ' is ',
#            round(final_models$EN_point_prediction[i], 0),
#            '. It should be between ',
#            round(final_models$EN_low_prediction[i], 0),
#            ' and ',
#            round(final_models$EN_high_prediction[i], 0)) }
#   
#   if (i == length(forecast_dfs))
#   {rm(i, y, EN_training_y, EN_training_x, EN_testing_y, EN_testing_x, EN_model,
#       MSEs_for_alphas, EN_alpha_to_use, EN_testing_plot,
#       estimated_CI)}
# }
#--




#Commented out since EN not working in Mode
# #Make the plots for EN predictions
# plot_ly(x = final_daily_models$day) %>%
#   add_lines(y = final_daily_models$EN_high_prediction,
#             line = list(color = '33ff33'),
#             name = 'high',
#             showlegend = T,
#             marker = NULL) %>%
#   add_markers(y = final_daily_models$EN_point_prediction,
#               name = 'prediction',
#               marker = list(color = 'cccc00')) %>%
#   add_lines(y = final_daily_models$EN_low_prediction,
#                 line = list(color = 'ff3333'),
#                 name = 'low') %>%
#   layout(title = paste0('EN daily forecast'),
#          legend = list(x = 0.05, y = 0.95))
# 
# plot_ly(x = factor(c('low', 'prediction', 'high'),
#                    levels = c('low', 'prediction', 'high')),
#         y = c(final_weekly_models$EN_low_prediction,
#               final_weekly_models$EN_point_prediction,
#               final_weekly_models$EN_high_prediction),
#         marker = list(color = c('ff3333', 'cccc00', '33ff33'))) %>%
#   layout(title = paste0('EN forecast for next 7 days'),
#          legend = list(x = 0.05, y = 0.95))
#--



#Another kind of model to try: elastic net (flexible blend of lasso and ridge).
#I built code to do Elastic Net, which was working locally, but failed on Mode.
#I asked Mode support to add the library 'glmnetUtils'.