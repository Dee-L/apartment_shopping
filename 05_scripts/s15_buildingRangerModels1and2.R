# Purpose: Building Ranger Random Forest Models 1 or 2
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Inspired by:
# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://juliasilge.com/blog/xgboost-tune-volleyball/
# https://juliasilge.com/blog/wind-turbine/

# 01 Ensure all pkgs in this script are installed ####
install.packages('here')
install.packages('tidymodels')
install.packages('ranger')
install.packages('xgboost')
install.packages('doParallel')
install.packages("vip")

library('here')
library('tidymodels')
library('ranger')
library('xgboost')
library('doParallel')
library('vip')

# 02 Load data ####
setwd(here())

importPath <- '07_outputs/06_dataForModel1and2/'

originalExportPath <- '07_outputs/07_buildingModel1and2/01_localExportData/'

exportPath <- originalExportPath

allDataForModel1and2 <- readRDS(paste0(importPath, 'allDataForModel1and2'))

selectedPredictorVarsDataForModel1and2 <-
    readRDS(paste0(importPath, 'selectedPredictorVarsDataForModel1and2'))

# 03 load latest var lists ####
outcomeVarModel1and2 <- readRDS(paste0(importPath, 'outcomeVarModel1and2'))

# 04 My save function ####
# I define the function in place in case I move this script to the cloud
# and will not be able to refer to local functions
mySave <- function(object) {

    objectName <- deparse(substitute(object))

    saveRDS(
        object,
        paste0(exportPath, objectName)
    )
}

testSave <- 1 + 1

mySave(testSave)

rm(testSave)

# 05 Set parameters for model-building ####
modelVersions <- c(1, 2)

datasets <- c('SelectedVars', 'ALL')

recordsShrinkFactors <- c(100, 50, 25, 12, 6, 3, 2, 1)

gridsizes <- c(4, 8, 16, 32)

# 06 Build grid of models for looping ####
gridOfModels <-
    expand.grid(modelVersions, gridsizes, datasets, recordsShrinkFactors)

names(gridOfModels) <-
    c('modelVersions', 'gridsizes', 'datasets', 'recordsShrinkFactors')

registerDoParallel(detectCores() - 2)
# 07 Start looping ####
for (model in
     1:2
     # seq_len(nrow(gridOfModels))
     ) {
    modelVersion <<- gridOfModels$modelVersions[model]
    gridsize <<- gridOfModels$gridsizes[model]
    dataset <<- gridOfModels$datasets[model]
    recordsShrinkFactor <<- gridOfModels$recordsShrinkFactors[model]

    modelParameters <<- paste0(
                              'modelVersion', modelVersion,
                              '_gridsize', gridsize,
                              '_dataset', dataset,
                              '_shrink', recordsShrinkFactor
    )

    newExportPath <<- paste0(paste0(originalExportPath, modelParameters, "/"))

    dir.create(newExportPath)

    exportPath <<- newExportPath

    cat("\n\n\nWORKING ON:\n", modelParameters, "\n\n\n")

    # 08 Subsetting variables ####
    if (dataset == 'ALL') {
        dataForModel <<- allDataForModel1and2
    } else if (dataset == 'SelectedVars') {
        dataForModel <<- selectedPredictorVarsDataForModel1and2
    }

    # 09 Dropping asking price predictor if model 2 ###
    if (modelVersion == 2) {

        dataForModel <<-
            dataForModel %>%
            select(-contains("askingPrice"))
    }

    # 10 Shrinking number of records ####
    sampleSize <<- floor(nrow(dataForModel) / recordsShrinkFactor)

    set.seed(412)
    dataForModel <<- dataForModel[sample(nrow(dataForModel), sampleSize), ]

    # 11 Splitting data ####
    set.seed(412)
    dataSplit <<- initial_split(dataForModel)
    dataTraining <<- training(dataSplit)
    dataTesting <<- testing(dataSplit)

    # 12 Make 'recipe' for model ####
    dataRecipe <<-
        recipe(dataTraining) %>%
        update_role(!!!syms(names(dataTraining)), new_role = "predictor") %>%
        update_role(!!!syms(outcomeVarModel1and2), new_role = "outcome")

    # 13 Make a 'prepared' object ####
    dataPrep <<- prep(dataRecipe)

    # 14 'Bake' the prepared object ####
    dataBaked <<- bake(dataPrep, new_data = NULL)

    # 15 Specify 'ranger', hyperparameter tuning, grid specifications ####
    tuneSpec <<-
        rand_forest(
            mode = 'regression',
            mtry = tune(),
            trees = tune(),
            min_n = tune()
        ) %>%
        set_engine('ranger')

    tuneGrid <<-
        grid_latin_hypercube(
            finalize(mtry(), dataTraining),
            trees(),
            min_n(),
            size = gridsize
        )

    tuneWorkflow <<-
        workflow() %>%
        add_recipe(dataRecipe) %>%
        add_model(tuneSpec)

    # 16 Specify cross-validation data ####
    set.seed(412)
    dataFolds <<- vfold_cv(dataTraining)

    # 17 Do initial exploratory tuning - this will take a long time ####
    t1 <<- Sys.time()
    tuneResults <<-
        tune_grid(
            tuneWorkflow,
            resamples = dataFolds,
            grid = tuneGrid,
            control = control_grid(save_pred = TRUE)
        )
    t2 <<- Sys.time()
    cat("Time elapsed with parameters", modelParameters, ":\n")
    print(t2 - t1)

    # 18 save the results ####

    mySave(tuneResults)

    # 19 Visualize results ####
    plotGridRmse <-
        tuneResults %>%
            collect_metrics() %>%
            filter(.metric == "rmse") %>%
            select(mean, mtry : min_n) %>%
            pivot_longer(mtry : min_n,
                         values_to = "value",
                         names_to = "parameter"
            ) %>%
            ggplot(aes(value, mean, color = parameter)) +
            geom_point(alpha = 0.8, show.legend = F) +
            facet_wrap(~parameter, scales = "free_x") +
            labs(x = NULL, y = "RMSE", title = modelParameters)

    mySave(plotGridRmse)

    plotGridRsq <-
        tuneResults %>%
            collect_metrics() %>%
            filter(.metric == "rsq") %>%
            select(mean, mtry : min_n) %>%
            pivot_longer(mtry : min_n,
                         values_to = "value",
                         names_to = "parameter"
            ) %>%
            ggplot(aes(value, mean, color = parameter)) +
            geom_point(alpha = 0.8, show.legend = F) +
            facet_wrap(~parameter, scales = "free_x") +
            labs(x = NULL, y = "RSQ", title = modelParameters)

    mySave(plotGridRsq)

    print(show_best(tuneResults, "rmse"))
    print(show_best(tuneResults, "rsq"))

    # 20 Finalize model ####
    bestRmse <<- select_best(tuneResults, "rmse")

    finalModel <<-
        finalize_model(tuneSpec, bestRmse)

    # 21 Looking at variable importance in the model ####
    plotTopPredictors <-
        finalModel %>%
            set_engine('ranger', importance = "permutation") %>%
            fit(sellingPriceRawData ~ .,
                data = dataBaked
            ) %>%
            vip(geom = "col") +
            labs(title = modelParameters)

    mySave(plotTopPredictors)

    # 22 Get the final fit model for getting predictions ####
    set.seed(412)
    finalFit <<- fit(finalModel, sellingPriceRawData ~., dataTraining)

    mySave(finalFit)

    # 23 Get the final train/test results for evaluating your model ####
    finalResults <<- last_fit(finalModel, sellingPriceRawData ~., dataSplit)

    mySave(finalResults)

    # 24 Get metrics from train-test results ####
    finalMetrics <<- collect_metrics(finalResults)

    mySave(finalMetrics)

    # 25 Visualize residuals ####
    plotResiduals <-
        finalResults %>%
            collect_predictions() %>%
            ggplot(aes(sellingPriceRawData, .pred)) +
            geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
            geom_point(alpha = 0.6, color = "midnightblue") +
            coord_fixed() +
            labs(title = modelParameters)

    mySave(plotResiduals)

    # 26 Get data for confidence interval ####
    dataForConfInterval <- dataTesting

    dataForConfInterval[['predictions']] <-
        predict(finalFit, new_data = dataForConfInterval)

    # 27 Resaving in global scope ####
    dataForConfInterval <<- dataForConfInterval

    mySave(dataForConfInterval)

    confIntervalData <<-
        dataForConfInterval %>%
        mutate(
            predictions =
                predict(finalFit, new_data = .),
            residuals = sellingPriceRawData - predictions) %>%
        select(predictions, sellingPriceRawData, residuals) %>%
        summarize(meanResidual = mean(residuals$.pred),
                  medianResidual = median(residuals$.pred),
                  percentile2.5 = quantile(residuals$.pred, 0.025),
                  percentile10 = quantile(residuals$.pred, 0.10),
                  percentile16 = quantile(residuals$.pred, 0.16),
                  percentile84 = quantile(residuals$.pred, 0.84),
                  percentile90 = quantile(residuals$.pred, 0.90),
                  percentile97.5 = quantile(residuals$.pred, 0.975)
        )

    print(confIntervalData)

    mySave(confIntervalData)

    # 28 Clean up
    rm(bestRmse, confIntervalData, dataBaked, dataFolds, dataForConfInterval,
       dataForModel, dataPrep, dataRecipe, dataSplit, dataTesting, dataTraining,
       finalFit, finalMetrics, finalModel, finalResults, plotGridRmse,
       plotGridRsq, plotResiduals, plotTopPredictors, tuneGrid, tuneResults,
       tuneSpec, tuneWorkflow, dataset, exportPath, gridsize, model,
       modelVersion, newExportPath, recordsShrinkFactor, sampleSize, t1, t2)
}

# 29 Celebrate ####
cat('\n\nGreat job, you finished building all those models!\n\n')
