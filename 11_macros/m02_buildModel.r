# Purpose: Script to build random forest model locally or on GCP
# Author: David Gray Lassiter, PhD
# Date: 2020-nov-20
# Version: 1.0


# 01 Make your input objects visible to this macro.
getAnywhere(importPath)
getAnywhere(originalExportPath)
getAnywhere(rfMode)
getAnywhere(startingModel)
getAnywhere(thisIsATestRun)

# 02 Set paths ####
exportPath <- originalExportPath

# 03 Load data ####
allCompleteData <- readRDS(paste0(importPath, "allCompleteData"))
outcomeVar <- readRDS(paste0(importPath, "outcomeVar"))

if (rfMode == 'classification') {
    scaledOutcomeVar <- readRDS(paste0(importPath, "scaledOutcomeVar"))
}

allPredictors <- readRDS(paste0(importPath, "allPredictors"))
manyPredictors <- readRDS(paste0(importPath, "manyPredictors"))
fewPredictors <- readRDS(paste0(importPath, "fewPredictors"))

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
modelVersions <- c(
    "lackAskingPrice"#,
    # "haveAskingPrice"
    )

datasets <- c("fewVars"
#, "manyVars", "allVars"
)

# 06 Set parameters that differ if test run ####
if (thisIsATestRun == T) {
    gridsizes <- c(2)
    recordsShrinkFactors <- c(500)
    startingModel <- 1

} else {
    gridsizes <- c(
        16, #already ran grid 16 with shrink 1 for haveAsking and fewVars, but it did not outperform grid32/shrink6 version.
    32)
    recordsShrinkFactors <- c(
        # 25,
        # 6#,
        4 #3 #2 # 1.5 this killed my pc
        )
}

# 07 Build grid of models for looping, set up parallel, and print some info ####
gridOfModels <-
    expand.grid(modelVersions, gridsizes, datasets, recordsShrinkFactors)

names(gridOfModels) <-
    c("modelVersions", "gridsizes", "datasets", "recordsShrinkFactors")

modelsToBuild <- startingModel : nrow(gridOfModels)

registerDoParallel(detectCores() - 2)

cat("\n\nworkers:", getDoParWorkers())
cat("\n\ngridOfModels:\n")
print(gridOfModels[1 : 10, ])

# 08 Start looping ####
for (model in modelsToBuild) {

    t1 <- Sys.time()
    cat(paste0("\n\nSys time is:", as.POSIXlt(Sys.time())))
    modelVersion <- gridOfModels$modelVersions[model]
    gridsize <- gridOfModels$gridsizes[model]
    dataset <- gridOfModels$datasets[model]
    recordsShrinkFactor <- gridOfModels$recordsShrinkFactors[model]

    modelParameters <- paste0(
        "m", model,
        "_", modelVersion,
        "_gridsize", gridsize,
        "_", dataset,
        "_shrink", recordsShrinkFactor
    )

    cat(
        "\n\n\nWORKING ON:\n", modelParameters, "out of", nrow(gridOfModels),
        "models.\n\n\n"
    )

    # 09 Subsetting variables ####
    if (dataset == 'fewVars') {
        varsForModel <- fewPredictors
    } else if (dataset == 'manyVars') {
        varsForModel <- manyPredictors
    } else if (dataset == 'allVars') {
        varsForModel <- allPredictors
    }

    # 10 Dropping asking price predictor if model lackAskingPrice ###
    if (modelVersion == 'lackAskingPrice') {
        varsForModel <-
            setdiff(varsForModel, "askingPriceImputedFromRandomSamplePsdData")
    } else if (modelVersion == 'haveAskingPrice') {
        varsForModel <- varsForModel
    }

    if (rfMode == 'regression') {
        dataForModel <-
            allCompleteData[, c(outcomeVar, varsForModel)]
        metric <- "rsq"
    } else if (rfMode == 'classification') {
        dataForModel <-
            allCompleteData[, c(outcomeVar, scaledOutcomeVar, varsForModel)]

        dataForModel[[outcomeVar]] <- as.factor(dataForModel[[outcomeVar]])
        metric <- "accuracy"
    }

    # 11 Shrinking number of records ####
    sampleSize <- floor(nrow(dataForModel) / recordsShrinkFactor)

    dataForModel <- dataForModel[sample(nrow(dataForModel), sampleSize), ]

    # 12 Splitting data ####
    dataSplit <- initial_split(dataForModel)
    dataTraining <- training(dataSplit)
    dataTesting <- testing(dataSplit)

    # 13 Make 'recipe' for model ####
    dataRecipe <-
        recipe(dataTraining) %>%
        update_role(!!!syms(names(dataTraining)), new_role = "predictor") %>%
        update_role(!!!syms(outcomeVar), new_role = "outcome")

    # 14 Make a 'prepared' object ####
    dataPrep <- prep(dataRecipe)

    # 15 'Bake' the prepared object ####
    dataBaked <- bake(dataPrep, new_data = NULL)

    # 16 Specify 'ranger', hyperparameter tuning, grid specifications ####
    tuneSpec <-
        rand_forest(
            mode = rfMode, #trying classification for predicting profitability
            mtry = tune(),
            trees = tune(),
            min_n = tune()
        ) %>%
        set_engine('ranger')

    tuneGrid <-
        grid_latin_hypercube(
            finalize(mtry(), dataTraining),
            trees(),
            min_n(),
            size = gridsize
        )

    tuneWorkflow <-
        workflow() %>%
        add_recipe(dataRecipe) %>%
        add_model(tuneSpec)

    # 17 Specify cross-validation data ####
    dataFolds <- vfold_cv(dataTraining)

    # 18 Do initial exploratory tuning - this will take a long time ####
    tuneResults <-
        tune_grid(
            tuneWorkflow,
            resamples = dataFolds,
            grid = tuneGrid,
            control = control_grid(save_pred = TRUE)
        )

    # 19 Finalize model ####
    bestTrainingModel <- select_best(tuneResults, metric)

    finalModel <-
        finalize_model(tuneSpec, bestTrainingModel)

    if (rfMode == 'regression') {
        # 20 Looking at variable importance in the model ####
        plotTopPredictors <-
            finalModel %>%
                set_engine('ranger', importance = "permutation") %>%
                fit_xy(x = dataBaked[varsForModel],
                y = dataBaked[outcomeVar]
                ) %>%
                vip(geom = "col") +
                labs(title = modelParameters)
    }

    # 21 Get the final fit model for getting predictions ####
    finalFit <- fit_xy(
                        finalModel,
                        x = dataTraining[varsForModel],
                        y = dataTraining[outcomeVar]
                        )

    # 22 Get the final train/test results for evaluating your model ####
    finalResults <- last_fit(finalModel, dataRecipe, dataSplit)

    # 23 Get metrics from train-test results ####
    finalMetrics <- collect_metrics(finalResults)

    # 24 Test if new model outperforms old model ####

    # 25 Control flow for 'lackAskingPrice' models ####
    if (modelVersion == "lackAskingPrice") {

        # 26 If first model, save as `lackAskingPriceBest` ####
        if (!exists("lackAskingPriceBest")) {
            cat(
                "\n\nModel", model,
                "is first 'lackAskingPrice' model. Saving model."
            )
            lackAskingPriceBest <-
                finalMetrics$.estimate[finalMetrics$.metric == metric]

        # 27 If not first, skip if not better than `lackAskingPriceBest` ####
        } else if (lackAskingPriceBest >
            finalMetrics$.estimate[finalMetrics$.metric == metric]) {
            cat(
                "\n\nModel", model,
                "is NOT better than former best 'lackAskingPrice' model.
                Skipping to next model."
            )
            t2 <- Sys.time()
            cat(paste0("\n\nSys time is:", as.POSIXlt(Sys.time())))
            cat("\n\nTime elapsed with parameters", modelParameters, ":\n")
            print(t2 - t1)
            next

        # 28 If not first and is new best, save as `lackAskingPriceBest` ####
        } else if (lackAskingPriceBest <=
            finalMetrics$.estimate[finalMetrics$.metric == metric]) {
            cat(
                "\n\nModel", model,
                "is better than former best 'lackAskingPrice' model.
                Saving model."
            )
            lackAskingPriceBest <-
                finalMetrics$.estimate[finalMetrics$.metric == metric]
            }

    # 29 Control flow for 'haveAskingPrice' models ####
    } else if (modelVersion == "haveAskingPrice") {

        # 30 If first model, save as `haveAskingPriceBest` ####
        if (!exists("haveAskingPriceBest")) {
            cat(
                "\n\nModel", model,
                "is first 'haveAskingPrice' model. Saving model."
            )
            haveAskingPriceBest <-
                finalMetrics$.estimate[finalMetrics$.metric == metric]

        # 31 If not first, skip if not better than `haveAskingPriceBest` ####
        } else if (haveAskingPriceBest >
            finalMetrics$.estimate[finalMetrics$.metric == metric]) {
            cat(
                "\n\nModel", model,
                "is NOT better than former best 'haveAskingPrice' model.
                Skipping to next model."
            )
            t2 <- Sys.time()
            cat(paste0("\n\nSys time is:", as.POSIXlt(Sys.time())))
            cat("\n\nTime elapsed with parameters", modelParameters, ":\n")
            print(t2 - t1)
            next

        # 32 If not first and is new best, save as `haveAskingPriceBest` ####
        } else if (haveAskingPriceBest <=
            finalMetrics$.estimate[finalMetrics$.metric == metric]) {
            cat(
                "\n\nModel", model,
                "is better than former best 'haveAskingPrice' model.
                Saving model."
            )
            haveAskingPriceBest <-
                finalMetrics$.estimate[finalMetrics$.metric == metric]
            }
    }

    newExportPath <- paste0(paste0(originalExportPath, modelParameters, "/"))
    dir.create(newExportPath)
    exportPath <- newExportPath

    # 33 Visualize metrics and residualsresiduals ####
    plotGridMetric <-
        tuneResults %>%
        collect_metrics() %>%
        filter(.metric == metric) %>%
        select(mean, mtry : min_n) %>%
        pivot_longer(mtry : min_n,
            values_to = "value",
            names_to = "parameter"
        ) %>%
        ggplot(aes(value, mean, color = parameter)) +
        geom_point(alpha = 0.8, show.legend = F) +
        facet_wrap(~parameter, scales = "free_x") +
        labs(x = NULL, y = metric, title = modelParameters)

    if (rfMode == 'regression') {
        plotResiduals <-
            finalResults %>%
            collect_predictions() %>%
            ggplot(aes_string(x = outcomeVar, y = ".pred")) +
            geom_abline(slope = 1, lty = 2, color = "gray50", alpha = 0.5) +
            geom_point(alpha = 0.6, color = "midnightblue") +
            coord_fixed() +
            labs(title = modelParameters)

        # 34 Get data for confidence interval ####
        dataForConfInterval <- dataTesting

        dataForConfInterval[["predictions"]] <-
            predict(finalFit, new_data = dataForConfInterval)

        # 35 Resaving in global scope ####
        dataForConfInterval <- dataForConfInterval

        confIntervalData <-
            dataForConfInterval %>%
            mutate(
                predictions =
                    predict(finalFit, new_data = .),
                residuals :=
                    !!sym(outcomeVar) - predictions
            ) %>%
            select(predictions, !!!syms(outcomeVar), residuals) %>%
            summarize(
                meanResidual = mean(residuals$.pred),
                medianResidual = median(residuals$.pred),
                percentile2.5 = quantile(residuals$.pred, 0.025),
                percentile10 = quantile(residuals$.pred, 0.10),
                percentile16 = quantile(residuals$.pred, 0.16),
                percentile84 = quantile(residuals$.pred, 0.84),
                percentile90 = quantile(residuals$.pred, 0.90),
                percentile97.5 = quantile(residuals$.pred, 0.975)
            )
        # 36 Print results ####
        cat("\n\nBest results were:")
        print(show_best(tuneResults, metric))
        cat("\n\n")
        print(finalMetrics)

        print(confIntervalData)
        mySave(plotResiduals)
        mySave(plotTopPredictors)
        mySave(dataForConfInterval)
        mySave(confIntervalData)
    } else if (rfMode == 'classification') {
        # 34 Get data for false positive rate ####
        dataForFalsePositiveRate <- dataTesting

        dataForFalsePositiveRate[["predictions"]] <-
            predict(finalFit, new_data = dataForFalsePositiveRate)

        # 35 Resaving in global scope ####
        dataForFalsePositiveRate <- dataForFalsePositiveRate

        falsePositiveRateData <-
            dataForFalsePositiveRate %>%
            mutate(
                predictions =
                    predict(finalFit, new_data = .),
                falsePositive :=
                    ifelse(predictions == T & !!sym(outcomeVar) != T, T, F)
            ) %>%
            select(predictions, !!!syms(outcomeVar), falsePositive) %>%
            summarize(
                avgFalsePositiveRate = mean(falsePositive)
            )

        # 34 Get data for false confidence ####
        dataForFalseConfidence <- dataTesting

        dataForFalseConfidence[["predictions"]] <-
            predict(finalFit, new_data = dataForFalseConfidence, type = "prob") %>%
            .$.pred_TRUE

        # 35 Resaving in global scope ####
        dataForFalseConfidence <- dataForFalseConfidence

        falseConfidenceData <-
            dataForFalseConfidence %>%
            mutate(
                predictions =
                    predict(finalFit, new_data = ., type = 'prob')$.pred_TRUE,
                residuals :=
                    !!sym(scaledOutcomeVar) - predictions
            ) %>%
            select(predictions, !!!syms(scaledOutcomeVar), residuals) %>%
            summarize(
                meanResidual = mean(residuals),
                medianResidual = median(residuals),
                percentile2.5 = quantile(residuals, 0.025),
                percentile10 = quantile(residuals, 0.10),
                percentile16 = quantile(residuals, 0.16),
                percentile84 = quantile(residuals, 0.84),
                percentile90 = quantile(residuals, 0.90),
                percentile97.5 = quantile(residuals, 0.975)
            )

        # 36 Print results ####
        cat("\n\nBest results were:")
        print(show_best(tuneResults, metric))
        cat("\n\n")
        print(finalMetrics)
        print(falsePositiveRateData)
        mySave(falsePositiveRateData)

        print(falseConfidenceData)
        mySave(falseConfidenceData)

    }

    # 37 save the results ####
    mySave(tuneResults)
    mySave(plotGridMetric)
    mySave(finalFit)
    mySave(finalResults)
    mySave(finalMetrics)

    t2 <- Sys.time()
    cat(paste0("\n\nSys time is:", as.POSIXlt(Sys.time())))
    cat("\n\nTime elapsed with parameters", modelParameters, ":\n")
    print(t2 - t1)

}


# 39 Celebrate ####
registerDoSEQ()
cat('\n\nYou can safely ignore any warnings about more columns being requested than there were predictors. The tuning procedure for mtry is driven by caret, which may attempt to use more predictors than is possible with a given dataset. This is okay since it then takes all of the predictors anyway while doing the tuning, which is not a bad thing to try.\n\n')

cat("\n\nGreat job, you finished building all those models!\n\n")