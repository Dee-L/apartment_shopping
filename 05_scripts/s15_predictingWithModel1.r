# Purpose: Make predictions on newdata with model 1
# Author: David Gray Lassiter, PhD
# Date: 2020-nov-16
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "sqldf",
        "lubridate",
        "RcppRoll",
        "caret",
        "openxlsx"
    )

activatePkgs(pkgs)

# 02 load latest compiled data ####
dataToPredict <-
    paste0(
        outFolderDataForPredictingWithModel1,
        list.files(outFolderDataForPredictingWithModel1) %>%
            .[length(.)]
    ) %>%
    readRDS()

# 03 Specify which kind of model to use ####
modelToUse <- 
    # 'lackAskingPrice'
    'haveAskingPrice'

# 04 Identify the relevant metrics files ####
relevantMetricsFiles <-
    list.files(outFolderBuildingModel1, recursive = T) %>%
        str_subset(modelToUse) %>%
        str_subset('finalMetrics') %>%
        paste0(outFolderBuildingModel1, '/', .)

# 05 find the best metrics file ####
for (metricFile in relevantMetricsFiles) {

    # 06 If first file, set it as best ####
    if (metricFile == relevantMetricsFiles[1]) {
        bestFile <- metricFile
        bestRsq <-
            relevantMetricsFiles %>%
            str_subset(metricFile) %>%
            readRDS %>%
            .$.estimate %>%
            .[2]
    } else {
        # 07 Get metric to compare to ####
        comparatorRsq <-
            relevantMetricsFiles %>%
                str_subset(metricFile) %>%
                readRDS() %>%
                .$.estimate %>%
                .[2]
        # 08 If new metric is as good or better, update best metric file ####
        if (comparatorRsq >= bestRsq) {
            bestFile <- metricFile
            bestRsq <- comparatorRsq
        }
    }
}

# 09 Get the best model and confidence interval data ####
bestPath <- left(metricFile, nchar(metricFile) - nchar("finalMetrics"))

bestFit <- paste0(bestPath, 'finalFit') %>% readRDS

bestConfIntData <- paste0(bestPath, "confIntervalData") %>% readRDS()

# Found out that the models I trained used aggregated columns that 
# did not include "thisdate" in the names, and thus they won't match my
# newly-preprocessed data.... Trying instead to use a 'fewVars' model...

myFit <-
    readRDS('07_outputs//12_buildingModel1/01_localExportData//m2_haveAskingPrice_gridsize16_fewVars_shrink25/finalFit')

myConfInt <-
    readRDS('07_outputs//12_buildingModel1/01_localExportData//m2_haveAskingPrice_gridsize16_fewVars_shrink25/confIntervalData')

percentiles <-
    myConfInt %>%
    select(-meanResidual, -medianResidual)

# 10 Make point prediction ####
predictions <- dataToPredict

predictions[["prediction"]] <-
    predict(myFit, new_data = predictions)$`.pred`

# 11 Add percentile predictions ####
predictions <-
    predictions %>%
    mutate(
        percentile2.5 = prediction + percentiles$percentile2.5,
        percentile10 = prediction + percentiles$percentile10,
        percentile16 = prediction + percentiles$percentile16,
        percentile84 = prediction + percentiles$percentile84,
        percentile90 = prediction + percentiles$percentile90,
        percentile97.5 = prediction + percentiles$percentile97.5,
    ) %>%
    select(
        percentile2.5,
        percentile10,
        percentile16,
        prediction,
        percentile84,
        percentile90,
        percentile97.5,
        everything()
    )
predictions %>% names %>% head(30)
# 12 Make and save a small data subset in excel ####
predictions %>%
    select(percentile2.5 : urlRawData) %>%
    write.xlsx(
        paste0(
            predictionsFromModel1,
            '01_predictions_',
            today8Digit(),
            '.xlsx'
            )
        )

# 13 Pivoting the data from wide to long format ####



# 15 Prepare data for going into Model 2 ####
dataForModel2 <-
    predictions %>%
    rename(sellingPriceRawData = prediction)



# 16 Once have data predictions, pivot to longer ####


# Learning how to pivot a dataframe from wide to long
longDf <-
    data.frame(
        id = c(rep(1, 3), rep(2, 3))
        , constant = c(rep('alpha', 3), rep('beta', 3))
        , varies = c('alow', 'amed', 'ahigh', 'blow', 'bmid', 'bhigh')
        )

wideDf <-
    data.frame(
        id = c(1, 2),
        constant = c('alpha', 'beta'),
        low = c('alow', 'blow'),
        mid = c('amid', 'bmid'),
        high = c('ahigh', 'bhigh')
    )




# 06 Make prediction with ranges ####

predictions <- dataToPredict

predictions[["id"]] <- seq.int(nrow(predictions))
predictions[['predType']] <- NA
predictions[["prediction"]] <- NA

preds <-
    c(
        "percentile2.5",
        "percentile10",
        "percentile16",
        "prediction",
        "percentile84",
        "percentile90",
        "percentile97.5"
    )

# Paused here. Wanted to pivot so that I could get one row per prediction.
# Goal would be to save the object and later just extract the one with 'prediction' as 'sellingPriceRawData' to use as input into model2 so that I don't have to do feature engineering all over before making predictions for model2.

for (id in seq_along(predictions[['id']])) {

    tempDf <- predictions[predictions[['id']] == id, ]

    tempDf <- do.call("rbind", replicate(7, tempDf, simplify = FALSE))

    for (pred in seq_along(preds)) {

        tempDf[['predtype']]
        predictions[["predtype"]][pred] <- preds[[pred]]
        if (preds[[pred]] == 'prediction') {
            predictions[['prediction']][pred] <-
                predict(finalFit, new_data = predictions)$`.pred`
        } else {
            predictions[["prediction"]][pred] <-
                predict(finalFit, new_data = predictions)$`.pred` +
                confIntervalData[[preds[[pred]]]]
        }
    }
}

predictions[["prediction"]] <-
    predict(finalFit, new_data = predictions)$`.pred`

predictions <-
    predictions %>%
    mutate(
        lowestGuess = prediction + confIntervalData[['percentile2.5']],
        lowGuess = prediction + confIntervalData[["percentile10"]],
        midlowGuess = prediction + confIntervalData[["percentile16"]],
        midhighGuess = prediction + confIntervalData[["percentile84"]],
        highGuess = prediction + confIntervalData[["percentile90"]],
        highestGuess = prediction + confIntervalData[["percentile97.5"]],
    ) %>% 
    select(lowestGuess, lowGuess, midlowGuess, prediction, midhighGuess, highGuess, highestGuess, (predictions %>% names() %>% str_subset("RawData")))

# 07 Save results ####

nameOfResultsDf <-
    paste0(
        "predictions_",
        today8Digit()
    )

write.xlsx(dataToPredict, paste0(predictionsFromModel1, nameOfResultsDf, '.xlsx'))

