# Purpose: Make predictions on newdata with model 2
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
        outFolderDataForPredictingWithModel2,
        list.files(outFolderDataForPredictingWithModel2) %>%
            .[length(.)]
    ) %>%
    readRDS()

# Manually selecting the best fit model ####

myFit <-
    readRDS('07_outputs//22_buildingModel2/01_localExportData/m4_haveAskingPrice_gridsize32_fewVars_shrink1/finalFit')

falsePositiveRateData <-
    readRDS("07_outputs//22_buildingModel2/01_localExportData/m4_haveAskingPrice_gridsize32_fewVars_shrink1/falsePositiveRateData")

# 10 Make point prediction ####
predictions <- dataToPredict

predictions[["profitable"]] <-
    predict(myFit, new_data = predictions)$`.pred_class`

# 12 Make and save a small data subset in excel ####
dataToSave <-
    predictions %>%
    relocate(profitable) %>%
    select(profitable : urlRawData) %>%
    write.xlsx(
        paste0(
            predictionsFromModel1,
            '01_predictions_',
            today8Digit(),
            '.xlsx'
            )
        )

# 07 Save results ####
nameOfResultsDf <-
    paste0(
        "predictions_",
        today8Digit()
    )

write.xlsx(dataToSave, paste0(predictionsFromModel2, nameOfResultsDf, ".xlsx"))
