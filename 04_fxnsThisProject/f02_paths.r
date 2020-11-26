# Purpose: Define settings and load functions
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# 01 Top-level folder paths ####
scriptsFolder <- paste0(getwd(), "/05_scripts/")
inputFolder <- paste0(getwd(), "/06_inputs/")
outputFolder <- paste0(getwd(), "/07_outputs/")

# 02 Scraped data ####
outputFolderScrapedGparent <-
    paste0(
        outputFolder,
        "01_scraped"
    )

makeDirIfDoesNotExist(outputFolderScrapedGparent)

# 03 Compiled data ####
outputFolderCompiled <<-
    paste0(
        outputFolder,
        "02_compiled/"
    )

makeDirIfDoesNotExist(outputFolderCompiled)

# 04 Preprocessed data for all data ####
outFolderPreprocessedAllData <-
    paste0(
        outputFolder,
        "10_preprocessedForModel1/"
    )

makeDirIfDoesNotExist(outFolderPreprocessedAllData)

# 05 Data for Model 1 ####
outFolderDataForModel1 <-
    paste0(
        outputFolder,
        "11_dataForModel1/"
    )

makeDirIfDoesNotExist(outFolderDataForModel1)

# 06 Building Model 1 ####
outFolderBuildingModel1 <-
    paste0(
        outputFolder,
        '12_buildingModel1'
    )

makeDirIfDoesNotExist(outFolderBuildingModel1, recursive = T)

# 07 Data to predict with Model 1 ####
outFolderDataForPredictingWithModel1 <-
    paste0(
        outputFolder,
        "13_dataForPredictingWithModel1/"
    )

makeDirIfDoesNotExist(outFolderDataForPredictingWithModel1)

# 08 Predictions from Model 1 ####
predictionsFromModel1 <-
    paste0(
        outputFolder,
        "14_predictionsFromModel1/"
    )

makeDirIfDoesNotExist(predictionsFromModel1)


# 09 Preprocessed data for Model 2 ####
outFolderPreprocessedDataModel2 <-
    paste0(
        outputFolder,
        "20_preprocessedForModel2/"
    )

makeDirIfDoesNotExist(outFolderPreprocessedDataModel2)

# 10 Data for Model 2 ####
outFolderDataForModel2 <-
    paste0(
        outputFolder,
        "21_dataForModel2/"
    )

makeDirIfDoesNotExist(outFolderDataForModel2)

# 11 Building Model 2 ####
outFolderBuildingModel2 <-
    paste0(
        outputFolder,
        "22_buildingModel2"
    )

makeDirIfDoesNotExist(outFolderBuildingModel2, recursive = T)

# 12 Data to predict with Model 2 ####
outFolderDataForPredictingWithModel2 <-
    paste0(
        outputFolder,
        "23_dataForPredictingWithModel2/"
    )

makeDirIfDoesNotExist(outFolderDataForPredictingWithModel2)

# 13 Predictions from Model 2 ####

predictionsFromModel2 <-
    paste0(
        outputFolder,
        "24_predictionsFromModel2/"
    )

makeDirIfDoesNotExist(predictionsFromModel2)


# 14 Orange explorations ####
outFolderOrange <-
    paste0(
        outputFolder,
        "30_orange/"
    )

outFolderOrangeAllData <-
    paste0(
        outFolderOrange,
        "o01_allData/"
    )

makeDirIfDoesNotExist(outFolderOrangeAllData, recursive = T)

outFolderOrangeDataForModel1 <-
    paste0(
        outFolderOrange,
        "o10_dataForModel1/"
    )

makeDirIfDoesNotExist(outFolderOrangeDataForModel1, recursive = T)


outFolderOrangeDataForModel2 <-
    paste0(
        outFolderOrange,
        "o20_dataForModel2/"
    )

makeDirIfDoesNotExist(outFolderOrangeDataForModel2, recursive = T)