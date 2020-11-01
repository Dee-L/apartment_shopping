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
        "04_preprocessedAllData/"
    )

makeDirIfDoesNotExist(outFolderPreprocessedAllData)

# 05 Orange explorations ####
outFolderOrange <-
    paste0(
        outputFolder,
        "05_orange/"
    )

outFolderOrangeAllData <-
    paste0(
        outFolderOrange,
        "o01_allData/"
    )

makeDirIfDoesNotExist(outFolderOrangeAllData, recursive = T)

# 06 Preprocessed data for resold properties ####
outFolderPreprocessedResoldProps <-
    paste0(
        outputFolder,
        "06_preprocessedResoldProps/"
    )

makeDirIfDoesNotExist(outFolderPreprocessedResoldProps)

outFolderOrangeResoldProps <-
    paste0(
        outFolderOrange,
        "o02_resoldProps/"
    )

makeDirIfDoesNotExist(outFolderOrangeResoldProps, recursive = T)

# 07 train/test split data ####
outputFolderDataSplits <-
    paste0(
        outputFolder,
        "07_dataSplits/"
    )