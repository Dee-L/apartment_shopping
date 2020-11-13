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

# 06 Data for Models 1 and 2####
outFolderDataForModel1and2 <-
    paste0(
        outputFolder,
        "06_dataForModel1and2/"
    )

makeDirIfDoesNotExist(outFolderDataForModel1and2)

outFolderOrangeDataForModel1and2 <-
    paste0(
        outFolderOrange,
        "o02_dataForModel1and2/"
    )

makeDirIfDoesNotExist(outFolderOrangeDataForModel1and2, recursive = T)

# 07 Data for Model 3 ####
outFolderDataForModel3and4 <-
    paste0(
        outputFolder,
        "08_dataForModel3and4/"
    )

makeDirIfDoesNotExist(outFolderDataForModel3and4)

outFolderOrangeDataForModel3and4 <-
    paste0(
        outFolderOrange,
        "o03_dataForModel3and4/"
    )

makeDirIfDoesNotExist(outFolderOrangeDataForModel3and4, recursive = T)
