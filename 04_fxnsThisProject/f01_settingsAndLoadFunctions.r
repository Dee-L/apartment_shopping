# Purpose: Define settings and load functions
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####

# Note, this fxn is also in `../01_my_fxns_mltpl_projects.r`, but is created
# and called already here in order to ensure that sourcing the `here` function
# works when referring to `../01_my_fxns_mltpl_projects.r`

installMyPkgs <- function(pkgs) {
    for (pkg in seq_along(pkgs)) {
        if (pkgs[pkg] %in% installed.packages()) {
            next
        } else {
            install.packages(pkgs[pkg])
        }
    }
}

pkgs <-
    c(
        "here"
    )

installMyPkgs(pkgs)

# 02 Initialize wd and source functions not unique to this project ####
setwd(here::here())
source("../01_myFxnsMltplProjects.r")

# 03 Source other functions ####

for (r_file in list.files("04_fxnsThisProject/")) {

    if (
        # Skip self so as to avoid endless recursion
            r_file != "f01_settingsAndLoadFunctions.r"
        ) {
        source(paste0("04_fxnsThisProject/", r_file))
    }
}

# 04 Basic settings ####

# Disable scientific notation
options(scipen = 999)

# 05 Folder paths ####

scriptsFolder <- paste0(getwd(), "/05_scripts/")
inputFolder <- paste0(getwd(), "/06_inputs/")
outputFolder <- paste0(getwd(), "/07_outputs/")

# Scraped data
outputFolderScrapedGparent <-
    paste0(
        outputFolder,
        "01_scraped"
    )

makeDirIfDoesNotExist(outputFolderScrapedGparent)

# Compiled data
outputFolderCompiled <<-
    paste0(
        outputFolder,
        "02_compiled/"
    )

makeDirIfDoesNotExist(outputFolderCompiled)

# Preprocessed data for all data
outFolderPreprocessedAllData <-
    paste0(
        outputFolder,
        "04_preprocessedAllData/"
    )

makeDirIfDoesNotExist(outFolderPreprocessedAllData)

# Preprocessed data for resold properties
outFolderPreprocessedResoldProps <-
    paste0(
        outputFolder,
        "05_preprocessedResoldProps/"
    )

makeDirIfDoesNotExist(outFolderPreprocessedAllData)

# Orange explorations
outFolderOrangeExplorations <-
    paste0(
        outputFolder,
        "06_orangeExplorations/"
    )

makeDirIfDoesNotExist(outFolderOrangeExplorations)

# train/test split data
outputFolderDataSplits <-
    paste0(
        outputFolder,
        "07_dataSplits/"
    )

# 05 Source inputs ####

mappingsXlsx <-
    paste0(inputFolder, "i01_mappings.xlsx")

# 06 Features for one-hot encoding - used in at least two scripts ####
featuresForOhe <-
    c(
        "city",
        "areaConsolidatedLowFreqGeneralized",
        "streetLowFreqGeneralized",
        "agentNameLowFreqGeneralized",
        "agencyLowFreqGeneralized",
        "quarterOfYearSold",
        "monthOfYearSold",
        "monthOfQuarterSold",
        "weekOfYearSold",
        "weekOfQuarterSold",
        "weekOfMonthSold",
        "dayOfYearSold",
        "dayOfQuarterSold",
        "dayOfMonthSold",
        "dayOfWeekSold"
    )

# 07 Cat variables for time series analysis - used in at least 2 scripts ####
catVarsForTimeSeriesAnalysis <-
    c(
        "city",
        "areaConsolidatedLowFreqGeneralized",
        "streetLowFreqGeneralized",
        "agentNameLowFreqGeneralized",
        "agencyLowFreqGeneralized"
    )
