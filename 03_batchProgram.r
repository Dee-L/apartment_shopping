# Purpose: Run all of the underlying scripts as necessary
# Author: David Gray Lassiter, PhD
# Date: YYYY-MMM-DD
# Version:

# 01 Source all my functions ####
source("04_fxnsThisProject/f01_sourceAllFxns.r")

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c()

activatePkgs(pkgs)

# 03 Scrape Hemnet ####
# They apparently changed the layout so scraping agency fails. This script
# Needs to be modified.
# sourceScriptFromFolder("s01_scrapingHemnet.R")

# 04 Re-scrape failed pages ####
# sourceScriptFromFolder("s02_rescrapeFailedPages.r")

# 05 Compiling dataframe from rda files ####
sourceScriptFromFolder("s03_compilingDataframeFromRdaFiles.r")

# 06 Getting summaries from the compiled data ####
sourceScriptFromFolder("s04_summaries.r")

# 07 Preprocess data and engineer features ####
sourceScriptFromFolder("s05_mainFeatureEngineering.r")

# 08 Define variable groups ####
sourceScriptFromFolder("s06_defineVariableGroups.R")

# 09 Prep data for Model 1: Predicting Selling Price ####
sourceScriptFromFolder("s07_prepDataModel1.R")

# 10 Prep data for Model 2: Predicting Selling Price without Asking Price ####
sourceScriptFromFolder("s07_prepDataModel1.R")

# 11 Prep data for Model 3: Predicting ROI ####
sourceScriptFromFolder("s08_prepDataModel2.R")

# 12 Build Model 1: Predicting Selling Price ####
sourceScriptFromFolder("s15_model1PredictSellingPrice.R")

# 13 Build Model 2: Predicting ROI ####
sourceScriptFromFolder("s16_model2PredictSellingRoi.R")

# 14 Predict with Models ####
sourceScriptFromFolder("s17_predictWithModels.R")

# 15 Put machine to sleep ####
shell(cmd = "rundll32.exe powrprof.dll,SetSuspendState 0,1,0")
