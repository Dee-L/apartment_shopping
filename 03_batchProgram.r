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
sourceScriptFromFolder("s01_scrapingHemnet.R")

# 04 Re-scrape failed pages ####
# sourceScriptFromFolder("s02_rescrapeFailedPages.r")

# 05 Compiling dataframe from rda files ####
sourceScriptFromFolder("s03_compilingDataframeFromRdaFiles.r")

# 06 Getting summaries from the compiled data ####
sourceScriptFromFolder("s04_summaries.r")

# 07 Preprocess data and engineer features ####
sourceScriptFromFolder("s05_mainFeatureEngineering.r")

# 08 QA plots ####
sourceScriptFromFolder("s07_defineVariableGroups.R")

# Data splits - sellingPrice vs increase in price and train/test for both

# 11 Train/test/cross-validation split ####

# 15 Put machine to sleep ####
shell(cmd = "rundll32.exe powrprof.dll,SetSuspendState 0,1,0")

# 12 EN model ####

# 13 RF model ####

# 14 NN model ####

# 15 Test models ####
