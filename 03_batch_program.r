# Purpose: Run all of the underlying scripts as necessary
# Author: David Gray Lassiter, PhD
# Date: YYYY-MMM-DD
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Source all my functions ####
source("04_fxns_this_project/f01_settings_and_load_functions.r")

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
    c()

install_my_pkgs(pkgs)

# 03 Scrape Hemnet ####
source_script_from_folder("s01_scraping_hemnet.r")

# 04 Re-scrape failed pages ####
# source_script_from_folder("s02_rescraping_failed_pages.r")

# 05 Compiling dataframe from rda files ####
source_script_from_folder("s03_compiling_dataframe_from_rda_files.r")

# 06 Getting summaries from the compiled data ####
source_script_from_folder("s04_summaries.r")

# 07 Preprocess data and engineer features ####
source_script_from_folder("s05_preprocessing_and_feature_engineering.r")

# 15 Put machine to sleep ####
shell(cmd = "rundll32.exe powrprof.dll,SetSuspendState 0,1,0")



# 08 QA plots ####
source_script_from_folder("s06_qa_plots.r")



# Data splits - selling_price vs increase in price and train/test for both

# 11 Train/test/cross-validation split ####

# 12 EN model ####
source_script_from_folder("s10_en_model.r")

# 13 RF model ####

# 14 NN model ####

# 15 Test models ####