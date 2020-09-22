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

# 04 Compiling dataframe from rda files ####
source_script_from_folder("s02_compiling_dataframe_from_rda_files.r")

# 05 Preprocess data ####
source_script_from_folder("s02_preprocessing_data.r")

# 06 Replacing NAs ####
source_script_from_folder("s03_replacing_NAs.r")

# 07 Feature Engineering ####
source_script_from_folder("s04_feature_engineering.r")

# 08 One-hot encoding of non linear variables ###

# 09 QA plots ####
source_script_from_folder("s06_qa_plots.r")

# 10 Train/test/cross-validation split ####

# 11 EN model ####
source_script_from_folder("s08_en_model.r")

# 12 RF model ####

# 13 NN model ####

# 14 Test models ####