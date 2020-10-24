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

install_my_pkgs <- function(pkgs) {
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

install_my_pkgs(pkgs)

# 02 Initialize wd and source functions not unique to this project ####
setwd(here::here())
source("../01_my_fxns_mltpl_projects.r")

# 03 Source other functions ####

for (r_file in list.files("04_fxns_this_project/")) {

    if (
        # Skip self so as to avoid endless recursion
            r_file != "f01_settings_and_load_functions.r"
        ) {
        source(paste0("04_fxns_this_project/", r_file))
    }
}

# 04 Basic settings ####

# Disable scientific notation
options(scipen = 999)

# 05 Folder paths ####

scripts_folder <- paste0(getwd(), "/05_scripts/")
input_folder <- paste0(getwd(), "/06_inputs/")
output_folder <- paste0(getwd(), "/07_outputs/")

# Scraped data
output_folder_scraped_gparent <-
    paste0(
        output_folder,
        "01_scraped"
    )

if (!dir.exists(output_folder_scraped_gparent)) {
    dir.create(output_folder_scraped_gparent)
}

# Compiled data
output_folder_compiled <<-
    paste0(
        output_folder,
        "02_compiled/"
    )

if (!dir.exists(output_folder_compiled)) {
    dir.create(output_folder_compiled)
}

# Preprocessed data
out_folder_preprocessed_data <-
    paste0(
        output_folder,
        "04_preprocessed_data/"
    )

if (!dir.exists(out_folder_preprocessed_data)) {
    dir.create(out_folder_preprocessed_data)
}

# Preprocessed data
out_folder_orange_explorations <-
    paste0(
        output_folder,
        "05_orange_explorations/"
    )

if (!dir.exists(out_folder_orange_explorations)) {
    dir.create(out_folder_orange_explorations)
}

# 05 Source inputs ####

mappings_xlsx <-
    paste0(input_folder, "i01_mappings.xlsx")

# 06 Features for one-hot encoding - used in at least two scripts ####
features_for_ohe <-
    c(
        "city",
        "area_consolidated_low_freq_generalized",
        "street_low_freq_generalized",
        "agent_name_low_freq_generalized",
        "agency_low_freq_generalized",
        "quarterofyear_sold",
        "monthofyear_sold",
        "monthofquarter_sold",
        "weekofyear_sold",
        "weekofquarter_sold",
        "weekofmonth_sold",
        "dayofyear_sold",
        "dayofquarter_sold",
        "dayofmonth_sold",
        "dayofweek_sold"
    )

# 07 Cat variables for time series analysis - used in at least 2 scripts ####
categorical_variables_for_tsa <-
    c(
        "city",
        "area_consolidated_low_freq_generalized",
        "street_low_freq_generalized",
        "agent_name_low_freq_generalized",
        "agency_low_freq_generalized"
    )