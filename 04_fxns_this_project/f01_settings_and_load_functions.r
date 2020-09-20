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
            break
        } else {
            install.packages(pkg)
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
        all(
            r_file != "f01_settings_and_load_functions.r"
            # Skip my plots for now since have not solved ggplot2 loading error
            , r_file != "f99_my_plots.r"
            )
        ) {
        source(paste0("04_fxns_this_project/", r_file))
    }
}

# 04 Basic settings ####

# Disable scientific notation
options(scipen = 999)

# Set paths to key folders
scripts_folder <- paste0(getwd(), "/05_scripts/")
input_folder <- paste0(getwd(), "/06_inputs/")
output_folder <- paste0(getwd(), "/07_outputs/")

# 05 Source inputs ####

mappings_xlsx <-
    paste0(input_folder, "i01_mappings.xlsx")