# Purpose: Define settings and load functions
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# 01 Ensure all pkgs in this script are installed ####
install.packages("here")
library("here")

# 02 Initialize wd and source functions not unique to this project ####
setwd(here())
source("../01_myFxnsMltplProjects.r")

# 03 Source other functions ####

for (r_file in list.files("04_fxnsThisProject/")) {

    if (
        # Skip self so as to avoid endless recursion
            r_file != "f01_sourceAllFxns.r"
        ) {
        source(paste0("04_fxnsThisProject/", r_file))
    }
}

# 02 Source scripts function ####
sourceScriptFromFolder <- function(script) {
    cat("Sourcing: ", script, "\n\n")
    source(paste0(scriptsFolder, script))
}

# 04 Basic settings ####

# Disable scientific notation
options(scipen = 999)