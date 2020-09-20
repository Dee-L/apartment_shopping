# Purpose: Source a script from the scripts folder
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:


# 01 Source scripts function ####
source_script_from_folder <- function(script) {
    cat("Sourcing: ", script, "\n\n")
    source(paste0(scripts_folder, "/", script))
}