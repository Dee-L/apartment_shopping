# Purpose: Source a script from the scripts folder
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this scripts are installed ####
pkgs <-
    c()

installMyPkgs(pkgs)

# 02 Source scripts function ####
sourceScriptFromFolder <- function(script) {
    cat("Sourcing: ", script, "\n\n")
    source(paste0(scriptsFolder, script))
}
