# Purpose: Define functions and parameters for this project
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c()

activatePkgs(pkgs)

# 01 Save as r object with its name ####

saveAsRObjectWithItsName <- function(object, folderToSaveIn) {
    fileName <-
        paste0(folderToSaveIn, deparse(substitute(object)), ".rds")
    saveRDS(object, file = fileName)
}


# 02 Return contents of RData file, allowing reassignment to a new name ####
reassignRda <- function(fileName) {
    load(fileName)
    get(ls()[ls() != "fileName"])
}

# 03 Saving variable groups ####

mySaveVarGroups <- function(exportPath, object) {
    objectName <- deparse(substitute(object))

    saveRDS(
        object,
        paste0(exportPath, objectName)
    )
}
