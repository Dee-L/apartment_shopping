# Purpose: Define functions and parameters for this project
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Save as r object with its name ####

save_as_r_object_with_its_name <- function(object, folder_to_save_in) {
    file_name <-
        paste0(folder_to_save_in, deparse(substitute(object)), ".rds")
    saveRDS(object, file = file_name)
}
