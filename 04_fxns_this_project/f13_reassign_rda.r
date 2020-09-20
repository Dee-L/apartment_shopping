# Purpose: Return an RDA object allow you to assign it to a new name
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# Source for this was:
# https://stackoverflow.com/questions/5577221/
# how-can-i-load-an-object-into-a-variable-name
# -that-i-specify-from-an-r-data-file

# 01 Return contents of RData file, allowing reassignment to a new name ####
reassign_rda <- function(file_name) {
            load(file_name)
            get(ls()[ls() != "file_name"])
}