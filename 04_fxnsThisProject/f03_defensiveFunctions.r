# Purpose: Defensive functions for trying to dodge bugs
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Try wait retry, good for web scraping to avoid getting blocked ####

tryWaitRetry <-
    function(expr, maxTries = 10) {
        for (try in 1:maxTries) {
            results <- try(expr = expr, silent = TRUE)
            if (inherits(results, "try-error")) {
                wait <- runif(n = 1, min = 0, max = 2 ^ (try - 1))
                wait %<>%
                    round(., 1)
                message("Try ", try, " failed. Waiting for ", wait, " seconds.")
                Sys.sleep(wait)
            } else {
                break
            }
        }
        if (try == maxTries & inherits(results, "try-error")) {
            results <- NULL
        }
        results
    }

# 02 Try 2 expressions ####

try2Expressions <-
    function(expr1, expr2) {
        results <- try(expr = expr1, silent = TRUE)
        if (inherits(results, "try-error")) {
            message("First expression failed; trying expression 2 instead.")
            results <- expr2
        } else {
            results <- expr1
        }
        results
    }

# 03 NA handling ####

# Give NA if empty to avoid having objects with length = 0
naIfEmpty <- function(x) {
    ifelse(length(x) == 1, x, NA)
}

# Give expected booleans if comparing potential NA values ####
compareNA <- function(v1, v2) {
    same <- (v1 == v2) | (is.na(v1) & is.na(v2))
    same[is.na(same)] <- FALSE
    return(same)
}
