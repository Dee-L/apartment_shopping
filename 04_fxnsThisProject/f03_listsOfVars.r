# Purpose: Lists of variables
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# 01 Cat Variables for Time Series Analysis ####
catVarsForTimeSeriesAnalysis <-
    c(
        "city",
        "areaConsolidatedLowFreqGeneralized",
        "streetLowFreqGeneralized",
        "agentNameLowFreqGeneralized",
        "agencyLowFreqGeneralized"
    )

# 02 Variables for building predictive models ####
varToPredictModel1 <- "sellingPriceRawData"

varToPredictModel2 <- "annualGrowthRate"

# 03 Identifiers for unique properties ####

identifiersForUniqueProp <-
    c(
        "yearBuiltRawData",
        "cityCleanedStringPsdData",
        "areaConsolidatedPsdData",
        "streetCleanedStringPsdData",
        "floorNonhalfDroppedPsdData",
        "roomsRawData",
        "kvmRawData"
    )