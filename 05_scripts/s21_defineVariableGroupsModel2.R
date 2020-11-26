# Purpose: Preparing variable groups for building model 2
# Author: David Gray Lassiter, PhD
# Date: 2020-nov-16
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
  )

activatePkgs(pkgs)

# 02 Load data prepped for Model 3 and 4 ####
dataForModel <-
  paste0(
    outFolderPreprocessedDataModel2,
    list.files(outFolderPreprocessedDataModel2) %>%
      .[length(.)]
  ) %>%
  readRDS()

# 03 All complete data ####
allCompleteData <-
  dataForModel %>%
  .[, colSums(is.na(.)) == 0]

mySaveVarGroups(outFolderDataForModel2, allCompleteData)

# 03 Variables for building predictive models ####
outcomeVar <- "profitable"

mySaveVarGroups(outFolderDataForModel2, outcomeVar)

# 04 Scaled outcomeVar ####
scaledOutcomeVar <- 'annualGrowthRateScaled'

mySaveVarGroups(outFolderDataForModel2, scaledOutcomeVar)

# 05 Drop intermediate calculated columns ####
allPredictors <-
  allCompleteData %>%
  names() %>%
  str_subset("profitable|annualGrowthRate|RawData|CleanedString|English|floorImputed|ConsolidatedPsd|Mean|Median|Sum",
    negate = T
  ) %>%
  union_all('sellingPriceRawData', "cityCleanedStringPsdData", "yearSoldRawData", aggCols) %>%
  sort() %>%
  unique

mySaveVarGroups(outFolderDataForModel2, allPredictors)

# 06 Drop one-hot encoding columns ####
manyPredictors <-
  allPredictors %>%
  str_subset("Ohe", negate = T) %>%
  unique

mySaveVarGroups(outFolderDataForModel2, manyPredictors)

# 07 Drop columns that did aggregated calculations ####
fewPredictors <-
  manyPredictors %>%
  str_subset("Mean|Median|Sum", negate = T) %>%
  unique

mySaveVarGroups(outFolderDataForModel2, fewPredictors)
