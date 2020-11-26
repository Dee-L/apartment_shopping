# Purpose: Preparing variable groups for building model 1
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
  )

activatePkgs(pkgs)

# 02 load latest preprocessed data ####
preprocessedAllData <-
  paste0(
    outFolderPreprocessedAllData,
    list.files(outFolderPreprocessedAllData) %>%
      .[length(.)]
  ) %>%
  readRDS()

# 03 Testing data classes ####
allNumericVars <-
  preprocessedAllData %>%
    select_if(is.numeric) %>%
    names

allFactorVars <-
  preprocessedAllData %>%
  select_if(is.factor) %>%
  names()

allOtherVars <-
  preprocessedAllData %>%
  select(!c(allNumericVars, allFactorVars)) %>%
  names

if (length(allOtherVars) > 0) message("Some variables are misclassified")

# 06 Variables for density plots ####
allVars <-
  names(preprocessedAllData)

originalVars <-
  str_subset(allVars, "RawData")

originalNumericVars <-
  intersect(originalVars, allNumericVars)

engnrdVars <-
  str_subset(allVars, "PsdData")

engnrdNumericVars <-
  intersect(engnrdVars, allNumericVars)

engnrdNumericAggByCat <-
  str_subset(engnrdNumericVars, "Mean|Median|Sum")

engnrdNumericNotAggByCat <-
  setdiff(engnrdNumericVars, engnrdNumericAggByCat)

numericVarsForDensityPlots <-
   sort(c(originalNumericVars, engnrdNumericNotAggByCat))

# 07 Cat variables for violin, strip, and conditional density plots ####
engnrdFactorVars <-
  intersect(engnrdVars, allFactorVars)

catVarsForPlots <-
  engnrdFactorVars %>%
  str_subset("Ohe", negate = T) %>%
  str_subset("monthSoldEnglish", negate = T) %>%
  str_subset("Generalized|Sold") %>%
  c("cityRawData", .)

oheVars <-
  engnrdFactorVars %>%
  str_subset("Ohe")

# 08 Con variables for violin, strip, and conditional density plots ####
engnrdNonhalfNotDropped <-
  engnrdNumericNotAggByCat %>%
    str_subset("floor") %>%
    str_subset("Nonhalf", negate = T)

conVarsForPlots <-
  str_subset(engnrdNumericNotAggByCat, "Imputed") %>%
  setdiff(engnrdNonhalfNotDropped) %>%
  sort %>%
  c("sellingPriceRawData", .)

# 09 Variables for heatmaps ####
discreteVarsForHeatmaps <-
  str_subset(conVarsForPlots, "kvm|room|floor") %>%
  str_subset("avgift|runningCosts", negate = T) %>%
  c(catVarsForPlots, "yearSoldRawData")

conVarsForAggs <-
  engnrdNumericVars %>%
  str_subset("Mean|Median|Sum") %>%
  str_subset("Interpolated")

catVarsForAggPlots <-
  catVarsForPlots %>%
  str_subset(catVarsForTimeSeriesAnalysis)

# 10 All complete data ####
allCompleteData <-
  preprocessedAllData %>%
  .[ , colSums(is.na(.)) == 0]

mySaveVarGroups(outFolderDataForModel1, allCompleteData)

# 11 Agg columns that will be kept later ####
aggCols <-
  allCompleteData %>%
  names %>%
  str_subset("Mean|Median|Sum") %>%
  str_subset("Interpolated")

# 12 Outcome variable ####
outcomeVar <- "sellingPriceRawData"

mySaveVarGroups(outFolderDataForModel1, outcomeVar)

# 13 Drop intermediate calculated columns ####
allPredictors <-
  allCompleteData %>%
  names %>%
  str_subset('RawData|CleanedString|English|floorImputed|ConsolidatedPsd|Mean|Median|Sum',
             negate = T) %>%
  union_all('cityCleanedStringPsdData', 'yearSoldRawData', aggCols) %>%
  sort

mySaveVarGroups(outFolderDataForModel1, allPredictors)

# 13 Drop one-hot encoding columns ####
manyPredictors <-
  allPredictors %>%
  str_subset('Ohe', negate = T)

mySaveVarGroups(outFolderDataForModel1, manyPredictors)

# 14 Drop columns that did aggregated calculations ####
fewPredictors <-
  manyPredictors %>%
  str_subset('Mean|Median|Sum', negate = T)

mySaveVarGroups(outFolderDataForModel1, fewPredictors)
