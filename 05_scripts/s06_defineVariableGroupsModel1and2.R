# Purpose: Preparing variable groups for building models 1 and 2
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

# 10 Variables for building predictive models ####
outcomeVarModel1and2 <- "sellingPriceRawData"

allPredictorVarsModel1and2 <-
  union_all(conVarsForPlots, conVarsForAggs, discreteVarsForHeatmaps) %>%
  unique %>%
  str_subset("sellingPriceRawData", negate = T) %>%
  sort

# 11 limiting the number of predictor variables ####
selectedPredictorVarsModel1and2 <-
  union_all(conVarsForPlots, discreteVarsForHeatmaps) %>%
  unique %>%
  str_subset("sellingPriceRawData", negate = T) %>%
  sort

# 12 Saving var groups for Models 1 and 2 ####
mySaveVarGroups(outFolderDataForModel1and2,
                outcomeVarModel1and2)

mySaveVarGroups(outFolderDataForModel1and2,
                allPredictorVarsModel1and2)

mySaveVarGroups(outFolderDataForModel1and2,
                selectedPredictorVarsModel1and2)

# 07 Subsetting based on var groups ####
allDataForModel1and2 <-
  preprocessedAllData[, c(outcomeVarModel1and2, allPredictorVarsModel1and2)]

selectedPredictorVarsDataForModel1and2 <-
  preprocessedAllData[, c(outcomeVarModel1and2, selectedPredictorVarsModel1and2)]

# 08 Saving data objects ####
mySaveVarGroups(outFolderDataForModel1and2,
                allDataForModel1and2)

mySaveVarGroups(outFolderDataForModel1and2,
                selectedPredictorVarsDataForModel1and2)
