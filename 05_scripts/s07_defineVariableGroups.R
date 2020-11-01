# Purpose: Plot some of my features to get a sense of the data
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
    "htmlwidgets"
    , "sqldf"
  )

activatePkgs(pkgs)

# 02 load latest preprocessed data ####
preprocessedData <-
  paste0(
    outFolderPreprocessedAllData,
    list.files(outFolderPreprocessedAllData) %>%
      .[length(.)]
  ) %>%
  readRDS()

# 03 Testing data classes ####
allNumericVars <-
  preprocessedData %>%
    select_if(is.numeric) %>%
    names

allFactorVars <-
  preprocessedData %>%
  select_if(is.factor) %>%
  names()

allOtherVars <-
  preprocessedData %>%
  select(!c(allNumericVars, allFactorVars)) %>%
  names

if (length(allOtherVars) > 0) message("Some variables are misclassified")

# 06 Variables for density plots ####
allVars <-
  names(preprocessedData)

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
  str_subset("agoPsdData|daysPsdData", negate = T)

catVarsForAggPlots <-
  catVarsForPlots %>%
  str_subset(catVarsForTimeSeriesAnalysis)

# 10 Variables for building predictive models ####
varToPredictModel1 <- "valueIncreaseAfter1YearMinusCost"

varToPredictModel2 <- "sellingPriceRawData"

predictorVars <-
  union_all(catVarsForPlots, conVarsForPlots, catVarsForAggPlots, conVarsForAggs, discreteVarsForHeatmaps) %>%
  unique %>%
  str_subset("sellingPriceRawData", negate = T)
