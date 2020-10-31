# Purpose: Plot some of my features to get a sense of the data
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
    "htmlwidgets"
    , "sqldf"
  )

installMyPkgs(pkgs)

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
    dplyr::select_if(is.numeric) %>%
    names

allFactorVars <-
  preprocessedData %>%
  dplyr::select_if(is.factor) %>%
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
  stringr::str_subset(allVars, "RawData")

originalNumericVars <-
  intersect(originalVars, allNumericVars)

engnrdVars <-
  stringr::str_subset(allVars, "PsdData")

engnrdNumericVars <-
  intersect(engnrdVars, allNumericVars)

engnrdNumericAggByCat <-
  stringr::str_subset(engnrdNumericVars, "Mean|Median|Sum")

engnrdNumericNotAggByCat <-
  setdiff(engnrdNumericVars, engnrdNumericAggByCat)

numericVarsForDensityPlots <-
   sort(c(originalNumericVars, engnrdNumericNotAggByCat))

# 07 Cat variables for violin, strip, and conditional density plots ####
engnrdFactorVars <-
  intersect(engnrdVars, allFactorVars)

catVarsForPlots <-
  engnrdFactorVars %>%
  stringr::str_subset("Ohe", negate = T) %>%
  stringr::str_subset("monthSoldEnglish", negate = T) %>%
  stringr::str_subset("generalized|sold") %>%
  c("cityRawData", .)

# 08 Con variables for violin, strip, and conditional density plots ####


# Paused here, need to make sure capturing Nonhalf floor dropped only in
# all relevant var sets that are being plotted later. I started
# working on removing the floors that don't have Nonhalf floors dropped
# below but didn't finish making it or incorporating it into
# conVarsForPlots

engnrdNonhalfNotDropped <-
  engnrdNumericNotAggByCat %>%
    stringr::str_subset("floor") %>%
    stringr::str_subset("Nonhalf", negate = T)

conVarsForPlots <-
  stringr::str_subset(engnrdNumericNotAggByCat, "Imputed") %>%
  setdiff(engnrdNonhalfNotDropped) %>%
  sort %>%
  c("sellingPriceRawData", .)

# 08 Variables for heatmaps ####
discreteVarsForHeatmaps <-
  stringr::str_subset(conVarsForPlots, "kvm|room|floor") %>%
  stringr::str_subset("avgift|runningCosts", negate = T) %>%
  c(catVarsForPlots, "yearSoldRawData")

conVarsForAggs <-
  engnrdNumericVars %>%
  stringr::str_subset("mean|median|sum") %>%
  stringr::str_subset("agoPsdData|daysPsdData", negate = T)

catVarsForAggPlots <-
  catVarsForPlots %>%
  stringr::str_subset(catVarsForTimeSeriesAnalysis)
