# Purpose: Preparing variable groups for building models 3 and 4
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
  )

activatePkgs(pkgs)

# 02 Load data prepped for Model 3 and 4 ####

# 03 Variables for building predictive models ####
outcomeVarModel3and4 <- "annualGrowthRate"

allPredictorVarsModel3and4 <-
  union_all(#DATA GROUPS FOR SUBSETTING HERE##
  ) %>%
  unique() %>%
  str_subset("annualGrowthRate", negate = T) %>%
  sort()

# 04 limiting the number of predictor variables ####
selectedPredictorVarsModel3and4 <-
  union_all(#DATA GROUPS FOR SUBSETTING HERE##
  ) %>%
  unique() %>%
  str_subset("annualGrowthRate", negate = T) %>%
  sort()

# 05 Saving var groups for Models 3 and 4 ####
mySaveVarGroups(outFolderDataForModel3and4,
                outcomeVarModel3and4)

mySaveVarGroups(outFolderDataForModel3and4,
                allPredictorVarsModel3and4)

mySaveVarGroups(outFolderDataForModel3and4,
                selectedPredictorVarsModel3and4)

# 07 Subsetting based on var groups ####
allDataForModel3and4 <-
  #DATA LOADED AT BEGINNING OF SCRIPT HERE#
    [, c(outcomeVarModel3and4, allPredictorVarsModel3and4)]

selectedPredictorVarsDataForModel3and4 <-
  # DATA LOADED AT BEGINNING OF SCRIPT HERE#
    [, c(outcomeVarModel3and4, selectedPredictorVarsModel3and4)]

# 08 Saving data objects ####
mySaveVarGroups(
  outFolderDataForModel3and4,
  allDataForModel3and4
)

mySaveVarGroups(
  outFolderDataForModel3and4,
  selectedPredictorVarsDataForModel3and4
)
