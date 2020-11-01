# Purpose: Splitting into train and test data
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c("sqldf")

activatePkgs(pkgs)

# 02 Load latest preprocessed data ####

preprocessedData <-
  paste0(
    outFolderPreprocessedAllData,
    list.files(outFolderPreprocessedAllData) %>%
      .[length(.)]
  ) %>%
  readRDS()

# 04 Do 80% : 20% split for training : testing ####
# Use my function

# 05 Save the datasets ####

saveAsRObjectWithItsName(outputFolderTrainTestSplit, trainingData)
saveAsRObjectWithItsName(outputFolderTrainTestSplit, testData)