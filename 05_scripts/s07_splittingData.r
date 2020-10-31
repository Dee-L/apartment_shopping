# Purpose: Splitting into train and test data
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c("sqldf")

installMyPkgs(pkgs)

# 02 Load latest preprocessed data ####

preprocessedData <-
  paste0(
    outFolderPreprocessedAllData,
    list.files(outFolderPreprocessedAllData) %>%
      .[length(.)]
  ) %>%
  readRDS()




# 03 Set the random seed ####
set.seed(412)

# 04 Do 80% : 20% split for training : testing ####
sampleSize <- floor(0.8 * nrow(preprocessedData))

indexForTrainingData <-
    sample(seq_len(nrow(preprocessedData)), size = sampleSize)

trainingData <- preprocessedData[indexForTrainingData, ]
testData <- preprocessedData[-indexForTrainingData, ]

# 05 Save the datasets ####

saveAsRObjectWithItsName(outputFolderTrainTestSplit, trainingData)
saveAsRObjectWithItsName(outputFolderTrainTestSplit, testData)