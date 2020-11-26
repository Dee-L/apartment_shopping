# Purpose: Send new data through same preprocessing as data used to model
# Author: David Gray Lassiter, PhD
# Date: 2020-nov-16
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
      "sqldf"
      , "lubridate"
      , "RcppRoll"
      , "caret"
      , 'openxlsx'
      )

activatePkgs(pkgs)

# 02 load latest compiled data ####
for (file in list.files(outputFolderCompiled)) {
  if (file == list.files(outputFolderCompiled)[1]) {
    compiledData <- readRDS(paste0(outputFolderCompiled, file))
  }
  else {
    newCompiledData <- readRDS(paste0(outputFolderCompiled, file))
    compiledData <- rbind(compiledData, newCompiledData)
  }
}

# 03 Bring in my data to predict ####
askingType <- 'have'

newData <- read.xlsx(paste0(predictionsFromModel1, "i03_inputForModel2.xlsx"))

compiledData <- rbind(newData, compiledData)

# 04 Sourcing the macro to do all the feature engineering ####
source('11_macros/m01_engineerMyFeatures.r')

# 05 Limiting the results back to just the newdata ####
dataToSave <- compiledData[compiledData$sellingPriceRawData %in% newData$sellingPrice, ]

# 06 save the object ####
nameOfResultsDf <-
    paste0(askingType,
        "AskingObjectsToPredict_"
        , today8Digit()
    )

saveRDS(
  dataToSave
  , paste0(
    outFolderDataForPredictingWithModel2
    , eval(nameOfResultsDf)
    )
  )
