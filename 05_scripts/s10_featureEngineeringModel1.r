# Purpose: Preprocess and engineer new features
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
      "sqldf"
      , "lubridate"
      , "RcppRoll"
      , "caret"
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

# 03 Sourcing the macro to do all the feature engineering ####
source('11_macros/m01_engineerMyFeatures.r')

# 04 save the object ####
nameOfResultsDf <-
    paste0(
        "date_"
        , today8Digit()
    )

saveRDS(
  compiledData
  , paste0(
    outFolderPreprocessedAllData
    , eval(nameOfResultsDf)
    )
  )

# 05 save as CSV for examining with Orange ####

write.csv(
  compiledData,
  paste0(
    outFolderOrangeAllData,
    eval(nameOfResultsDf),
    ".csv"
  )
)
