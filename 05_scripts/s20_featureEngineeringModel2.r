# Purpose: Preprocess and engineer new features for resold properties
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
      )

activatePkgs(pkgs)

# 02 Load data for this app if it isn't already loaded ####

if (any(
  !exists("allCompleteData")
  , !exists("outcomeVar")
  , !exists('allPredictors')
  , !exists('manyPredictors')
  , !exists('fewPredictors')
)) {
  source("05_scripts/s06_defineVariableGroupsModel1.R")
}

# 02 Renaming main dataframe ####
dataForModel <- allCompleteData


# 03 Identifiers for unique properties ####
identifiersForUniqueProp <-
  c(
    "yearBuiltImputedFromRandomSamplePsdData",
    "cityCleanedStringPsdData",
    "areaCleanedStringPsdData",
    "streetCleanedStringPsdData",
    "floorNonhalfDroppedImputedFromRandomSamplePsdData",
    "roomsRawData",
    "kvmImputedFromRandomSamplePsdData"
  )

# 04 Subsetting to properties that were resold in the timeframe ####
dataForModel <-
  dataForModel %>%
  # counting how many times each property appears
    count(!!!syms(identifiersForUniqueProp)) %>%
  # Filtering those that appear only once (not resold in my dataset)
    filter(n > 1) %>%
  # Dropping the column that counted the number of times they appeared in data
    select(-n) %>%
  # Bring back in dateSoldPsdData and sellingPriceRawData columns for processing
    inner_join(allCompleteData[,
      c(identifiersForUniqueProp,
        "dateSoldPsdData", "sellingPriceRawData")]) %>%
  # Grouping by the unique property identifiers
    group_by(!!!syms(identifiersForUniqueProp)) %>%
  # Arranging by date
    arrange(!!!syms(identifiersForUniqueProp), dateSoldPsdData) %>%
  # Using dateSoldPsdData column to calculate other columns
  # Way to improve this would be to take most recent instead of just first and last... Would have more records
    summarize(
      firstDateSold = first(dateSoldPsdData)
      , lastDateSold = last(dateSoldPsdData)
      , firstSellingPrice = first(sellingPriceRawData)
      , lastSellingPrice = last(sellingPriceRawData)
      ) %>%
  # Calculating various other columns
  mutate(
    daysElapsed = lastDateSold - firstDateSold
    , changeInValue = lastSellingPrice - firstSellingPrice
    , dailyGrowthRate =
        exp(log(lastSellingPrice / firstSellingPrice) / daysElapsed) - 1
    , annualGrowthRate = (1 + dailyGrowthRate) ^ 365.25 - 1
    , profitable = ifelse(annualGrowthRate > 0, T, F)
  ) %>%
  # Dropping properties that resold in less than half a year
  filter(daysElapsed >= 365.25 / 2) %>%
  # Ungrouping data
  ungroup() %>%
  # Moving the column that I want to predict to the front
  relocate(profitable, annualGrowthRate) %>%
  # Dropping columns that won't be used in my predictive model
  select(
    -firstDateSold
    , -lastDateSold
    , -firstSellingPrice
    , -lastSellingPrice
    , -daysElapsed
    , -changeInValue
    , -dailyGrowthRate
    # , -annualGrowthRate
    ) %>%
  # Bringing back in all of the other columns
  inner_join(allCompleteData) %>%
  arrange(annualGrowthRate)

# 04 Constants for scaling annualGrowthRate ####
minAnnualGrowthRate <- min(dataForModel$annualGrowthRate)
maxAnnualGrowthRate <- max(dataForModel$annualGrowthRate)

# 05 Adding annualGrowthRateScaled ####
dataForModel <-
  dataForModel %>%
    mutate(
      annualGrowthRateScaled = (annualGrowthRate - minAnnualGrowthRate) / (maxAnnualGrowthRate - minAnnualGrowthRate)
    ) %>%
    relocate(profitable, annualGrowthRate, annualGrowthRateScaled)

# 06 save the object ####
nameOfResultsDf <-
  paste0(
    "date_",
    today8Digit()
  )

saveRDS(
  dataForModel,
  paste0(
    outFolderPreprocessedDataModel2,
    eval(nameOfResultsDf)
  )
)

# 06 save as CSV for examining with Orange ####
write.csv(
  dataForModel,
  paste0(
    outFolderOrangeDataForModel2,
    eval(nameOfResultsDf),
    ".csv"
  )
)