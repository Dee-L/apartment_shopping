# Purpose: Preprocess and engineer new features for resold properties
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

# 02 Load latest preprocessed data ####
preprocessedAllData <-
  paste0(
    outFolderPreprocessedAllData,
    list.files(outFolderPreprocessedAllData) %>%
      .[length(.)]
  ) %>%
  readRDS()

# 03 Subsetting to properties that were resold in the timeframe ####
preprocessedResoldProps <-
  preprocessedAllData %>%
  # counting how many times each property appears
    count(!!!syms(identifiersForUniqueProp)) %>%
  # Filtering those that appear only once (not resold in my dataset)
    filter(n > 1) %>%
  # Dropping the column that counted the number of times they appeared in data
    select(-n) %>%
  # Bring back in dateSoldPsdData and sellingPriceRawData columns
    inner_join(preprocessedAllData[,
      c(identifiersForUniqueProp,
        "dateSoldPsdData", "sellingPriceRawData")]) %>%
  # Grouping by the unique property identifiers
    group_by(!!!syms(identifiersForUniqueProp)) %>%
  # Arranging by date
    arrange(!!!syms(identifiersForUniqueProp), dateSoldPsdData) %>%
  # Using dateSoldPsdData column to calculate other columns
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
    , valueIncreasePerYear = (changeInValue) / daysElapsed * 365.25
    , valueIncreaseAfter1YearMinusCost =
        valueIncreasePerYear - firstSellingPrice
  ) %>%
  # Dropping properties that resold in less than half a year
  filter(daysElapsed >= 365.25 / 2) %>%
  # Ungrouping data
  ungroup() %>%
  # Moving the column that I want to predict to the front
  relocate(valueIncreaseAfter1YearMinusCost) %>%
  # Dropping columns that won't be used in my predictive model
  select(
    -firstDateSold
    , -lastDateSold
    , -firstSellingPrice
    , -lastSellingPrice
    , -daysElapsed
    , -changeInValue
    , -dailyGrowthRate
    , annualGrowthRate
    , valueIncreasePerYear
    )

# 60 save the object ####
nameOfResultsDf <-
  paste0(
    "date_",
    today8Digit()
  )

saveRDS(
  xxx,
  paste0(
    outFolderPreprocessedResoldProps,
    eval(nameOfResultsDf)
  )
)

# 61 save as CSV for examining with Orange ####

write.csv(
  xxx,
  paste0(
    outFolderOrangeResoldProps,
    eval(nameOfResultsDf),
    ".csv"
  )
)