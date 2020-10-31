# Purpose: Preprocess and engineer new features
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
      "sqldf"
      , "dplyr"
      , "lubridate"
      , "RcppRoll"
      , "caret"
      , "stringr"
      )

installMyPkgs(pkgs)

library(caret)

# 02 Load latest preprocessed data ####

preprocessedData <-
  paste0(
    outFolderPreprocessedAllData,
    list.files(outFolderPreprocessedAllData) %>%
      .[length(.)]
  ) %>%
  readRDS()

# 03 Subsetting to properties that were resold in the timeframe ####

propsAppearingMoreThanOnce <-
  preprocessedData %>%
    select(cityRawData, areaConsolidatedPsdData,
      streetRawData, kvmRawData, roomsRawData,
      floorRawData, yearBuiltRawData
      ) %>%
    count(cityRawData, areaConsolidatedPsdData,
      streetRawData, kvmRawData, roomsRawData,
      floorRawData, yearBuiltRawData) %>%
    filter(n > 1) %>%
    select(-n)

potentialResoldProps <-
  preprocessedData %>%
  inner_join(propsAppearingMoreThanOnce)

resoldPropStats <-
  potentialResoldProps %>%
  select(
    sellingPriceRawData : streetRawData
    , yearBuiltRawData : agencyRawData
    , dateSoldPsdData
    ) %>%
  arrange(cityRawData, streetRawData, kvmRawData, roomsRawData,
    floorRawData, yearBuiltRawData, dateSoldPsdData) %>%
  group_by(cityRawData, streetRawData,
    kvmRawData, roomsRawData, floorRawData, yearBuiltRawData) %>%
  summarize(
    firstDateSold = first(dateSoldPsdData)
    , lastDateSold = last(dateSoldPsdData)
    , firstSellingPrice = first(sellingPriceRawData)
    , lastSellingPrice = last(sellingPriceRawData)
    ) %>%
    mutate(
      daysElapsed = lastDateSold - firstDateSold
      , changeInValue = lastSellingPrice - firstSellingPrice) %>%
    mutate(
      dailyGrowthRate =
        exp(
          log(lastSellingPrice / firstSellingPrice) / daysElapsed) - 1
          ) %>%
    mutate(
      annualGrowthRate = (1 + dailyGrowthRate) ^ 365.25 - 1
      , profitPerYear = (changeInValue) / daysElapsed * 365.25
      , riskToRewardAfter1Year =
        firstSellingPrice / profitPerYear
    ) %>%
  filter(daysElapsed >= 365.25 / 2) %>%
  ungroup() %>%
  relocate(dailyGrowthRate, riskToRewardAfter1Year)

# Paused here... I'm not sure if this join is attributing the resoldPropStats
# with the correct agent/agency - it should go to the first agent/agency
# in the potential_resold, not the second, third, or whatever.

potentialResoldProps %<>%
  inner_join(resoldPropStats)

# Looking at the average annual growth rate
(potentialResoldProps %>%
  ggplot(aes(annualGrowthRate * 100)) +
  geom_histogram()) %>%
  ggplotly()

# Organizing agents by the growth rates of properties they sold
agentsByAnnualGrowthRate <-
  potentialResoldProps %>%
    filter(yearSoldRawData > 2014) %>%
    group_by(agentNameRawData) %>%
    summarize(medianAnnualGrowthRate = median(annualGrowthRate)) %>%
    inner_join(potentialResoldProps) %>%
    filter(yearSoldRawData > 2019) %>%
    select(agentNameRawData, medianAnnualGrowthRate) %>%
    group_by(agentNameRawData) %>%
    summarize(medianAnnualGrowthRate = median(medianAnnualGrowthRate)) %>%
    ungroup %>%
    inner_join(potentialResoldProps) %>%
    select(agentNameRawData, agencyRawData, medianAnnualGrowthRate) %>%
    distinct %>%
    arrange(medianAnnualGrowthRate)

agentsByAnnualGrowthRate %>%
  inner_join(preprocessedData) %>%
  select(agentNameRawData, medianAnnualGrowthRate) %>%
  filter(agentNameRawData == "gabrielbilir") %>%
  head

preprocessedData %>%
  filter(agentNameRawData == "peteröhman") %>%
    head


percentile <- 2.25

agentsSellingAboveValue <-
  agentsByAnnualGrowthRate %>%
  filter(medianAnnualGrowthRate <
    quantile(medianAnnualGrowthRate, (0 + percentile / 100))) %>%
    arrange(medianAnnualGrowthRate)

agentsSellingBelowValue <-
  agentsByAnnualGrowthRate %>%
    filter(medianAnnualGrowthRate >
      quantile(medianAnnualGrowthRate, (1 - percentile / 100))) %>%
    arrange(medianAnnualGrowthRate)

nrow(agentsSellingAboveValue)
nrow(agentsSellingBelowValue)

topAndBottomAgents <-
  union(agentsSellingAboveValue, agentsSellingBelowValue)


preprocessedData %>%
  select(agentNameRawData, urlRawData) %>%
  # head
  filter(agentNameRawData == "saidsaid")
  
