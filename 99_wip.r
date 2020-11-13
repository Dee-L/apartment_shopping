
# # Exploring best/worst agents

# resoldProperties %>%
#   # Grouping by the unique property identifiers
#   group_by(cityRawData, streetRawData,
#     kvmRawData, roomsRawData, floorRawData, yearBuiltRawData) %>%
#   arrange(cityRawData, streetRawData, kvmRawData, roomsRawData,
#           floorRawData, yearBuiltRawData, dateSoldPsdData) %>%
#   summarize(
#     firstDateSold = first(dateSoldPsdData)
#     , lastDateSold = last(dateSoldPsdData)
#     , firstSellingPrice = first(sellingPriceRawData)
#     , lastSellingPrice = last(sellingPriceRawData)
#     ) %>%
#     mutate(
#       daysElapsed = lastDateSold - firstDateSold
#       , changeInValue = lastSellingPrice - firstSellingPrice) %>%
#     mutate(
#       dailyGrowthRate =
#         exp(
#           log(lastSellingPrice / firstSellingPrice) / daysElapsed) - 1
#           ) %>%
#     mutate(
#       annualGrowthRate = (1 + dailyGrowthRate) ^ 365.25 - 1
#       , profitPerYear = (changeInValue) / daysElapsed * 365.25
#       , riskToRewardAfter1Year =
#         firstSellingPrice / profitPerYear
#     ) %>%
#   filter(daysElapsed >= 365.25 / 2) %>%
#   ungroup() %>%
#   relocate(dailyGrowthRate, riskToRewardAfter1Year)

# # Paused here... I'm not sure if this join is attributing the resoldPropStats
# # with the correct agent/agency - it should go to the first agent/agency
# # in the potential_resold, not the second, third, or whatever.

# potentialResoldProps %<>%
#   inner_join(resoldPropStats)

# # Looking at the average annual growth rate
# (potentialResoldProps %>%
#   ggplot(aes(annualGrowthRate * 100)) +
#   geom_histogram()) %>%
#   ggplotly()

# # Organizing agents by the growth rates of properties they sold
# agentsByAnnualGrowthRate <-
#   potentialResoldProps %>%
#     filter(yearSoldRawData > 2014) %>%
#     group_by(agentNameRawData) %>%
#     summarize(medianAnnualGrowthRate = median(annualGrowthRate)) %>%
#     inner_join(potentialResoldProps) %>%
#     filter(yearSoldRawData > 2019) %>%
#     select(agentNameRawData, medianAnnualGrowthRate) %>%
#     group_by(agentNameRawData) %>%
#     summarize(medianAnnualGrowthRate = median(medianAnnualGrowthRate)) %>%
#     ungroup %>%
#     inner_join(potentialResoldProps) %>%
#     select(agentNameRawData, agencyRawData, medianAnnualGrowthRate) %>%
#     distinct %>%
#     arrange(medianAnnualGrowthRate)

# agentsByAnnualGrowthRate %>%
#   inner_join(preprocessedData) %>%
#   select(agentNameRawData, medianAnnualGrowthRate) %>%
#   filter(agentNameRawData == "gabrielbilir") %>%
#   head

# preprocessedData %>%
#   filter(agentNameRawData == "peteröhman") %>%
#     head


# percentile <- 2.25

# agentsSellingAboveValue <-
#   agentsByAnnualGrowthRate %>%
#   filter(medianAnnualGrowthRate <
#     quantile(medianAnnualGrowthRate, (0 + percentile / 100))) %>%
#     arrange(medianAnnualGrowthRate)

# agentsSellingBelowValue <-
#   agentsByAnnualGrowthRate %>%
#     filter(medianAnnualGrowthRate >
#       quantile(medianAnnualGrowthRate, (1 - percentile / 100))) %>%
#     arrange(medianAnnualGrowthRate)

# nrow(agentsSellingAboveValue)
# nrow(agentsSellingBelowValue)

# topAndBottomAgents <-
#   union(agentsSellingAboveValue, agentsSellingBelowValue)


# preprocessedData %>%
#   select(agentNameRawData, urlRawData) %>%
#   # head
#   filter(agentNameRawData == "saidsaid")


# Purpose: Density plots for numerical features in dataset
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:


# May want to change this to offer two plots or (better) two traces on same plot

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
    "htmlwidgets",
    "sqldf",
    "shiny"
  )

activatePkgs(pkgs)

# 02 Load data for this app if it isn't already loaded ####

if (any(
  !exists("preprocessedAllData"),
  !exists("preprocessedResoldProps"),
  !exists("numericVarsForDensityPlots")
)) {
  source("05_scripts/s06_prepResoldPropDatasets.R")
  source("05_scripts/s07_defineVariableGroups.R")
}


# 03 load shiny ####
library(shiny)

# 04 Define UI ####
ui <-
  fluidPage(
    titlePanel("Density plots of numerical variables"),
    sidebarLayout(
      uiOutput("sidebarOutput"),
      mainPanel(uiOutput("mainPanel"))
    )
  )

# 04 Define server ####
server <- function(input, output) {
  output[["sidebarOutput"]] <-
    renderUI({
      sidebarPanel(
        selectInput(
          inputId = "continuousVariable",
          label = "Select a continuous variable to plot",
          h3("Select box"),
          choices =
            c(
              # varToPredictModel1
              # "annualGrowthRate"
              # "annualGrowthByFirstSellingPrice"
              "valueIncreasePerYear"
              # "sellingPriceRawData",
              # numericVarsForDensityPlots %>%
              #   .[. != "sellingPriceRawData"]
            )
        )
      )
    })

  # 05 Save inputs from user in reactive objects ####
  contVar <- reactive({
    input[["continuousVariable"]]
  })

  # 06 Create plot ####
  output[["densityPlot"]] <-
    renderPlotly({
      myBasicDensityPlot(
        df = preprocessedResoldProps %>% .[!is.na(.[[contVar()]]), ],
        x = contVar(),
        title = paste0("Density plot of ", contVar())
      )
    })

  # 07 Render the plot ####
  output[["mainPanel"]] <-
    renderUI({
      plotlyOutput(outputId = "densityPlot")
    })
}

shinyApp(ui = ui, server = server)





# Trying to see if can't look at resold properties where sold at least 3
# times so that can get unique stats for each agent.
# Example, agent A sells it in 2012, agent B sells it in 2013, agent C sells
# it in 2014, then want to figure out the growth in value from agent A
# to B, then again for Agent B to C

source("04_fxnsThisProject/f01_sourceAllFxns.r")

# 02 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
    "sqldf"
      , "lubridate"
      , "RcppRoll"
      , "caret"
  )

activatePkgs(pkgs)

preprocessedAllData <-
  paste0(
    outFolderPreprocessedAllData,
    list.files(outFolderPreprocessedAllData) %>%
      .[length(.)]
  ) %>%
  readRDS()

# Renaming for work here
preprocessedResoldProps <- preprocessedAllData

preprocessedResoldProps %<>%
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
    arrange(!!!syms(identifiersForUniqueProp), dateSoldPsdData) %>%
# Grouping by the unique property identifiers
    group_by(!!!syms(identifiersForUniqueProp)) %>%
    mutate(
      nextDateSold = lead(dateSoldPsdData)
      , daysElapsed = nextDateSold - dateSoldPsdData
      , nextSellingPrice = lead(sellingPriceRawData)
      , changeInValue = nextSellingPrice - sellingPriceRawData
      , dailyGrowthRate =
        exp(log(nextSellingPrice / sellingPriceRawData) / daysElapsed) - 1
    , annualGrowthRate = (1 + dailyGrowthRate) ^ 365.25 - 1
    , valueIncreasePerYear = (changeInValue) / daysElapsed * 365.25
    ) %>%
  # Dropping properties that resold in less than half a year
  filter(daysElapsed >= 365.25 / 2) %>%
    # Ungrouping data
    ungroup() %>%
    # Moving the column that I want to predict to the front
    relocate(valueIncreasePerYear, annualGrowthRate) %>%
    # Dropping columns that won't be used in my predictive model
    select(
      -nextDateSold,
      -daysElapsed,
      -nextSellingPrice,
      -changeInValue,
      -dailyGrowthRate,
      -annualGrowthRate
    ) %>%
    # Bringing back in the other data
    inner_join(preprocessedAllData) %>%
    # Limiting just to predictor variables and what I want to predict
    .[, c(varToPredictModel2, allPredictorVars2)] %>%
  .[complete.cases(.), ]



# https://juliasilge.com/blog/sf-trees-random-tuning/
# install.packages("tidymodels")
# library("tidymodels")
set.seed(412)
data_split <- initial_split(preprocessedResoldProps)
data_train <- training(data_split)
data_test <- testing(data_split)

data_rec <- recipe(valueIncreasePerYear ~ ., data = data_train)

data_prep <- prep(data_rec, retain = T)

data_training <- juice(data_prep)

data_ranger <-
  rand_forest(mode = "regression") %>%
  set_engine("ranger") %>%
  fit(valueIncreasePerYear ~ ., data = data_train)

data_rf <-
  rand_forest(mode = "regression") %>%
  set_engine("randomForest") %>%
  fit(valueIncreasePerYear ~ ., data = data_train)

predict(data_ranger, data_test)

data_ranger %>%
  predict(data_test) %>%
  bind_cols(data_test) %>%
  metrics(truth = valueIncreasePerYear, estimate = .pred)


tune_spec


dataset <- consolidatedData


myvector <- c('a', 'a', 'a', 'b')

fct_lump_min(myvector, 2, other_level = 'other') %>% as.character


createObjectToRestore(compiledData)

compiledData1 <-
  replaceLowFreqWithOther(compiledData, "areaConsolidated", 31)

compiledData2 <- compiledData

compiledData2[["areaConsolidatedLowFreqGeneralized"]] <-
  fct_lump_min(compiledData2[["areaConsolidated"]],
               31,
               other_level = 'other') %>%
  as.character

categoricalVariables %>%
  dplyr::recode(areaConsolidated = "areaConsolidatedLowFreqGeneralized")

myvector <- c('a', 'a', 'b')
fct_lump_min(myvector, 1, other_level = 'other')
