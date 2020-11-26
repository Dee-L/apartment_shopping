# Purpose: Send data through all my feature engineering steps
# Author: David Gray Lassiter, PhD
# Date: 2020-nov-16
# Version:


# I tried to write this as a function that took a dataset as input and then
# saved `compiledData` as a copy of that input dataset before running it
# through the rest, but I kept running into issues with the `mutate` functions
# where, for example, `areaConsolidated` or `areaCleanedString` could not be
# found. Eventually, I just saved this all as a macro that can be sourced
# directly.

# There are problems/oversights with this script:
# 1. It doesn't handle cleaninng strings for the categorical variables well since many new columns get made but then are later not used
# 2. It doesn't handle missing/na values well when running the replacelowfrequency function
# Unfortunately, Hemnet changed their site so that scraping agency no longer works well and I get missing values... These cause problems and I am unable to run data through the full pipeline then.
# It may be best to consider dropping this pipeline altogether by re-writing it as a series of tidymodel preprocessing steps.

# 01 Make your input object visible to this macro.
getAnywhere(compiledData)

# 03 Gathering lists of variables ####

rawDataVariables <- colnames(compiledData)

categoricalVariables <-
    c(
        "city"
        , "area"
        , "street"
        , "agentName"
        , "agency"
    )

for (variable in categoricalVariables) {

    # 03 drop blanks ####
    cleanStringVarName <- paste0(variable, "CleanedString")

    compiledData[[cleanStringVarName]] <-
        compiledData[[variable]] %>%
        str_replace_all(" |/", "")

    # 04 Specify new variable name and count missing ####
    newVarName <- paste0(cleanStringVarName, "MissingReplaced")

    nMissing <-
        sum((compiledData[[cleanStringVarName]] == "") |
                (is.na(compiledData[[cleanStringVarName]])))

    cat("\nVariable is:", cleanStringVarName, "and nMissing is:", nMissing, "\n")

    # 05 If any missing, replace with "missing" ####
    if (nMissing > 0) {

        compiledData[[newVarName]] <-
            ifelse(
                (
                    (compiledData[[cleanStringVarName]] == "")
                    | is.na(compiledData[[cleanStringVarName]])
                )
                , "missing"
                , compiledData[[cleanStringVarName]]
            )

        nMissingNewVariable <-
            sum((compiledData[[newVarName]] == "") |
                    (is.na(compiledData[[newVarName]])))

        cat("\n\tNew variable is:", newVarName
            , "and nMissing is:", nMissingNewVariable, "\n")

        # 06 Update categorical variables list ####
        categoricalVariables[which(categoricalVariables %in% cleanStringVarName)] <-
            newVarName
    }
}

# 07 Initializing column to populate in the loop ####

if ("areaCleanedStringMissingReplaced" %in% names(compiledData)) {

    areaColumnForProcessing <- "areaCleanedStringMissingReplaced"

    compiledData %<>%
        mutate(areaConsolidated = areaCleanedStringMissingReplaced)

} else {

    areaColumnForProcessing <- "areaCleanedString"

    compiledData %<>%
        mutate(areaConsolidated = areaCleanedString)
}

# 08 Repeat loop to consolidate areas ####
repeat {
    # 09 Getting the count of unique levels for area ####

    nUniqueAreas <-
        compiledData[["areaConsolidated"]] %>%
        unique %>%
        length

    message(
        "\nNumber of unique areas: "
        , nUniqueAreas
        , " Trying to consolidate.\n")

    # 10 Find the top 50 areas ####
    replacementAreas <-
        sqldf::sqldf(
            "select areaConsolidated, count(*) as count
  from compiledData
  group by areaConsolidated
  order by count desc
  limit 100")

    # 11 Use top hit to try to consolidate lower frequency hits ####
    for (area in replacementAreas[[1]]) {

        # 12 Create areasConsolidated list ####
        if (!exists("areasConsolidated")) {
            areasConsolidated <- c()
        }

        # 13 Skip area if you've already consolidated to it ####
        if (area %in% areasConsolidated) {
            cat("\nArea ", area, "already consolidated. Skipping.\n")
            next
        } else {
            nAreaConsolidated <- sum(compiledData[["areaConsolidated"]] == area)

            cat(
                "\nArea is:", area
                , "\n\tn before consolidation:", nAreaConsolidated, "\n"
            )

            # 14 Consolidate if text matches, otherwise leave alone ####
            compiledData[["areaConsolidated"]] <-
                ifelse(
                    (
                        grepl(area, compiledData[[areaColumnForProcessing]]) &
                            (compiledData[["areaConsolidated"]] ==
                                 compiledData[[areaColumnForProcessing]])
                    )
                    ,
                    area,
                    compiledData[["areaConsolidated"]]
                )

            nAreaConsolidated <- sum(compiledData[["areaConsolidated"]] == area)

            cat("\n\t n after consolidation:", nAreaConsolidated, "\n")

            # 15 add area to areasConsolidated so don't consolidate to it again ####
            areasConsolidated %<>% c(., area)

            # 16 after consolidating an area break to go back to repeat level ####
            break
        }

    }
    # 17 Break loop if you have reached the end of the list and update vars ####
    if (area == replacementAreas[[1]][nrow(replacementAreas)]) {
        message("Reached the end of the list. No more areas to consolidate.")
        areasConsolidated <- NULL

        categoricalVariables %<>%
            dplyr::recode(areaMissingReplaced = "areaConsolidated")

        break
    }
}

# 18 Making generalized columns for some variables ####
compiledData <-
    replaceLowFreqWithOther(compiledData, "areaConsolidated", 31)

categoricalVariables %<>%
    dplyr::recode(area = "areaConsolidatedLowFreqGeneralized")

compiledData <- replaceLowFreqWithOther(compiledData, "street", 31)

categoricalVariables %<>%
    dplyr::recode(street = "streetLowFreqGeneralized")

compiledData <- replaceLowFreqWithOther(compiledData, "agentName", 31)

categoricalVariables %<>%
    dplyr::recode(agentName = "agentNameLowFreqGeneralized")

compiledData <- replaceLowFreqWithOther(compiledData, "agency", 31)

categoricalVariables %<>%
    dplyr::recode(agency = "agencyLowFreqGeneralized")

# 19 Removing nonsensical floor data due to imperfect scraping ####

compiledData$floorNonhalfDropped <-
    compiledData$floor %>%
    ifelse(. %% 0.5 == 0, ., NA)

# 20 Adding features that are calculations of known features ####

compiledData %<>%
    mutate(
        ageWhenSold = yearSold - yearBuilt
        , roomsPerFloor = rooms / floorNonhalfDropped
        , kvmPerRoom = kvm / rooms
        , kvmPerFloor = kvm / floorNonhalfDropped
        , avgiftPerKvm = avgift / kvm
        , avgiftPerRoom = avgift / rooms
        , avgiftPerFloor = avgift / floorNonhalfDropped
        , avgiftPerAskingPrice = avgift / askingPrice
        , runningCostsPerKvm = runningCosts / kvm
        , runningCostsPerRoom = runningCosts / rooms
        , runningCostsPerFloor = runningCosts / floorNonhalfDropped
        , runningCostsPerAskingPrice = runningCosts / askingPrice
    )

# 21 Imputing where have NAs for numerical variables ####
rawDataNumericalVariables <-
    compiledData %>%
    head %>%
    select(askingPrice : runningCosts, yearBuilt) %>%
    names

fewEngnrdNumericalVariables <-
    compiledData %>%
    head %>%
    select(floorNonhalfDropped : runningCostsPerAskingPrice) %>%
    names

numericalVariables <-
    c(
        rawDataNumericalVariables,
        fewEngnrdNumericalVariables
    )

# 22 Replace NAs with a random sample from the real values in new variable ####
set.seed(412)

for (variable in numericalVariables) {

    newVariableName <-
        paste0(
            variable
            , "ImputedFromRandomSample"
        )

    nonMissing <- compiledData[[variable]] %>% .[!is.na(.)]

    lengthMissing <- compiledData[[variable]] %>% .[is.na(.)] %>% length

    compiledData[[eval(newVariableName)]] <-
        ifelse(
            is.na(compiledData[[variable]])
            , sample(nonMissing, lengthMissing, replace = T)
            , compiledData[[variable]]
        )

    # 23 Update numerical variables list ####
    numericalVariables[which(numericalVariables %in% variable)] <-
        newVariableName
}

# 24 Setting Swedish months to English ####
compiledData[["monthSoldEnglish"]] <-
    dplyr::case_when(
        compiledData[["monthSoldSwedish"]] ==  "januari" ~ "jan",
        compiledData[["monthSoldSwedish"]] ==  "februari" ~ "feb",
        compiledData[["monthSoldSwedish"]] ==  "mars" ~ "mar",
        compiledData[["monthSoldSwedish"]] ==  "april" ~ "apr",
        compiledData[["monthSoldSwedish"]] ==  "maj" ~ "may",
        compiledData[["monthSoldSwedish"]] ==  "juni" ~ "jun",
        compiledData[["monthSoldSwedish"]] ==  "juli" ~ "jul",
        compiledData[["monthSoldSwedish"]] ==  "augusti" ~ "aug",
        compiledData[["monthSoldSwedish"]] ==  "september" ~ "sep",
        compiledData[["monthSoldSwedish"]] ==  "oktober" ~ "oct",
        compiledData[["monthSoldSwedish"]] ==  "november" ~ "nov",
        compiledData[["monthSoldSwedish"]] ==  "december" ~ "dec"
    )

# 25 Concatenating to lubridate format ####
compiledData[["dateSold"]] <-
    paste(
        compiledData[["yearSold"]]
        , compiledData[["monthSoldEnglish"]]
        , compiledData[["dayOfMonthSold"]]
        , sep = "-") %>%
    lubridate::ymd()

# 26 Adding several levels of date information for time series analysis ####
compiledData %<>% addDateDataForTsa("dateSold")

# 27 Adding Swedish holiday date information ####
compiledData %<>% addSwedishDaysOffData("dateSold")

# 28 Note to self - should reduce complexity of loop in 29 ####

# 29 Engineering time-series analysis features ####

for (variable in catVarsForTimeSeriesAnalysis) {

    # 30 Aggregating by unique level within the categorical variables ####
    uniqueLevels <- unique(compiledData[[variable]])
    columnPrefix <- variable
    sqlColumnNames <-
        c("MedianSellingPriceThisDate",
          "MeanSellingPriceThisDate",
          "SumSellingPriceThisDate") %>%
        paste0(columnPrefix, .)

    # 31 Create a datesDf to join to ####
    minDate <-
        compiledData[["dateSold"]] %>%
        as.Date %>%
        min

    maxDate <-
        compiledData[["dateSold"]] %>%
        as.Date %>%
        max

    datesDf <-
        data.frame(
            dateSold = c(minDate : maxDate) %>% as.Date(origin = "1970-01-01")
            , categoricalVariable = variable
        )

    for (uniqueLevel in uniqueLevels) {

        # 32 Add column to datesDf based on the level for joining ####
        datesDf[["level"]] <- uniqueLevel

        # 33 Create tempDf with aggregate fxns by joining to datesDf ####

        tempDf <-
            sqldf::sqldf(
                paste0(
                    "select
        d.dateSold
        , level as ", variable,
                    ", median(sellingPrice) as ", sqlColumnNames[1],
                    ", avg(sellingPrice) as ", sqlColumnNames[2],
                    ", sum(sellingPrice) as ", sqlColumnNames[3],
                    " from datesDf as d
        left join compiledData as c
          on d.dateSold = c.dateSold and
            d.level = c.", variable,
                    " group by d.dateSold
      order by d.dateSold desc"
                )
            )

        # 34 Interpolate prices (linearly) for dates not in the raw data ####
        for (aggColumn in sqlColumnNames) {

            tempDf %<>%
                interpolateForMissingDates(aggColumn, "dateSold")

            columnForFurtherAggs <- paste0(aggColumn, "Interpolated")

            # 35 Will do rolling and lag fxns at several timeframes ####
            timeframesToLook <- c(7, 31, 91, 365)
            for (daysBack in timeframesToLook) {
                cat(
                    "Working on: ", variable
                    , "--", uniqueLevel
                    , "--", aggColumn
                    , "--", daysBack
                    , "\n"
                )

                # 36 Prepping column names ####
                colNameSufForRollData <-
                    paste0("SellingPriceLast", daysBack, "Days")

                colNameSufForLagData <-
                    paste0("SellingPrice", daysBack, "DaysAgo")

                if (grepl("Median", aggColumn)) {
                    fxn <- "Median"
                } else if (grepl("Mean", aggColumn)) {
                    fxn <- "Mean"
                } else if (grepl("Sum", aggColumn)) {
                    fxn <- "Sum"
                }

                columnNameForRollingData <-
                    paste0(columnPrefix, fxn, colNameSufForRollData)
                columnNameForLagData <-
                    paste0(columnPrefix, fxn, colNameSufForLagData)

                # 37 Rolling aggregation data ####
                fxnTextForRollingData <- paste0("RcppRoll::roll_", tolower(fxn), "l")

                tempDf[[columnNameForRollingData]] <-
                    eval(parse(text = fxnTextForRollingData))(
                        tempDf[[columnForFurtherAggs]]
                        , daysBack
                        , fill = NA)

                tempDf %<>%
                    interpolateForMissingDates(
                        columnNameForRollingData, "dateSold")

                # 38 Lag data ####
                tempDf[[columnNameForLagData]] <-
                    dplyr::lead(
                        tempDf[[columnForFurtherAggs]]
                        , daysBack
                    )

                tempDf %<>%
                    interpolateForMissingDates(
                        columnNameForLagData
                        , "dateSold"
                    )

                # 39 Engineering more features that are lag + rolling ####

                # 40 Lag+rolling features only if done with last timeframe ####

                if (daysBack == (timeframesToLook %>% .[length(.)])) {

                    timeframePermutations <-
                        data.frame(
                            rollDay = c(
                                rep(timeframesToLook, length(timeframesToLook)) %>% sort
                            )
                            , lagDay = c(timeframesToLook)
                        )

                    # 41 set the rolling and lag days ####
                    for (rowN in seq_len(nrow(timeframePermutations))) {
                        rollDay <- timeframePermutations[rowN, 1]
                        lagDay <- timeframePermutations[rowN, 2]

                        colNameSufForRollLagData <-
                            paste0(
                                "SellingPriceOver"
                                , rollDay
                                , "Days"
                                , lagDay
                                , "DaysAgo"
                            )

                        # 42 Name the columns ####
                        colNameForRollLagData <-
                            paste0(columnPrefix
                                   , fxn
                                   , colNameSufForRollLagData
                            )

                        columnNameForSourceData <-
                            paste0(
                                columnPrefix
                                , fxn
                                , "SellingPriceLast"
                                , rollDay
                                , "Days"
                            )

                        # 43 Create the columns ####
                        tempDf[[colNameForRollLagData]] <-
                            dplyr::lead(
                                tempDf[[columnNameForSourceData]]
                                , lagDay
                            )

                        tempDf %<>%
                            interpolateForMissingDates(
                                colNameForRollLagData
                                , "dateSold"
                            )
                    }
                }
            }
        }

        # 44 Creating the joinDf ####
        if (uniqueLevel == uniqueLevels[1]) {
            joinDf <- tempDf
        } else {
            joinDf %<>% rbind(tempDf)
        }
    }

    # 45 Adding the new columns to the compiledData ####
    compiledDataColumnsForSql <-
        names(compiledData) %>% paste0("c.", ., collapse = ", ")

    newColumnsForSql <-
        names(joinDf)[3 : ncol(joinDf)] %>% paste0(collapse = ", ")

    compiledData <-
        sqldf::sqldf(
            paste0(
                "select ", compiledDataColumnsForSql,
                ", ", newColumnsForSql,
                " from compiledData as c
        left join joinDf as j
          on c.dateSold = j.dateSold and
            c.", variable, " = j.", variable
            )
        )

    message(
        "Compiled data dimensions: ", dim(compiledData)[1]
        , " by ", dim(compiledData)[2])

}


# 46 Create df for making the ohe features ####
featuresForOhe <-
    c(
        "city",
        "areaConsolidatedLowFreqGeneralized",
        "streetLowFreqGeneralized",
        "agentNameLowFreqGeneralized",
        "agencyLowFreqGeneralized",
        "quarterOfYearSold",
        "monthOfYearSold",
        "monthOfQuarterSold",
        "weekOfYearSold",
        "weekOfQuarterSold",
        "weekOfMonthSold",
        "dayOfYearSold",
        "dayOfQuarterSold",
        "dayOfMonthSold",
        "dayOfWeekSold"
    )

dfForOhe <- compiledData[, featuresForOhe]

# 47 Convert all to factor so one-hot encoding will work on numeric data ####
dfForOhe[] <- lapply(dfForOhe, as.factor)

# 48 Make the ohe columns ####
dfForOhe %<>%
    dummyVars(" ~ .", data = ., sep = "") %>%
    predict(newdata = dfForOhe) %>%
    data.frame()

# 49 add suffix for easier sorting later ####
names(dfForOhe) %<>% paste0("Ohe")

# 50 Add columns back to compiledData ####
compiledData %<>%
    cbind(dfForOhe)

# 51 Converting what could be numerical or character into factor data ####
columnIdentifiers <- c("dayOf", "weekOf", "monthOf", "quarterOf", "Ohe")

for (id in columnIdentifiers) {
    for (clmn in names(compiledData)) {
        if ((grepl(id, clmn)) | (class(compiledData[[clmn]]) == "character")) {
            compiledData[[clmn]] %<>% as.factor
        }
    }
}

# 52 Reordering some factors for display purposes ####
compiledData[["dayOfWeekSold"]] %<>%
    factor(x = ., levels = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"))

compiledData[["monthSoldSwedish"]] %<>%
    factor(
        x = .,
        levels = c(
            "januari",
            "februari",
            "mars",
            "april",
            "maj",
            "juni",
            "juli",
            "augusti",
            "september",
            "oktober",
            "november",
            "december"
        )
    )

compiledData[["monthSoldEnglish"]] %<>%
    factor(
        x = .,
        levels = c(
            "jan",
            "feb",
            "mar",
            "apr",
            "may",
            "jun",
            "jul",
            "aug",
            "sep",
            "oct",
            "nov",
            "dec"
        )
    )

#53 Converting dateSold into a number ####
compiledData$dateSold %<>%
    as.numeric

#54 Converting swedish days off columns into factors ####
compiledData$swedishRedOrPinkDay %<>%
    as.factor

compiledData$swedishPinchDay %<>%
    as.factor

compiledData$swedishDayOff %<>%
    as.factor

# 55 Identifying processed data variables ####
psdDataVariables <-
    colnames(compiledData)[colnames(compiledData) %notIn% rawDataVariables]

# 56 Adding "rawData" suffix to all original variables ####
rawDataVariableNames <- paste0(rawDataVariables, "RawData")

oldColNamesToReplace <-
    match(colnames(compiledData), rawDataVariables, nomatch = 0)

colnames(compiledData)[colnames(compiledData) %in% rawDataVariables] <-
    rawDataVariableNames[oldColNamesToReplace]

# 57 Adding "PsdData" suffix to all new variables ####
psdDataVariableNames <- paste0(psdDataVariables, "PsdData")

newColNamesToReplace <-
    match(colnames(compiledData), psdDataVariables, nomatch = 0)

colnames(compiledData)[colnames(compiledData) %in% psdDataVariables] <-
    psdDataVariableNames[newColNamesToReplace]

# 58 Renaming dayOfMonthSold column so it will be handled like psddata ####
compiledData %<>%
    rename(dayOfMonthSoldPsdData = dayOfMonthSoldRawData)
