# Purpose: Look at summaries of data
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
      )

installMyPkgs(pkgs)

# 02 load latest compiled data ####

compiledData <-
  paste0(
    outputFolderCompiled
    , list.files(outputFolderCompiled) %>%
      .[length(.)]) %>%
    readRDS

# 03 specify where to save capture of summaries log ####
outputFolderSummaries <<-
paste0(
    outputFolder,
    "03_summaries/"
)

if (!dir.exists(outputFolderSummaries)) {
  dir.create(outputFolderSummaries)
}

# 03 Check if need to make new summaries ####

latestCompiledResults <-
    outputFolderCompiled %>%
    list.files %>%
    .[length(.)] %>%
    gsub(".rds", "", .) %>%
    right(8) %>%
    as.numeric
    
latestSummaries <-
    outputFolderSummaries %>%
    list.files %>%
    .[length(.)] %>%
    gsub(".txt", "", .) %>%
    right(8) %>%
    as.numeric

if (length(latestSummaries) == 0) {
    latestSummaries <- latestCompiledResults - 1
}

# 04 Only proceed if compiled results are older than scraped results ####
if (latestSummaries < latestCompiledResults) {

  # 05 Start logging ####
  logName <-
    paste0(
      outputFolderSummaries
      , "date_"
      , today8Digit()
      , ".txt"
    )

  sink(logName)

  # 06 Generate summaries ####
  for (columnName in names(compiledData)) {
    mySummary(compiledData, columnName, 10)
  }

  # 07 Stop logging

  sink()

}
