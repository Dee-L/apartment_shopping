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

install_my_pkgs(pkgs)

# 02 load latest compiled data ####

compiled_data <-
  paste0(
    output_folder_compiled
    , list.files(output_folder_compiled) %>%
      .[length(.)]) %>%
    readRDS

# 03 specify where to save capture of summaries log ####
output_folder_summaries <<-
paste0(
    output_folder,
    "03_summaries/"
)

if (!dir.exists(output_folder_summaries)) {
dir.create(output_folder_summaries)
}

# 03 Check if need to make new summaries ####

latest_compiled_results <-
    output_folder_compiled %>%
    list.files %>%
    .[length(.)] %>%
    gsub(".rds", "", .) %>%
    right(8) %>%
    as.numeric
    
latest_summaries <-
    output_folder_summaries %>%
    list.files %>%
    .[length(.)] %>%
    gsub(".txt", "", .) %>%
    right(8) %>%
    as.numeric

if (length(latest_summaries) == 0) {
    latest_summaries <- latest_compiled_results - 1
}

# 04 Only proceed if compiled results are older than scraped results ####
if (latest_summaries < latest_compiled_results) {

  # 05 Start logging ####
  log_name <-
    paste0(
      output_folder_summaries
      , "date_"
      , today_8digit()
      , ".txt"
    )

  sink(log_name)

  # 06 Generate summaries ####
  for (column_name in names(compiled_data)) {
  my_summary(compiled_data, column_name, 10)
  }

  # 07 Stop logging

  sink()

}
