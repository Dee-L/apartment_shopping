# Purpose: My summary
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 This presents customized summary data from a column of a df ####

mySummary <- function(df, columnToSummarize, limit = 3) {

    writeLines(paste0("\n\nCOLUMN SUMMARIZED:\n\n", columnToSummarize, "\n\n"))

    tempDf <- df[, columnToSummarize] %>% as.data.frame()
    colnames(tempDf)[1] <- "value"

    cat("Default summary:
")
    print(summary(df[[columnToSummarize]]))

    cat("Number of unique levels in this factor:
")

    print(df[[columnToSummarize]] %>% unique() %>% length())

    cat("
Sorted by value ascending:
")
    print(sqldf::sqldf(paste0("select value, count(*) as count
                     from tempDf
                     group by value
                     order by value asc
                     limit ", limit)))

    cat("
Sorted by value descending:
")
    print(sqldf::sqldf(paste0("select value, count(*) as count
                     from tempDf
                     group by value
                     order by value desc
                     limit ", limit)))

    cat("
Sorted by count ascending:
")
    print(sqldf::sqldf(paste0("select value, count(*) as count
                     from tempDf
                     group by value
                     order by count asc
                     limit ", limit)))

    cat("
Sorted by count descending:
")
    print(sqldf::sqldf(paste0("select value, count(*) as count
                     from tempDf
                     group by value
                     order by count desc
                     limit ", limit)))
}
