# Purpose: My summary
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 This presents customized summary data from a column of a df ####

my_summary <- function(df, column_to_summarize, limit = 3) {
    temp_df <- df[, column_to_summarize] %>% as.data.frame()
    colnames(temp_df)[1] <- "value"

    cat("Default summary:
")
    print(summary(df[[column_to_summarize]]))

    cat("Number of unique levels in this factor:
")

    print(df[[column_to_summarize]] %>% unique() %>% length())

    cat("
Sorted by value ascending:
")
    print(sqldf(paste0("select value, count(*) as count 
                     from temp_df
                     group by value
                     order by value asc
                     limit ", limit)))

    cat("
Sorted by value descending:
")
    print(sqldf(paste0("select value, count(*) as count 
                     from temp_df
                     group by value
                     order by value desc
                     limit ", limit)))

    cat("
Sorted by count ascending:
")
    print(sqldf(paste0("select value, count(*) as count 
                     from temp_df
                     group by value
                     order by count asc
                     limit ", limit)))

    cat("
Sorted by count descending:
")
    print(sqldf(paste0("select value, count(*) as count 
                     from temp_df
                     group by value
                     order by count desc
                     limit ", limit)))
}