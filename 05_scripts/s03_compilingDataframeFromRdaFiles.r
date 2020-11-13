# Purpose: Compiling scraped Hemnet data into a df
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-18
# Version: 1.0



# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c()

activatePkgs(pkgs)

# 02 Check if need to compile new data ####

latestScrapedResults <-
    outputFolderScrapedGparent %>%
    list.dirs(recursive = F) %>%
    .[length(.)] %>%
    right(8) %>%
    as.numeric
    
latestCompiledResults <-
    outputFolderCompiled %>%
    list.files %>%
    .[length(.)] %>%
    gsub(".rds", "", .) %>%
    right(8) %>%
    as.numeric

if (length(latestCompiledResults) == 0) {
    latestCompiledResults <- latestScrapedResults - 1
}

# 03 Only proceed if compiled results are older than scraped results ####
if (latestCompiledResults < latestScrapedResults) {

    # 04 Subset the scraped folders to those that contain new data ####
    scrapedFolders <-
        list.dirs(outputFolderScrapedGparent, recursive = F)

    datesOfScrapedFolders <-
        scrapedFolders %>%
            right(8) %>%
            as.numeric

    relevantScrapedFolders <-
        scrapedFolders[datesOfScrapedFolders > latestCompiledResults]

    # 05 compile the data ####
    for (file in list.files(relevantScrapedFolders, recursive = T)) {

        # 06 create path for file ####
        thisPath <-
            paste0(relevantScrapedFolders, "/", file)

        # 07 ignore the failedPages files ####
        if (!(grepl("failedPages.rds", thisPath))) {

            cat("Adding: ", file, "\n\n")

            # 08 save into a tempDf the file ####
            tempDf <<- reassignRda(thisPath)

            # 09 create or append to resultsDf ####
            if (!exists("resultsDf")) {
                resultsDf <<- tempDf
            } else {
                resultsDf <<- rbind(resultsDf, tempDf)
            }
        }
    }

    # 10 save the object ####
    nameOfResultsDf <-
        paste0(
            "date_"
            , today8Digit()
        )

    saveRDS(
        resultsDf
        , paste0(outputFolderCompiled, eval(nameOfResultsDf))
        )
}
