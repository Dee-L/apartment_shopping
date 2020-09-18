test_function <- function(object_to_test) {
    if (length(object_to_test) == 0) {
        class_to_cast <- class(object_to_test)
        object_to_test <- NA
        object_to_test %<>%
            as(., class_to_cast)
        object_to_test
    }
}



test_function(as.numeric())


old_url <- url_to_scrape

new_url <- "https://www.hemnet.se/salda/lagenhet-2rum-bromma-bromma-kyrka-stockholms-kommun-spangavagen-138,-hogst-upp-1250812"


urls_to_test <-
    c(
        "https://www.hemnet.se/salda/lagenhet-2rum-stockholms-kommun-dobelnsgatan-67-1250737"
    )

url_to_scrape <- "https://www.hemnet.se/salda/lagenhet-2rum-stockholms-kommun-dobelnsgatan-67-1250737"

url_to_scrape <- old_url

for (url in urls_to_test) {
    
    if(exists("iterator")) {
        iterator <- iterator + 1
    } else {
        iterator <- 1
    }
    
    cat("Testing url", iterator, "of", length(urls_to_test), "\n\n")

    get_all_variables(url)

}

rm(iterator)


get_day_of_month_sold("https://www.hemnet.se/salda/lagenhet-4rum-kungsangen-uppsala-kommun-muningatan-4-1250838")



# loads an RData file, and returns it, allowing you to reassign it to a new name
reassign_rda <- function(file_name) {
            load(file_name)
            get(ls()[ls() != "file_name"])
}




for (file in
    list.files("07_outputs//01_scraped_new/price_1000000_to_2187000/")) {

        temp_df <<- reassign_rda(
            paste0(
                "07_outputs//01_scraped_new/price_1000000_to_2187000/"
                , file
                )
            )

        if (!exists("results_df")) {
           results_df <<- temp_df
        } else {
            results_df <<- rbind(results_df, temp_df)
        }
    }
