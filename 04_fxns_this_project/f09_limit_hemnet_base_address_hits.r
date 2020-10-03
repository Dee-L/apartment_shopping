# Purpose: Adjust Hemnet base address to limited hits
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-19
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "rvest"
    )

install_my_pkgs(pkgs)

# 02 Define the function ####

limit_hemnet_base_address_hits <- function(min_n_pages = 1, max_n_pages = 50) {

    repeat {
        update_hemnet_base_address()

        # 03 Create html session for navigating and scraping ####

        html_to_read <<-
            hemnet_base_address %>%
            get_html()

        # 04 Calculate the number of pages from the page landed on ####

        total_pages <<-
            html_to_read %>%
            rvest::html_nodes("#result .clear-children .centered") %>%
            rvest::html_text() %>%
            gsub("[^[:alnum:] ]| ", "", .) %>%
            gsub("Visar150av", "", .) %>%
            as.numeric() %>%
            prod(., 1 / 50) %>%
            ceiling(.)

        # 05 Reset total pages to 1 if only 1 page in range ####

        if (is.na(total_pages)) {
            total_pages <<- 1
        }

        cat(
            "total pages are: ",
            total_pages,
            "\n\n"
        )

        # 06 Adjust price ranges so that have fewer than max_n_pages ####

        if (total_pages < min_n_pages) {
            cat(
                "Increasing current_max_price.\n\nMax price was: ",
                current_max_price,
                "\n\n"
            )
            current_max_price <<- current_max_price + 1000000
            cat("Max price is now: ", current_max_price, "\n\n")

        } else if (total_pages >= max_n_pages) {
            cat(
                "Decreasing current_max_price.\n\nMax price was: ",
                current_max_price,
                "\n\n"
            )
            current_max_price <<-
                median(c(current_min_price, current_max_price)) %>%
                floor
            cat("Max price is now: ", current_max_price, "\n\n")
        } else if (
            all(
                min_n_pages < total_pages
                , total_pages < max_n_pages
            )
        ) {
            break
        }
    }

}
