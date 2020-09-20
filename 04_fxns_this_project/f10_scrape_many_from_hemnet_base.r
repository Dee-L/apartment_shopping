# Purpose: Scrape 35 to 50 pages from a Hemnet base address
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Start function definition ####

scrape_many_from_hemnet_base <- function() {

    # 02 Set the range for how many pages the base page should point to ####

    limit_hemnet_base_address_hits(min_n_pages = 1, max_n_pages = 50)

    # 03 Set where data will be saved ####

    output_folder_scraped <<-
    paste0(
        output_folder,
        "01_scraped/",
        "price_",
        current_min_price,
        "_to_",
        current_max_price,
        "/"
    )

    dir.create(output_folder_scraped)

    # 04 Loop to scrape the pages ####

    for (page in seq_len(total_pages)) {

        cat(
            "\n\nHemnet base page updating to",
            page,
            "of",
            total_pages,
            "total pages.\n\n"
        )

        hemnet_base_page_number <<- page

        # update base_address and session

        update_hemnet_base_address()

        html_to_read <<-
            hemnet_base_address %>%
            get_html()

        # count the number of links to scrape from

        pages_to_scrape <<-
            html_to_read %>%
            rvest::html_nodes(".item-link-container") %>%
            rvest::html_attr("href")

        cat(length(pages_to_scrape), "pages to scrape from this base page.")

        # 05 Inner loop ####

        for (page_to_scrape in seq_along(pages_to_scrape)) {

            cat(
                paste0(
                    "\n\n"
                    , page_to_scrape
                    , " of "
                    , length(pages_to_scrape)
                    , " pages on this base page."
                    )
                )

            # update url

            url_to_scrape <<- pages_to_scrape[page_to_scrape]

            html_to_read <<- try_wait_retry(get_html(url_to_scrape))

            # 06 Scrape and test all data from the url. Capture fails. ####

            try_scrape_save_hemnet_html()
        }
    }
}
