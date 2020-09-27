# Purpose: Re-scraping failed_pages
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this scripts are installed ####
pkgs <-
    c()

install_my_pkgs(pkgs)

# 02 Load the failed pages ####
failed_pages_to_rescrape <-
    output_folder_scraped_parent %>%
    list.dirs %>%
    .[length(.)] %>%
    paste0(., "/failed_pages.rds") %>%
    readRDS

# 02 Set where data will be saved ####

output_folder_scraped <<-
    paste0(
        output_folder_scraped_parent,
        "failed_pages_rescraped/"
    )

if (!dir.exists(output_folder_scraped)) {
    dir.create(output_folder_scraped)
}

# 03 Loop to scrape the pages ####
total_pages <<- nrow(failed_pages_to_rescrape)

for (page in seq_len(total_pages)) {
    cat(
        "\n\nRe-scraping page",
        page,
        "of",
        total_pages,
        "total pages.\n\n"
    )

    url_to_scrape <<- failed_pages_to_rescrape[["url"]][page]
    html_to_read <<- try_wait_retry(get_html(url_to_scrape))

    # 07 Scrape and test all data from the url. Capture fails. ####

    try_scrape_save_hemnet_html()

}
