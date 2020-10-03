# Purpose: Save hemnet object after scraping
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-17
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c()

install_my_pkgs(pkgs)

# 02 Gather results ####

save_hemnet_object <- function() {
    sold_object <- data.frame(
        selling_price = na_if_empty(selling_price)
        , asking_price = na_if_empty(asking_price)
        , rooms = na_if_empty(rooms)
        , kvm = na_if_empty(kvm)
        , floor_in_building = na_if_empty(floor_in_building)
        , avgift = na_if_empty(avgift)
        , running_costs = na_if_empty(running_costs)
        , city = na_if_empty(city)
        , area = na_if_empty(area)
        , street = na_if_empty(street)
        , day_of_month_sold = na_if_empty(day_of_month_sold)
        , month_sold_swedish = na_if_empty(month_sold_swedish)
        , year_sold = na_if_empty(year_sold)
        , year_built = na_if_empty(year_built)
        , agent_name = na_if_empty(agent_name)
        , agency = na_if_empty(agency)
        , url = na_if_empty(url_to_scrape)
    )

    # 03 Create an object name ####

    object_name <-
        paste0(
            "o"
            , year_sold
            , left(month_sold_swedish, 3)
            , day_of_month_sold
            , "_"
            , city
            , "_"
            , street
            , "_"
            , street_number
            , ".rda"
        )

    assign(object_name, sold_object)

    # 04 Save the object ####

    save(
        list = object_name
        , file = paste0(output_folder_scraped, object_name)
        )

    if (object_name %not_in% list.files(output_folder_scraped)) {
        message(
            paste0(
                object_name
                , " not found in "
                , output_folder_scraped
                , ". Some kind of untracked failure with "
                , url_to_scrape))
    }

}