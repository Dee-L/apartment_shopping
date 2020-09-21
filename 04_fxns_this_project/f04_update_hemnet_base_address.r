# Purpose: Update Hemnet base address
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-18
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this scripts are installed ####
pkgs <-
    c()

install_my_pkgs(pkgs)

update_hemnet_base_address <- function() {
    hemnet_base_address <<-
        paste0(
            "https://www.hemnet.se/salda/bostader?",
            "location_ids%5B%5D=",
            hemnet_area_number,
            "&item_types%5B%5D=",
            housing_type,
            "&rooms_min=",
            min_rooms,
            "&fee_max=",
            max_avgift,
            "&selling_price_min=",
            current_min_price,
            "&selling_price_max=",
            current_max_price,
            "&sold_age=",
            days_to_look_back,
            "d",
            "&by=",
            sorting_feature,
            "&order=",
            sorting_direction,
            "&page=",
            hemnet_base_page_number
    )
}