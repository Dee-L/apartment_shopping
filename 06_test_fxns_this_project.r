# Purpose: Test functions for this project
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-13
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Testing data that was scraped ####

test_address <- function(address = address) {
        if (
      any(
        (length(address) == 0)
        , (!is.character(address))
        , (!(length(address) > 0))
        )
      ) {
        stop("Impossible address data")
      }
}

test_street_number <- function(street_number = street_number) {
    if (
      any(
        (length(street_number) == 0)
        , !(street_number > 0)
        )
      ) {
        stop("Impossible street number")
      }
}

test_street <- function(street = street) {
    if (
      any(
        (length(street) == 0)
        , (!is.character(street))
        , (!(length(street) > 0))
        )
      ) {
        stop("Impossible street")
      }
}

test_floor_in_building <- function(floor_in_building = floor_in_building) {
    if (!is.na(floor_in_building)) {
      if (
        any(
          (length(floor_in_building) == 0)
          , (floor_in_building < 1)
          , (floor_in_building > 40)
          )
        ) {
          stop("Impossible floor_in_building")
        }
      }
}

test_type_area_date <- function(type_area_date = type_area_date) {
    if (
          any(
            (length(type_area_date) == 0)
            , !is.character(type_area_date)
            )
          ) {
            stop("Impossible type_area_date on top of page.")
          }
}

test_type <- function(type = type) {
    if (
          any(
            (length(type) == 0)
            , (grep("lägenhet", type) < 1)
            )
          ) {
          stop("Not a lägenhet")
          }
}

test_city <- function(city = city) {
    if (
          any(
            (length(city) == 0)
            , (grep(
                paste(
                  c("stockholm", "solna", "uppsala"),
                  collapse = "|"),
                city) < 1)
            )
          ) {
          stop("Not in Stockholm, Solna, or Uppsala")
        }
}

test_day_of_month_sold <- function(day_of_month_sold = day_of_month_sold) {
    if (
          any(
            (length(day_of_month_sold) == 0)
            , (day_of_month_sold <= 0)
            , (day_of_month_sold > 31)
            )
          ) {
          stop("Impossible day of month sold")
          }
}

test_month_sold_swedish <- function(month_sold_swedish = month_sold_swedish) {
    if (
          any(
            (length(month_sold_swedish) == 0)
            , (
                grep(
                  paste(
                    c(
                      "jan", "feb", "mar",
                      "apr", "maj", "jun",
                      "jul", "aug", "sep",
                      "okt", "nov", "dec"
                      ),
                    collapse = "|"),
                  month_sold_swedish) < 1)
              )
              ) {
          stop("Impossible month sold")
        }
}

test_year_sold <- 
    function(year_sold = year_sold, day_of_month_sold = day_of_month_sold) {
        if (
          any(
            (length(year_sold) == 0)
            ,  (year_sold < 2010)
            , (day_of_month_sold > 20)
            )
          ) {
          stop("Impossible year sold")
          }
}

test_final_price <- function(final_price = final_price) {
    if (
          any(
            (length(final_price) == 0)
            , (final_price < 1000000)
            , (final_price > 10000000)
            )
          ) {
            stop("Impossible final price")
      }
}

test_asking_price <- function(asking_price = asking_price) {
    if (
          any(
            (length(asking_price) == 0)
            , (asking_price < 0)
            , (asking_price > 12000000)
            )
          ) {
            stop("Impossible asking_price")
        }
}

test_rooms <- function(rooms = rooms) {
    if (
          any(
            (length(rooms) == 0)
            , (rooms < 1)
            , (rooms > 10)
            )
          ) {
          stop("Impossible number of rooms")
          }
}

test_kvm <- function(kvm = kvm) {
    if (
          any(
            (length(kvm) == 0)
            , (kvm < 10)
            , (kvm > 200)
            , (kvm <= rooms)
            )
          ) {
          stop("Impossible kvm")
          }
}

test_avgift <- function(avgift = avgift) {
    if (
          any(
            (length(avgift) == 0)
            , (avgift < 500)
            , (avgift > 20000)
            )
          ) {
          stop("Impossible avgift")
          }
}

test_running_costs <- function(running_costs = running_costs) {
    if (length(running_costs) == 0) {
          assign("running_costs",
          as.numeric(NA),
          envir = .GlobalEnv)
        }

        if (!is.na(running_costs)) {
          if (
            any(
              (length(running_costs) == 0)
              , (running_costs < 500)
              , (running_costs > 20000)
              )
            ) {
            stop("Impossible running costs")
          }
        }
}

test_year_built <-
    function(year_built = year_built, year_sold = year_sold) {
        if (
          any(
            (length(year_built) == 0)
            , (year_built < 1250)
            , (year_built > 2020)
            , (year_built > year_sold)
            )
          ) {
          stop("Impossible year built")
          }
}

# 02 Test all scraped data ####

test_all <- function() {
    test_address()
    test_street_number()
    test_street()
    test_floor_in_building()
    test_type_area_date()
    test_type()
    test_city()
    test_day_of_month_sold()
    test_month_sold_swedish()
    test_year_sold()
    test_final_price()
    test_asking_price()
    test_rooms()
    test_kvm()
    test_avgift()
    test_running_costs()
    test_year_built()
}