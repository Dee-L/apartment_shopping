# Purpose: Test functions for this project
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-13
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this scripts are installed ####

pkgs <-
  c()

install_my_pkgs(pkgs)

# 02 Testing data that was scraped ####

test_address <- function(a = address) {
        if (
      any(
        (length(a) == 0)
        , (!is.character(a))
        , (!(length(a) > 0))
        )
      ) {
        stop("Impossible address data")
      }
}

test_street_number <- function(s_n = street_number) {
    if (
      any(
        (length(s_n) == 0)
        , !(s_n > 0)
        )
      ) {
        stop("Impossible street number")
      }
}

test_street <- function(s = street) {
    if (
      any(
        (length(s) == 0)
        , (!is.character(s))
        , (!(length(s) > 0))
        )
      ) {
        stop("Impossible street")
      }
}

test_floor_in_building <- function(f_i_b = floor_in_building) {
    if (!is.na(f_i_b)) {
      if (
        any(
          (length(f_i_b) == 0)
          , (f_i_b < 1)
          , (f_i_b > 40)
          )
        ) {
          stop("Impossible floor_in_building")
        }
      }
}

test_type_area_date <- function(t_a_d = type_area_date) {
    if (
          any(
            (length(t_a_d) == 0)
            , !is.character(t_a_d)
            )
          ) {
            stop("Impossible type_area_date on top of page.")
          }
}

test_type <- function(t = type) {
    if (
          any(
            (length(t) == 0)
            , (grep("lägenhet", t) < 1)
            )
          ) {
          stop("Not a lägenhet")
          }
}

test_city <- function(c = city) {
    if (
          any(
            (length(c) == 0)
            , (grep(
                paste(
                  c("stockholm", "solna", "uppsala"),
                  collapse = "|"),
                c) < 1)
            )
          ) {
          stop("Not in Stockholm, Solna, or Uppsala")
        }
}

test_day_of_month_sold <- function(d_o_m_s = day_of_month_sold) {
    if (
          any(
            (length(d_o_m_s) == 0)
            , (d_o_m_s <= 0)
            , (d_o_m_s > 31)
            )
          ) {
          stop("Impossible day of month sold")
          }
}

test_month_sold_swedish <- function(m_s_s = month_sold_swedish) {
    if (
          any(
            (length(m_s_s) == 0)
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
                  m_s_s) < 1)
              )
              ) {
          stop("Impossible month sold")
        }
}

test_year_sold <-
    function(y_s = year_sold, d_o_m_s = day_of_month_sold) {
        if (
          any(
            (length(y_s) == 0)
            ,  (y_s < 2010)
            , (d_o_m_s > 20)
            )
          ) {
          stop("Impossible year sold")
          }
}

test_final_price <- function(f_p = final_price) {
    if (
          any(
            (length(f_p) == 0)
            , (f_p < 1000000)
            , (f_p > 10000000)
            )
          ) {
            stop("Impossible final price")
      }
}

test_asking_price <- function(a_p = asking_price) {
    if (
          any(
            (length(a_p) == 0)
            , (a_p < 0)
            , (a_p > 12000000)
            )
          ) {
            stop("Impossible asking_price")
        }
}

test_rooms <- function(r = rooms) {
    if (
          any(
            (length(r) == 0)
            , (r < 1)
            , (r > 10)
            )
          ) {
          stop("Impossible number of rooms")
          }
}

test_kvm <- function(k = kvm) {
    if (
          any(
            (length(kvm) == 0)
            , (k < 10)
            , (k > 200)
            , (k <= rooms)
            )
          ) {
          stop("Impossible kvm")
          }
}

test_avgift <- function(a = avgift) {
    if (
          any(
            (length(a) == 0)
            , (a < 500)
            , (a > 20000)
            )
          ) {
          stop("Impossible avgift")
          }
}

test_running_costs <- function(r_c = running_costs) {
    if (length(r_c) == 0) {
          assign("running_costs",
          as.numeric(NA),
          envir = .GlobalEnv)
        }

        if (!is.na(r_c)) {
          if (
            any(
              (length(r_c) == 0)
              , (r_c < 500)
              , (r_c > 20000)
              )
            ) {
            stop("Impossible running costs")
          }
        }
}

test_year_built <-
    function(y_b = year_built, y_s = year_sold) {
        if (
          any(
            (length(y_b) == 0)
            , (y_b < 1250)
            , (y_b > 2020)
            , (y_b > y_s)
            )
          ) {
          stop("Impossible year built")
          }
}

# 03 Test all scraped data ####

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