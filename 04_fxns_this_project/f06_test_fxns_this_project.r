# Purpose: Test functions for this project.
# Author: David Gray Lassiter, PhD
# Date: 2020-Sep-13
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####

pkgs <-
  c()

install_my_pkgs(pkgs)

# 02 Testing data that was scraped and throwing error if problem ####

test_street_number <- function(s_n = street_number) {
  if (!is.na(s_n)) {
    if (
      any(
        (length(s_n) == 0)
        , (!(length(s_n) > 0))
        , !(s_n > 0)
        , !(is.numeric(s_n))
        )
      ) {
        stop("Impossible street number")
      }
  }
}

test_street <- function(s = street) {
  if (
    any(
      (length(s) == 0)
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
          , (f_i_b < 0)
          , (f_i_b > 40)
          )
        ) {
          stop("Impossible floor_in_building")
        }
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
  if (!is.na(c)) {
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
}

test_dayofmonth_sold <- function(d_o_m_s = dayofmonth_sold) {
  if (!is.na(d_o_m_s)) {
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
}

test_month_sold_swedish <- function(m_s_s = month_sold_swedish) {
  if (!is.na(m_s_s)) {
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
}

test_year_sold <-
    function(y_s = year_sold, d_o_m_s = dayofmonth_sold) {
      if (!is.na(y_s)) {
        if (
          any(
            (length(y_s) == 0)
            ,  (y_s < 2010)
            , y_s > 2020
            )
          ) {
          stop("Impossible year sold")
          }
      }
}

test_selling_price <- function(f_p = selling_price) {
    if (
          any(
            (length(f_p) == 0)
            , (f_p < first_min_price)
            , (f_p > final_max_price)
            )
          ) {
            stop("Impossible final price")
      }
}

test_asking_price <- function(a_p = asking_price) {
  if (!is.na(a_p)) {
    if (
          any(
            (length(a_p) == 0)
            , (a_p < 0)
            )
          ) {
            stop("Impossible asking_price")
        }
  }
}

test_rooms <- function(r = rooms) {
  if (!is.na(r)) {
    if (
          any(
            (length(r) == 0)
            )
          ) {
          stop("Impossible number of rooms")
          }
    }
}

test_kvm <- function(k = kvm) {
  if (!is.na(k)) {
    if (
          any(
            (length(k) == 0)
            , (k <= rooms)
            )
          ) {
          stop("Impossible kvm")
          }
  }
}

test_avgift <- function(a = avgift) {
  if (!is.na(a)) {
    if (
          any(
            (length(a) == 0)
            )
          ) {
          stop("Impossible avgift")
          }
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
              )
            ) {
            stop("Impossible running costs")
          }
        }
}

test_year_built <-
    function(y_b = year_built, y_s = year_sold) {
      if (!is.na(y_b)) {
        if (
          any(
            (length(y_b) == 0)
            , (y_b > y_s)
            )
          ) {
          stop("Impossible year built")
          }
      }
}

# 03 Test all scraped data ####

test_all <- function() {
    test_street_number()
    test_street()
    test_floor_in_building()
    test_type()
    test_city()
    test_dayofmonth_sold()
    test_month_sold_swedish()
    test_year_sold()
    test_selling_price()
    test_asking_price()
    test_rooms()
    test_kvm()
    test_avgift()
    test_running_costs()
    test_year_built()
}