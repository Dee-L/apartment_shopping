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

installMyPkgs(pkgs)

# 02 Testing data that was scraped and throwing error if problem ####

testStreetNumber <- function(sN = streetNumber) {
  if (!is.na(sN)) {
    if (
      any(
        (length(sN) == 0)
        , (!(length(sN) > 0))
        , !(sN > 0)
        , !(is.numeric(sN))
        )
      ) {
        stop("Impossible street number")
      }
  }
}

testStreet <- function(s = street) {
  if (
    any(
      (length(s) == 0)
      , (!(length(s) > 0))
      )
    ) {
      stop("Impossible street")
    }
}

testFloor <- function(fIB = floor) {
    if (!is.na(fIB)) {
      if (
        any(
          (length(fIB) == 0)
          , (fIB < 0)
          , (fIB > 40)
          )
        ) {
          stop("Impossible floor")
        }
      }
}

testType <- function(t = type) {
    if (
          any(
            (length(t) == 0)
            , (grep("lägenhet", t) < 1)
            )
          ) {
          stop("Not a lägenhet")
          }
}

testCity <- function(c = city) {
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

testDayOfMonthSold <- function(dOMS = dayOfMonthSold) {
  if (!is.na(dOMS)) {
    if (
          any(
            (length(dOMS) == 0)
            , (dOMS <= 0)
            , (dOMS > 31)
            )
          ) {
          stop("Impossible day of month sold")
          }
  }
}

testMonthSoldSwedish <- function(mSS = monthSoldSwedish) {
  if (!is.na(mSS)) {
    if (
          any(
            (length(mSS) == 0)
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
                  mSS) < 1)
              )
              ) {
          stop("Impossible month sold")
        }
  }
}

testYearSold <-
    function(yS = yearSold, dOMS = dayOfMonthSold) {
      if (!is.na(yS)) {
        if (
          any(
            (length(yS) == 0)
            ,  (yS < 2010)
            , yS > 2020
            )
          ) {
          stop("Impossible year sold")
          }
      }
}

testSellingPrice <- function(fP = sellingPrice) {
    if (
          any(
            (length(fP) == 0)
            , (fP < firstMinPrice)
            , (fP > finalMaxPrice)
            )
          ) {
            stop("Impossible final price")
      }
}

testAskingPrice <- function(aP = askingPrice) {
  if (!is.na(aP)) {
    if (
          any(
            (length(aP) == 0)
            , (aP < 0)
            )
          ) {
            stop("Impossible askingPrice")
        }
  }
}

testRooms <- function(r = rooms) {
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

testKvm <- function(k = kvm) {
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

testAvgift <- function(a = avgift) {
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

testRunningCosts <- function(rC = runningCosts) {
    if (length(rC) == 0) {
          assign("runningCosts",
          as.numeric(NA),
          envir = .GlobalEnv)
        }

        if (!is.na(rC)) {
          if (
            any(
              (length(rC) == 0)
              )
            ) {
            stop("Impossible running costs")
          }
        }
}

testYearBuilt <-
    function(yB = yearBuilt, yS = yearSold) {
      if (!is.na(yB)) {
        if (
          any(
            (length(yB) == 0)
            , (yB > yS)
            )
          ) {
          stop("Impossible year built")
          }
      }
}

# 03 Test all scraped data ####

testAll <- function() {
    testStreetNumber()
    testStreet()
    testFloor()
    testType()
    testCity()
    testDayOfMonthSold()
    testMonthSoldSwedish()
    testYearSold()
    testSellingPrice()
    testAskingPrice()
    testRooms()
    testKvm()
    testAvgift()
    testRunningCosts()
    testYearBuilt()
}