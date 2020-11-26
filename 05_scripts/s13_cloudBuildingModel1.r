# Purpose: Building Ranger Random Forest Models 1 or 2
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Inspired by:
# https://juliasilge.com/blog/sf-trees-random-tuning/
# https://juliasilge.com/blog/xgboost-tune-volleyball/
# https://juliasilge.com/blog/wind-turbine/

# 01 Ensure all pkgs in this script are installed ####
# install.packages("here")
# install.packages("tidyverse")
# install.packages("ranger")
# install.packages("tidymodels")
# install.packages("doParallel")
# install.packages("vip")

library("here")
library("tidyverse")
library("ranger")
library("tidymodels")
library("doParallel")
library("vip")

# 02 Set paths ####
setwd('~')

importPath <- "11_dataForModel1/"

originalExportPath <- "02_cloudExportData/"

rfMode <- 'regression'
startingModel <- 1
thisIsATestRun <- T

# 03 Establish starting best values for comparing to ####
# lackAskingPriceBestRsq <-
haveAskingPriceBestRsq <- 0.9744827


# 04 Source macro ####
source("m02_buildModel.r")
