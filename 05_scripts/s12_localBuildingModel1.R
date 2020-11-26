# Purpose: Building Ranger Random Forest Model 1
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
setwd(here())

importPath <- '07_outputs/11_dataForModel1/'

originalExportPath <- '07_outputs/12_buildingModel1/01_localExportData/'

rfMode <- "regression"
startingModel <- 1
thisIsATestRun <- F

# 03 Source macro ####
# source("11_macros/m02_buildModel1.r")
source("11_macros/m02_buildModel.r")
