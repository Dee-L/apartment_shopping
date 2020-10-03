# test_function <- function(object_to_test) {
#     if (length(object_to_test) == 0) {
#         class_to_cast <- class(object_to_test)
#         object_to_test <- NA
#         object_to_test %<>%
#             as(., class_to_cast)
#         object_to_test
#     }
# }



# test_function(as.numeric())


# old_url <- url_to_scrape

# new_url <- "https://www.hemnet.se/salda/lagenhet-2rum-bromma-bromma-kyrka-stockholms-kommun-spangavagen-138,-hogst-upp-1250812"


# urls_to_test <-
#     c(
#         "https://www.hemnet.se/salda/lagenhet-2rum-stockholms-kommun-dobelnsgatan-67-1250737"
#     )

# url_to_scrape <- "https://www.hemnet.se/salda/lagenhet-2rum-stockholms-kommun-dobelnsgatan-67-1250737"

# url_to_scrape <- old_url

# for (url in urls_to_test) {
    
#     if(exists("iterator")) {
#         iterator <- iterator + 1
#     } else {
#         iterator <- 1
#     }
    
#     cat("Testing url", iterator, "of", length(urls_to_test), "\n\n")

#     get_all_variables(url)

# }

# rm(iterator)


# get_day_of_month_sold("https://www.hemnet.se/salda/lagenhet-4rum-kungsangen-uppsala-kommun-muningatan-4-1250838")



# # loads an RData file, and returns it, allowing you to reassign it to a new name
# reassign_rda <- function(file_name) {
#             load(file_name)
#             get(ls()[ls() != "file_name"])
# }




# for (file in
#     list.files("07_outputs//01_scraped/price_1000000_to_2187000/")) {

#         temp_df <<- reassign_rda(
#             paste0(
#                 "07_outputs//01_scraped/price_1000000_to_2187000/"
#                 , file
#                 )
#             )

#         if (!exists("results_df")) {
#            results_df <<- temp_df
#         } else {
#             results_df <<- rbind(results_df, temp_df)
#         }
#     }



# my_rename <- function(old_file) {
#     file.rename(
#         old_file
#         , right(old_file, nchar(old_file) - 11)
#         )
# }

# for (folder in list.dirs()) {

#     if (folder == ".") next

#     message("Moving to: ", folder, "\n\n")
#     setwd(folder)

#     for (file in list.files()) {

#         if (file != "failed_pages.rds") {

#             if (exists("n_renamed")) {
#                 n_renamed <<- n_renamed + 1
#             } else {
#                 n_renamed <<- 1
#             }

#             cat("Renaming: ", file, "\n\n")
#             my_rename(file)

#         }

#     }

#     message("Moving to parent folder.\n\n")
#     setwd("../")

#     cat(n_renamed, " renamed files.\n\n")

# }


# install.packages("mice")
# library('mice')

# df <- data.frame(
#     x = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#     , y = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10) * 2
# )

# test_string <- "\n  Slutpris\n  Adlerbethsgatan 17, 3-4 tr\n"

# # drops commas and dashes
# test_string %>% gsub("[^[:alnum:] ]", "", .)

# # drops dashes
# test_string %>% gsub("[^[:alnum:], ]", "", .)

# # keeps all three
# test_string %>% gsub("[^-[:alnum:], ]", "", .)

# test_string %>% gsub("[^[:alnum:],- ]", "", .)



set.seed(53177)
# Variable to examine in the dataset
xname <- c('age', 'hgt', 'wgt')
# A dataset
boys <- mice::boys

# Boolean if complete case
r <- stats::complete.cases(boys[, xname])

# Only the complete cases - numeric matrix with length y that has predictors
# for y - cannot have missing values
x <- boys[r, xname]

# the column to predict, only keeping complete cases
y <- boys[r, 'tv']

# The indexes that don't need to be filled - logical vector of length y where
# TRUE is an existing value in y and FALSE is one to be imputed
ry <- !is.na(y)
table(ry)
# percentage of missing data in tv
sum(!ry) / length(ry)
# Impute missing tv data

yimp <- mice::mice.impute.pmm(y, ry, x)
yimp2 <- mice::mice.impute.pmm(y, ry, x)
yimp == yimp2
length(yimp)
hist(yimp, xlab = 'Imputed missing tv')
# Impute all tv data
yimp <- mice.impute.pmm(y, ry, x, wy = rep(TRUE, length(y)))
length(yimp)
hist(yimp, xlab = 'Imputed missing and observed tv')
plot(jitter(y), jitter(yimp),
main = 'Predictive mean matching on age, height and weight',
xlab = 'Observed tv (n = 224)',
ylab = 'Imputed tv (n = 224)')
abline(0, 1)
cor(y, yimp, use = 'pair')





set.seed(918273)                                # Seed
 
N <- 10                                       # Sample size
 
y <- round(runif(N, -10, 10))                   # Target variable Y
x1 <- y + round(runif(N, 0, 50))                # Auxiliary variable 1
x2 <- round(y + 0.25 * x1 + rnorm(N, - 3, 15))  # Auxiliary variable 2
x3 <- round(0.1 * x1 + rpois(N, 2))             # Auxiliary variable 3
x4 <- as.factor(round(0.02 * y + runif(N)))     # Auxiliary variable 4 (categorical variable)
 
y[rbinom(N, 1, 0.2) == 1] <- NA                 # Insert 20% missing data in Y
 
data <- data.frame(y, x1, x2, x3, x4)           # Store data in dataset
head(data)                                      # First 6 rows of our data

imputations <- ceiling(sum(is.na(y))/length(y) * 100)

imputations <- 1

imp_multi <- mice::mice(data, m = imputations, maxit = 30, method = "pmm")  # Impute missing values multiple times
# data_imp_multi_all <-

# mice::complete(imp_multi, include = TRUE) # Don't like this, can't tell imputations/iterations
mice::complete(imp_multi, "all", include = TRUE) # imputations nested under $`1` and $`2` for example - not sure how iterations are shown
mice::complete(imp_multi, "long", include = TRUE) # imputations under .imp - not sure how iterations are shown
mice::complete(imp_multi, "broad", include = TRUE) # imputations baked into name - not sure how iterations are shown
mice::complete(imp_multi, "repeated", include = TRUE) # imputations baked into name - not sure how iterations are shown

 
data_imp_multi <- data.frame(                   # Combine imputed Y and X1-X4 (for convenience)
  data_imp_multi_all[ , 1:2], data[, 2:5])
head(data_imp_multi)                            # First 6 rows of our multiply imputed data



# Could not get MICE working
# Instead use interpolation with linear except for endpoints where use min and max




# Can if use multiple imputation, then will get multiple results that need
# to be analyzed.

y <- practice_df[["final_price"]]
x <- data.frame(
  #ds_as_character = practice_df[["date_sold"]]
  # ds_as_number = practice_df[["date_sold"]] %>% as.numeric
  random_nums = sample(18503:18516)
#  , random_nums2 = sample (15:28)
)

ry <- !is.na(y)

mice::mice.impute.pmm(
  y
  , ry
  , x
  , donors = 1
)

# This also isn't working


# The mice with pool strategy doesn't give me my results in a dataframe
# as expected. I also have to specify a model in the with step



# I might be thinking about this wrong. Might be better to just replace
# with a random value from the sample. Otherwise I'm building in linear
# relationships where they may not exist.
# Then again, the date I'm filling in here may be expected to be more
# patterned than the data I filled in for missing data before.


# PAUSED HERE - next need to see if interpolation works for date_sold outside
# of the range - if not, then try other "options" in na_interpolation fxn

# The linear option does make straight lines between two missing values rather
# than the entire data set. i.e., 2, NA, 4, NA, 6, NA, 2 becomes
# 2, 3, 4, 5, 6, 4, 2. This is desirable. The only part that is perhaps
# undesirable is that it carries backward the first observation and carries
# forward the last observation.

# This site indicates multiple imputation is better:
# https://www.tandfonline.com/doi/pdf/10.1081/BIP-120015744

# Mice package:
# https://www.rdocumentation.org/packages/mice/versions/3.11.0/topics/mice

# Mice walkthrough:
# https://gist.github.com/mick001/df77b69b30ef6ff9fc0b

# More about MICE and how to think of hyperparameters
# https://statisticalhorizons.com/predictive-mean-matching


# Much more on MICE using miche %>% with %>% pool workflow
# https://stefvanbuuren.name/fimd/workflow.html

# Original mice documentation
# https://cran.r-project.org/web/packages/mice/mice.pdf

# Major downside with MICE is that you create x imputations of your full
# dataset. You then subject the x versions to whatever analysis you will do
# and later average the results of your summary/inferential statistics.

# https://stats.stackexchange.com/questions/219013/how-do-the-number-of-imputations-the-maximum-iterations-affect-accuracy-in-mul/219049
# Should do imputations that = % missing
# Should do 30 iterations
# Should then extract at random one of the imputations from the last iteration

# imputations <-
#   ceiling(sum(is.na(practice_df[["final_price"]]))/nrow(practice_df) * 100)

# imp_multi <- mice::mice(practice_df, m = imputations, max_iter = 30, method = "pmm")  # Impute missing values multiple times

# data_imp_multi_all <- mice::complete(imp_multi,       # Store multiply imputed data
#                            "repeated",
#                            include = TRUE)
 
# data_imp_multi <- data.frame(                   # Combine imputed Y and X1-X4 (for convenience)
#   data_imp_multi_all[ , 1:6], data[, 2:5])
# head(data_imp_multi) 

#One-hot encoding
# 
# customers <- data.frame(
  # id=c(10, 20, 30, 40, 50),
  # gender=c('male', 'female', 'female', 'male', 'female'),
  # mood=c('happy', 'sad', 'happy', 'sad','happy'),
  # outcome=c(1, 1, 0, 0, 0))
# customers
# 
# 
# 
# dummify the data
# 
# dmy <- caret::dummyVars(" ~ .", data = customers)
# dmy
# caret must actually be loaded for the next step
# library(caret)
# trsf <- data.frame(predict(dmy, newdata = customers))
# 
# detach caret to avoid future conflicts
# detach("package:caret", unload = TRUE)
# 
# trsf

# Motion/timelapse plot

df <- preprocessed_data
x <- "avgift"
y <- "selling_price"
color <- "running_costs"
size <- "rooms"
time_frames <- "year_sold" # date_sold is computationally heavy
# ids <-
# group <- "age_when_sold"
# stroke <- "age_when_sold"
opaqueness <- "rooms"
shape <- "city"


# motion_plot <-
ggplotly(
  ggplot(df) +
    geom_point(
      aes(
        x = df[[x]],
        y = df[[y]],
        color = df[[color]],
        size = df[[size]],
        frame = df[[time_frames]],
        # id
        # group = df[[group]],
        # stroke = df[[stroke]],
        alpha = 1 - 1 / df[[opaqueness]],
        shape = df[[shape]]
      )
    ) +
    scale_color_viridis_c(option = "magma")
)
