# Purpose: Splitting into train and test data
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c("sqldf")

install_my_pkgs(pkgs)

# 02 Load latest preprocessed data ####

preprocessed_data <- 
  paste0(
    output_folder_compiled
    , list.files(output_folder_compiled) %>%
      .[length(.)]) %>%
    readRDS

# 03 Set the random seed ####
set.seed(412)

# 04 Do 80% : 20% split for training : testing ####
sample_size <- floor(0.8 * nrow(preprocessed_data))

index_for_training_data <-
    sample(seq_len(nrow(preprocessed_data)), size = sample_size)

training_data <- preprocessed_data[index_for_training_data, ]
test_data <- preprocessed_data[-index_for_training_data, ]

# 05 specify where to save the different datasets ####
output_folder_tt_split <-
paste0(
    output_folder
    , "05_tt_split/"
    , today_8digit(),
    , "/"
)

if (!dir.exists(output_folder_tt_split)) {
dir.create(output_folder_tt_split)
}

# 06 Save the datasets ####

save_as_r_object_with_its_name(output_folder_tt_split, training_data)
save_as_r_object_with_its_name(output_folder_tt_split, test_data)