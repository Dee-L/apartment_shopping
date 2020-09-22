# Purpose: Compiling scraped Hemnet data into a df
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-18
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:


# 01 Ensure all pkgs in this scripts are installed ####
pkgs <-
  c()

install_my_pkgs(pkgs)

# 02 Check if need to compile new data ####

    # look at date of latest results and if greater than date of latest scraped
    # no need to compile new

# 02 create directory for scraping data today ####

    # Want to change naming so that the files get the date name,
    # since only one file per date no need to nest in a folder with the date
output_folder_compiled_parent <<-
  paste0(
    output_folder,
    "02_compiled/"
  )

if (!dir.exists(output_folder_compiled_parent)) {
  dir.create(output_folder_compiled_parent)
}



output_folder_compiled <<-
  paste0(
    output_folder_compiled_parent,
    "date_",
    gsub("-", "", Sys.Date()),
    "/"
  )

if (!dir.exists(output_folder_compiled)) {
  dir.create(output_folder_compiled)
}

# 03 compile the data ####

# Want to update this so it doesn't compile everything each time,
# but just takes the latest compiled results, and then adds to those from whatever
# has been scraped since

for (file in list.files("07_outputs/01_scraped/", recursive = T)) {

    # create path for file
    this_path <-
        paste0("07_outputs/01_scraped/", file)

    # ignore the failed_pages files
    if (!(grepl("failed_pages.rds", this_path))) {

        cat("Adding: ", file, "\n\n")

        # save into a temp_df the file
        temp_df <<- reassign_rda(this_path)

        # create or append to results_df
        if (!exists("results_df")) {
            results_df <<- temp_df
        } else {
            results_df <<- rbind(results_df, temp_df)
        }

    }
}

# 04 save the object ####
save_as_r_object_with_its_name(results_df, output_folder_compiled)

