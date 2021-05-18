#' download_data.R
#'
#' Downloads the data sets for group assignment 2
#' of Social Media and Web Analytics at TiSEM in 2021
#' 

# --- Load Libraries --- #
library(googledrive)

# --- Info --- #
data_id <- "1XR6RSd6p6A2kBH11s8w_5K60nJvuFmlY"
out_file <- "data/assignment-02.zip"

#---- Download --- #
drive_download(
  as_id(data_id), 
  path = out_file, 
  overwrite = TRUE)

# --- Unzip and Clean up --- #
unzip(out_file,
      exdir = "data")

file.remove(out_file)
