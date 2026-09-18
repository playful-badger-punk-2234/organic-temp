# Define packages
library(markdown)
library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
library(RMariaDB)
library(here)
library(readxl)
library(writexl)
library(lubridate)
library(readr)
library(openxlsx)
library(tidyverse)
library(duckdb)
library(OnePlateTools)
library(datapackage)
library(dphelpers)
library(extenddatapackage)
library(purrr)
library(janitor)

# Functions ---------------------------------------------------------------


# Function to append categories to a doc
append_categories <- function(new_data, filepath, logpath) {
  if (file.exists(filepath)) {
    existing_data <- read.csv(filepath, stringsAsFactors = FALSE)
    
    # Fix column names if they were changed by R (e.g. X1 instead of 1)
    if (!identical(names(existing_data), names(new_data))) {
      names(existing_data) <- names(new_data)
    }
    
    combined <- rbind(existing_data, new_data)
    combined_unique <- unique(combined)
    
    new_rows <- setdiff(combined_unique, existing_data)
  } else {
    combined_unique <- new_data
    new_rows <- new_data
  }
  
  write.csv(combined_unique, filepath, row.names = FALSE)
  
  if (nrow(new_rows) > 0) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    log_data <- cbind(Timestamp = timestamp, new_rows)
    
    if (file.exists(logpath)) {
      existing_log <- read.csv(logpath, stringsAsFactors = FALSE)
      updated_log <- rbind(existing_log, log_data)
    } else {
      updated_log <- log_data
    }
    
    write.csv(updated_log, logpath, row.names = FALSE)
  }
}

# Function to load the latest file in a folder
load_latest_file <- function(folder_path, name_pattern = "", file_type = "csv", reader_function = read_csv) {
  # Build the regex pattern for file matching
  pattern <- paste0(name_pattern, ".*\\.", file_type, "$")
  
  # List matching files
  files <- list.files(path = folder_path, pattern = pattern, full.names = TRUE)
  
  # Check if any files were found
  if (length(files) == 0) {
    stop(paste("No files matching pattern", pattern, "found in folder:", folder_path))
  }
  
  # Get file info and find the most recently modified file
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  
  # Read the file using the provided reader function
  data <- reader_function(latest_file)
  
  # Attach filename as attribute
  attr(data, "filename") <- latest_file
  
  return(data)
}


# Read csv-files and make sure they don't have rogue quotes in some of the lines

# Function to clean and read a single file

# read_and_clean <- function(file_path) {
#   # Read raw lines
#   # lines <- readLines(file_path, warn = FALSE)
#   
#   # Remove leading/trailing quotes from each line
#   # clean_lines <- gsub('^"|"$', '', lines)
#   
#   # Write to temp file
#   # temp_file <- tempfile(fileext = ".csv")
#   # writeLines(clean_lines, temp_file)
#   
#   # Read with read_csv and handle embedded commas properly
#   # df <- read_csv(temp_file, quote = "\"", na = c("", "NA"))
#   df <- read_csv(file_path)
#   
#   # Convert date columns to the same format
#   if ("DateOrdered" %in% names(df)) {
#     df$DateOrdered <- parse_date_time(df$DateOrdered, orders = c("ymd", "dmy", "mdy"))
#   }
#   if ("DateDelivery" %in% names(df)) {
#     df$DateDelivery <- parse_date_time(df$DateDelivery, orders = c("ymd", "dmy", "mdy"))
#   }
#   
#   return(df)
#   
# }

# Clean data for column names

clean_label <- function(x) {
  x %>%
    str_replace_all("[æÆ]", "ae") %>%
    str_replace_all("[øØ]", "oe") %>%
    str_replace_all("[åÅ]", "aa") %>%
    str_replace_all("[^[:alnum:] ]", "") %>%  # Remove special characters and parentheses
    str_split(" ") %>%                        # Split by space
    unlist() %>%
    str_to_title() %>%                        # Capitalize each word
    paste0(collapse = "")                     # Combine into one string
}



# Robust importer for Nemlig CSV files (handles line-wrapped quotes and normal CSV)
read_and_clean <- function(file_path) {
  # Read raw lines from file
  raw_lines <- readLines(file_path, encoding = "UTF-8", warn = FALSE)
  
  # If file is empty, return empty tibble
  if (length(raw_lines) == 0) {
    return(tibble())
  }
  
  # Detect if most data lines are wrapped in outer quotes
  # We ignore the header (first line) for this check
  data_lines <- raw_lines[-1]
  has_outer_quotes <- grepl('^".*"$', data_lines)
  
  # If majority of data lines are wrapped, strip one leading and one trailing quote
  if (mean(has_outer_quotes) > 0.8) {
    # Remove exactly one leading and one trailing quote per line, keep inner quotes
    clean_lines <- gsub('^"(.*)"$', '\\1', raw_lines)
  } else {
    # Use lines as-is for standard CSV
    clean_lines <- raw_lines
  }
  
  # Collapse cleaned lines to a single string for readr
  txt <- paste(clean_lines, collapse = "\n")
  
  # Parse CSV with readr (comma-separated, standard quoting)
  df <- readr::read_csv(
    txt,
    show_col_types = TRUE,
    # Ensure date columns are read as character to handle mixed formats
    col_types = cols(
      SalesOrderNumber            = col_integer(),
      Email__c                    = col_character(),
      DateOrdered                 = col_date(),
      DateDelivery                = col_date(),
      ProductSku                  = col_integer(),
      ProductName                 = col_character(),
      Description2                = col_character(),
      Description3                = col_character(),
      GroupLevel1                 = col_character(),
      GroupLevel2                 = col_character(),
      GroupLevel3                 = col_character(),
      ManufacturerName            = col_character(),
      AttributesMarkings          = col_character(),
      QuantityProductsInvoiced    = col_double(),
      AttributesCountryOfOrigin   = col_character(),
      CurrentSalesPriceNemlig     = col_double(),
      ProductBrandName            = col_character(),
      CustomerEmail               = col_character()
    )
  )
  
  # Parse date columns robustly (handle both "YYYY-MM-DD" and "DD/MM/YYYY")
  if ("DateOrdered" %in% names(df)) {
    df$DateOrdered <- suppressWarnings(
      parse_date_time(df$DateOrdered, orders = c("ymd", "dmy"))
    )
  }
  if ("DateDelivery" %in% names(df)) {
    df$DateDelivery <- suppressWarnings(
      parse_date_time(df$DateDelivery, orders = c("ymd", "dmy"))
    )
  }
  
  # Return cleaned data frame
  return(df)
}


read_and_clean <- function(file_path) {
  df <- data.table::fread(
    file_path,
    sep = ",",
    quote = "\"",
    fill = TRUE,
    encoding = "UTF-8",
    showProgress = TRUE,
    data.table = FALSE
  )
  
  # Harmonise email column name
  if ("Email__c" %in% names(df) && !"CustomerEmail" %in% names(df)) {
    df$CustomerEmail <- df$Email__c
  }
  
  # Force ID columns to character
  if ("SalesOrderNumber" %in% names(df)) {
    df$SalesOrderNumber <- as.character(df$SalesOrderNumber)
  }
  if ("ProductSku" %in% names(df)) {
    df$ProductSku <- as.character(df$ProductSku)
  }
  
  # Robust date parsing
  if ("DateOrdered" %in% names(df)) {
    df$DateOrdered <- suppressWarnings(
      lubridate::parse_date_time(df$DateOrdered, orders = c("ymd", "dmy"))
    )
  }
  if ("DateDelivery" %in% names(df)) {
    df$DateDelivery <- suppressWarnings(
      lubridate::parse_date_time(df$DateDelivery, orders = c("ymd", "dmy"))
    )
  }
  
  df
}

clean_rows <- function(df) {
  keep <- rowSums(!is.na(df) & df != "") > 3
  df[keep, , drop = FALSE]
}


# dp functions ------------------------------------------------------------


timestamped_output_path <- function(output_path, timestamp = Sys.time()) {
  file.path(
    output_path,
    format(timestamp, "%Y"),
    format(timestamp, "%m"),
    format(timestamp, "%d"),
    format(timestamp, "%H-%M-%S")
  )
}
