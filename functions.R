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
library(tibble)

# Functions ---------------------------------------------------------------
clean_tokens <- function(x) {
  x %>%
    str_remove("^\\s*\\*\\s*") %>%
    str_split("/") %>%
    map(str_squish) %>%
    map(~ .x[.x != ""])
}

# Convert numeric value+unit to a standardized unit (g or ml or pcs)
convert_to_std <- function(value, unit) {
  unit <- tolower(unit)
  unit <- str_replace(unit, "\\.$", "")
  if (unit == "kg")  return(list(measure_type = "weight",  unit_std = "g",   value_std = value * 1000))
  if (unit == "g")   return(list(measure_type = "weight",  unit_std = "g",   value_std = value))
  if (unit == "l")   return(list(measure_type = "volume",  unit_std = "ml",  value_std = value * 1000))
  if (unit == "ml")  return(list(measure_type = "volume",  unit_std = "ml",  value_std = value))
  if (unit == "stk") return(list(measure_type = "pieces",  unit_std = "pcs", value_std = value))
  if (unit == "pt") return(list(measure_type = "pieces",  unit_std = "pcs", value_std = value))
  list(measure_type = NA_character_, unit_std = NA_character_, value_std = NA_real_)
}

# Parse one mixed token to standardized net amount(s)
parse_net_amount <- function(s) {
  raw <- s
  
  # Basic cleanup
  x <- raw %>%
    str_to_lower() %>%
    str_replace_all(",", ".") %>%
    str_replace_all("\\bca\\.?\\b", " ") %>%
    str_replace_all("\\bmin\\.?\\b", " ") %>%
    str_replace_all("\\bmindst\\b", " ") %>%
    str_replace_all("\\+", " ") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
  
  # Remove trailing punctuation
  x <- str_replace(x, "[\\.;]+$", "")
  
  # 1) Multiplication like "6 x 0.33 l" or "12 x 400 g"
  m_mul <- str_match(x, "(\\d+)\\s*[x×]\\s*(\\d+(?:\\.\\d+)?)\\s*(kg|g|l|ml|stk\\.?)(?=\\s|$)")
  if (!is.na(m_mul[1, 1])) {
    n <- as.numeric(m_mul[1, 2])
    v <- as.numeric(m_mul[1, 3])
    u <- str_replace(m_mul[1, 4], "\\.$", "")
    conv <- convert_to_std(n * v, u)
    
    return(tibble(
      token_raw = raw,
      token_clean = x,
      rule = "multiply",
      is_range = FALSE,
      range_side = "single",
      unit_raw = u,
      measure_type = conv$measure_type,
      unit_std = conv$unit_std,
      amount_std = conv$value_std
    ))
  }
  
  # 2) Range like "10-20 g" or "920-1000 g" or "0.80-1.0 kg"
  m_rng <- str_match(x, "(\\d+(?:\\.\\d+)?)\\s*-\\s*(\\d+(?:\\.\\d+)?)\\s*(kg|g|l|ml|stk\\.?)(?=\\s|$)")
  if (!is.na(m_rng[1, 1])) {
    lo <- as.numeric(m_rng[1, 2])
    hi <- as.numeric(m_rng[1, 3])
    u  <- str_replace(m_rng[1, 4], "\\.$", "")
    
    conv_lo <- convert_to_std(lo, u)
    conv_hi <- convert_to_std(hi, u)
    
    return(tibble(
      token_raw = raw,
      token_clean = x,
      rule = "range",
      is_range = TRUE,
      range_side = c("low", "high"),
      unit_raw = u,
      measure_type = conv_lo$measure_type,
      unit_std = conv_lo$unit_std,
      amount_std = c(conv_lo$value_std, conv_hi$value_std)
    ))
  }
  
  # 3) Single amount anywhere in the string
  all_matches <- str_match_all(x, "(\\d+(?:\\.\\d+)?)\\s*(kg|g|l|ml|stk\\.?)(?=\\s|$)")[[1]]
  if (nrow(all_matches) > 0) {
    units <- str_replace(all_matches[, 3], "\\.$", "")
    
    # Prefer weight over volume over pieces
    idx_weight <- which(units %in% c("kg", "g"))
    idx_vol    <- which(units %in% c("l", "ml"))
    idx_pcs    <- which(units %in% c("stk"))
    
    pick <- if (length(idx_weight) > 0) {
      idx_weight[1]
    } else if (length(idx_vol) > 0) {
      idx_vol[1]
    } else if (length(idx_pcs) > 0) {
      idx_pcs[1]
    } else {
      1
    }
    
    v <- as.numeric(all_matches[pick, 2])
    u <- units[pick]
    conv <- convert_to_std(v, u)
    
    return(tibble(
      token_raw = raw,
      token_clean = x,
      rule = "single",
      is_range = FALSE,
      range_side = "single",
      unit_raw = u,
      measure_type = conv$measure_type,
      unit_std = conv$unit_std,
      amount_std = conv$value_std
    ))
  }
  
  # No parseable amount found
  tibble(
    token_raw = raw,
    token_clean = x,
    rule = "no_match",
    is_range = NA,
    range_side = NA_character_,
    unit_raw = NA_character_,
    measure_type = NA_character_,
    unit_std = NA_character_,
    amount_std = NA_real_
  )
}

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


## Load all resources from a datapackage into a named list of data  --------


get_data_tables <- function(dp) {
  
  # Get resource names
  resource_names <- dp_resource_names(dp)
  
  # Load data and use resource names as list names
  setNames(
    lapply(
      resource_names,
      function(name) dp_get_data(dp_resource(dp, name))
    ),
    gsub("-", "_", resource_names, fixed = TRUE)
  )
  
}

## Function for creating a codebook with info on all variables -------------


create_codebook <- function(dp) {
  
  # Get resource names
  resource_names <- dp_resource_names(dp)
  
  # Get resource metadata
  resources <- setNames(
    lapply(resource_names, \(x) dp_resource(dp, x)),
    gsub("-", "_", resource_names, fixed = TRUE)
  )
  
  # Get data tables
  data_tables <- tryCatch(
    setNames(
      lapply(resource_names, \(x) dp_get_data(dp_resource(dp, x))),
      gsub("-", "_", resource_names, fixed = TRUE)
    ),
    error = function(e) NULL
  )
  
  # Extract metadata
  metadata <- purrr::imap_dfr(
    resources,
    function(resource, resource_name) {
      
      fields <- resource$schema$fields
      
      # Handle missing schema
      if (is.null(fields) || length(fields) == 0) {
        
        return(
          tibble::tibble(
            resource = resource_name,
            name = NA_character_
          )
        )
      }
      
      dplyr::bind_rows(
        lapply(fields, tibble::as_tibble)
      ) |>
        dplyr::mutate(
          resource = resource_name,
          .before = 1
        )
    }
  )
  
  # Return metadata only if data unavailable
  if (is.null(data_tables)) {
    
    return(
      metadata |>
        dplyr::mutate(
          resource = factor(resource)
        )
    )
    
  }
  
  # Calculate variable statistics
  stats <- purrr::imap_dfr(
    data_tables,
    function(df, resource_name) {
      
      tibble::tibble(
        resource = resource_name,
        name = names(df),
        class = purrr::map_chr(df, ~ class(.x)[1]),
        n_missing = purrr::map_int(df, ~ sum(is.na(.x))),
        n_unique = purrr::map_int(
          df,
          ~ dplyr::n_distinct(.x, na.rm = TRUE)
        )
      )
    }
  )
  
  # Merge metadata and statistics
  dplyr::full_join(
    metadata,
    stats,
    by = c("resource", "name")
  ) |>
    dplyr::mutate(
      resource = factor(resource)
    )
}
