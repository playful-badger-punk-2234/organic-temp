source("functions.R")


# Load in datapackage -----------------------------------------------------

# Get the latest version of the dp
latest <- find_latest_timestamped_folder("Z:/DTUFOOD-DL00056/Bronze/raw-shopping-data-dp")

# Import dp as an object
dp <- get_dp(
  dl_path = "Z:/DTUFOOD-DL00056",
  dp_path = "Bronze/raw-shopping-data-dp",
  version = latest
  )


# Assign resources to a list
shopping_data_tables <- get_all_data_from_datapackage_grouped(dp)

# Create codebook
shopping_data_codebook <- create_codebook(dp)


# Check for duplicate rows
dupes <- shopping_data_tables$raw_nemlig_data %>%
  # distinct() %>%
  group_by(SalesOrderNumber, ProductSku, QuantityProductsInvoiced) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(
    n > 1)

if(nrow(dupes) > 0){
  stop("Dataset contains duplicates in 'raw_nemlig_data")
}
