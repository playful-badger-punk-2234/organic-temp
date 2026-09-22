source("functions.R")


# Load in datapackage -----------------------------------------------------

# Path to datapackage.json
dp <- open_datapackage(
  "Z:/DTUFOOD-DL00056/Bronze/raw-shopping-data-dp/2026/09/22/15-37-25/datapackage.json"
)

# Assign resources to a list
shopping_data_tables <- get_data_tables(dp)

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
