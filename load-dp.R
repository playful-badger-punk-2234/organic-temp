source("functions.R")


# Load in datapackage -----------------------------------------------------

# Path to datapackage.json
dp <- open_datapackage(
  "Z:/DTUFOOD-DL00056/Bronze/raw-shopping-data-dp/2026/09/18/14-11-55/datapackage.json"
)

# Assign resources to a list
shopping_data_tables <- get_data_tables(dp)

# Create codebook
shopping_data_codebook <- create_codebook(dp)

