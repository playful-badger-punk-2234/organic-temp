source("functions.R")

source("load-dp.R")

latest_data <- shopping_data_tables$raw_nemlig_data %>% 
  select(-Email) %>% 
  distinct()

# latest_data %>% select(-CurrentSalesPriceNemlig, -PriceUnit) %>% distinct() %>%  group_by(SalesOrderNumber, ProductSku, QuantityProductsInvoiced) %>% mutate(n = n()) %>% filter(n>1) %>% arrange(SalesOrderNumber, ProductSku) %>% View()

categories_keep <- shopping_data_tables$categories_to_keep

categories_removed <- latest_data %>% 
  filter(
    !Categories %in% categories_keep$Categories
  ) %>% 
  select(
    contains("GroupLevel"),
    Categories
  ) %>% 
  distinct() %>% 
  arrange(
    GroupLevel1, GroupLevel2, GroupLevel3
  )


nemlig_encoded <- latest_data %>%
  filter(
   Categories %in% categories_keep$Categories 
  ) %>% 
  # Split attibute markings into list
  mutate(attributes = strsplit(AttributesMarkings, ",\\s*")) %>%
  # Replace empty attributes with text
  mutate(attributes = ifelse(attributes == "character(0)", "nomarking", attributes)) %>% 
  # Expand the attributes into long format
  unnest(attributes) %>%
  mutate(
    attributes = trimws(attributes),
    attributes = make_clean_names(attributes, allow_dupes = TRUE),
    value = TRUE
    ) %>%
  # Pivot the table to wide format with each attribute = one column
  pivot_wider(
    names_from  = attributes,
    values_from = value,
    values_fill = FALSE
  ) %>% 
  # Remove the column that shows no attribute
  select(-nomarking)




nemlig_encoded2 <- nemlig_encoded %>%
  # Create variable with organic TRUE/FALSE
  mutate(
    organic = oko_dansk | oko_europaeisk
  ) %>% 
  mutate(
    # Use a custom function to extract important text from description fields
    desc2_tokens = clean_tokens(Description2),
    desc3_tokens = clean_tokens(Description3)
  ) %>%
  mutate(
    # Remove from description2 what is also in description3
    desc2_tokens_clean = map2(
      desc2_tokens,
      desc3_tokens,
      ~ setdiff(.x, .y)
    )
  ) %>%
  # mutate(
  #   Description2_clean = map_chr(
  #     desc2_tokens_clean,
  #     ~ if (length(.x)) str_c(.x, collapse = " / ") else NA_character_
  #   )
  # ) %>%
  select(-desc2_tokens, -desc3_tokens) %>%
  
  # This will extract new columns with letters, mixed, and numeric values from description2
  mutate(
    # Alpha: letters only
    desc2_alpha = map(
      desc2_tokens_clean,
      ~ .x[str_detect(.x, "\\p{L}") & !str_detect(.x, "\\p{N}")]
    ),
    
    # Mixed:
    #  - letters + digits
    #  - OR numeric but NOT integer (decimals)
    desc2_mixed = map(
      desc2_tokens_clean,
      ~ .x[
        (str_detect(.x, "\\p{L}") & str_detect(.x, "\\p{N}")) |
          (str_detect(.x, "^\\p{N}+[\\.,]\\p{N}+$"))
      ]
    ),
    
    # Num: integers only
    desc2_num = map(
      desc2_tokens_clean,
      ~ .x[str_detect(.x, "^\\p{N}+$")]
    )
  ) %>%
  mutate(
    desc2_alpha = map_chr(
      desc2_alpha,
      ~ {
        if (length(.x) > 0) {
          str_c(.x, collapse = " / ")
        } else {
          NA_character_
        }
      }
    ),
    desc2_num = map_chr(
      desc2_num,
      ~ {
        if (length(.x) > 0) {
          str_c(.x, collapse = " / ")
        } else {
          NA_character_
        }
      }
    )
    # # Collapse alpha and num to atomic character columns
    # desc2_alpha = map_chr(
    #   desc2_alpha,
    #   ~ if (length(.x)) str_c(.x, collapse = " / ") else NA_character_
    # ),
    # desc2_num = map_chr(
    #   desc2_num,
    #   ~ if (length(.x)) str_c(.x, collapse = " / ") else NA_character_
    # )
  ) %>% 
  select(-desc2_tokens_clean) %>% 
  mutate(row_id = row_number()) %>% 
  unnest_longer(
    desc2_mixed,
    values_to = "desc2_mixed",
    indices_to = "mixed_pos"
  ) %>% 
  filter(
    !grepl("Klasse \\d", desc2_mixed)
  ) %>% 
  mutate(
    str_split(desc2_mixed, "")
  )





nemlig_mixed_long <- nemlig_encoded2 %>%
  filter(
    !grepl("Bleer og tilbehør", GroupLevel2),
    !grepl("Plejeprodukter", GroupLevel2)
  ) %>% 
  mutate(row_id2 = row_number()) %>%
  unnest_longer(desc2_mixed, values_to = "mixed_token", indices_to = "mixed_pos2") %>%
  filter(!is.na(mixed_token), mixed_token != "") %>%
  mutate(parsed = map(mixed_token, parse_net_amount)) %>%
  unnest(parsed) %>% 
  mutate(
    purchase_amount = amount_std * QuantityProductsInvoiced
  )

nemlig_mixed_long %>% filter(is.na(purchase_amount)) %>% View()
