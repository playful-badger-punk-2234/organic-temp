source("functions.R")

nemlig_encoded <- latest_data %>%
  mutate(attributes = strsplit(AttributesMarkings, ",\\s*")) %>%
  unnest(attributes) %>%
  # mutate(
  #   attributes = trimws(attributes),
  #   attributes = str_to_lower(attributes),
  #   attributes = clean_label(attributes),
  #   attributes = as.character(attributes)
  # ) %>%
  filter(!is.na(attributes), attributes != "") %>%
  distinct(across(c(attributes, everything()))) %>%   # Remove duplicate rows
  mutate(value = TRUE) %>%
  pivot_wider(
    names_from  = attributes,
    values_from = value,
    values_fill = FALSE
  )


nrow(latest_data)

nemlig_encoded <- latest_data %>%
  # mutate(row = row_number()) %>% 
  # Convert attributes column to new colums
  mutate(attributes = strsplit(AttributesMarkings, ",\\s*")) %>% 
  unnest(attributes, keep_empty = TRUE) %>%
  # Apply TRUE/FALSE for each attribute and clean label with custom function
  mutate(
    value = TRUE,
    attributes = map(attributes, ~ str_to_lower(trimws(.x))),
    attributes = map(attributes, ~ map_chr(.x, clean_label))
  ) %>% 
  arrange(attributes) %>% 
  # Pivot to a longer table
  pivot_wider(names_from = attributes, values_from = value, values_fill = FALSE) %>%
  # select(-row, -"NA") %>% 
  select(-any_of("NA")) %>%
  mutate(
    Organic = OekoDansk | OekoEuropaeisk
  ) %>% 
  relocate(Organic, OekoDansk, OekoEuropaeisk, .after = ProductName)

nrow(nemlig_encoded)


clean_tokens <- function(x) {
  x %>%
    str_remove("^\\s*\\*\\s*") %>%
    str_split("/") %>%
    map(str_squish) %>%
    map(~ .x[.x != ""])
}

nemlig_encoded2 <- nemlig_encoded %>%
  mutate(
    desc2_tokens = clean_tokens(Description2),
    desc3_tokens = clean_tokens(Description3)
  ) %>%
  mutate(
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

library(dplyr)
library(stringr)
library(purrr)
library(tidyr)
library(tibble)

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