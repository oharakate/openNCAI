library(readxl)
library(dplyr)
library(stringr)

# Path to your source file
source_path <- "data-raw/ncai_corrected.xlsx"

# --- HABITATS ---
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Get and correct the habitat labels:
raw_habs <- read_excel(
  source_path,
  sheet = "ES Potential per SPU",
  range = "C4:E34",
  col_names = c("broad_cat", "code", "name"),
  col_types = "text"
) %>%
  # Drop the redundant code column immediately
  select(-code) %>%
  # Fill the merged Broad Category values down
  fill(broad_cat, .direction = "down") %>%
  # Keep only the rows that actually have habitat names
  filter(!is.na(name)) %>%
  mutate(
    # Clean up whitespace (tabs, double spaces, etc.) from the 'name' column
    print_name = str_squish(name)
  ) %>%
  # Select only the two columns we need for the tree
  select(broad_cat, print_name)

raw_habs[4, "broad_cat"] <- "C. INLAND SURFACE WATERS"
raw_habs[31, "broad_cat"] <- "K. MONTANE"

print(raw_habs)

# Create label tree:
ns_dirty_habitats_label_tree <- split(raw_habs$print_name, raw_habs$broad_cat)
ns_dirty_habitats_label_tree


# Get the ecosystem service labels:
raw_es_header <- read_excel(
  source_path,
  sheet = "ES Potential per SPU",
  range = "F1:AG3",
  col_names = FALSE,
  col_types = "text"
)
# Transpose and clean them:
raw_es <- as.data.frame(t(raw_es_header)) %>%
  rename(es_type = 1, code = 2, name = 3) %>%
  # Fill the Category (Provisioning, etc.) down
  fill(es_type, .direction = "down") %>%
  mutate(
    # Combine Code and Name: "1.1 Cultivated crops"
    print_name = paste(code, name),
    # Clean up line breaks and extra spaces
    print_name = str_squish(print_name),
    es_type = str_squish(es_type)
  ) %>%
  select(es_type, print_name)
# View(raw_es)

# Create label tree:
ns_dirty_es_label_tree <- split(raw_es$print_name, raw_es$es_type)
ns_dirty_es_label_tree
