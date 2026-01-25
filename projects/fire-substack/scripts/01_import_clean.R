# Packages ----------------------------------------------------------------
# Load the main packages we’ll use for importing + cleaning data

library(tidyverse)
library(janitor)

# Read File ---------------------------------------------------------------
# Define file paths:
# - raw_path  = original dataset (do not edit manually)
# - clean_path = cleaned dataset saved into output/data/

raw_path   <- "projects/fire-substack/data/epicla_eaton_palisades_raw.csv"
clean_path <- "projects/fire-substack/output/data/epicla_eaton_palisades_clean.csv"

# Read the raw CSV file into R
df_raw <- read_csv(raw_path)

# Clean column names (standard format: lowercase + underscores)
df_clean <- df_raw %>%
  clean_names()

# Preview the cleaned dataset structure (columns + data types)
glimpse(df_clean)

# Save the cleaned dataset so future scripts can use it
write_csv(df_clean, clean_path)
