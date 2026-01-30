# 01_import_clean.R
# Purpose: Import incidents + dictionary, standardize fields, create plot-ready data,
#          and save cleaned outputs for later visualization scripts.

# Packages
library(tidyverse)
library(janitor)
library(here)

# File Paths
incidents_path   <- here("projects/ice-substack/data", "incidents-2026-01-29.csv")
dictionary_path  <- here("projects/ice-substack/data", "incidents-dictionary.csv")

# Import
incidents_raw <- read_csv(incidents_path, show_col_types = FALSE) %>%
  clean_names()

dictionary_raw <- read_csv(dictionary_path, show_col_types = FALSE) %>%
  clean_names()

std_incident_type <- function(x) {
  x <- str_to_lower(str_squish(x))
  case_when(
    str_detect(x, "brandish") ~ "brandishing",
    str_detect(x, "less\\s*-\\s*lethal|less lethal") ~ "lesslethal",
    str_detect(x, "shoot") ~ "shooting",
    TRUE ~ NA_character_
  )
}

incidents <- incidents_raw %>%
  mutate(
    date = as.Date(date),
    summary = str_squish(summary),
    
    category_raw = str_to_lower(str_squish(as.character(category))),
    
    # standard incident type (prefer incident_type, fallback to category)
    incident_type_std = std_incident_type(incident_type),
    incident_type_std = case_when(
      !is.na(incident_type_std) ~ incident_type_std,
      str_detect(category_raw, "lesslethal") & str_detect(category_raw, "brandishing") ~ "multiple",
      category_raw %in% c("brandishing", "lesslethal", "shooting") ~ category_raw,
      TRUE ~ NA_character_
    ),
    
    civilians_killed  = replace_na(as.integer(civilians_killed), 0L),
    civilians_wounded = replace_na(as.integer(civilians_wounded), 0L),
    
    # outcome derived from counts (fallback to category if needed)
    outcome = case_when(
      civilians_killed > 0 ~ "fatal",
      civilians_wounded > 0 ~ "injured",
      TRUE ~ "noinjury"
    ),
    outcome = case_when(
      outcome != "noinjury" ~ outcome,
      category_raw %in% c("fatal", "injured") ~ category_raw,
      TRUE ~ outcome
    )
  ) %>%
  separate(location, into = c("latitude", "longitude"), sep = ",\\s*", convert = TRUE, remove = FALSE) %>%
  separate(city, into = c("city_name", "state_name"), sep = ",\\s*", fill = "right", extra = "merge", remove = FALSE)

write_csv(incidents, here("projects/ice-substack/output/data", "incidents_clean.csv"))
