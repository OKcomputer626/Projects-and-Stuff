library(tidyverse)
library(janitor)

# Load raw data -----------------------------------------------------------
crime_raw <- read_csv("projects/Beverly Hills Crime/data/Beverly Hills Crime Feb 6 2026.csv")

crime_clean <- crime_raw %>%
  clean_names()

# View cleaned names
names(crime_clean)

crime_clean <- crime_clean %>%
  mutate(
    occurred_from_dt = mdy_hms(occurred_from_date),
    occurred_through_dt = mdy_hms(occurred_through_date),
    
    # Reported date is messy like: "Jun 23 2017 9:35:24:000PM"
    reported_date_clean = reported_date %>%
      str_squish() %>%
      str_replace(":000", "") %>%
      str_replace("(AM|PM)$", " \\1"),
    
    reported_dt = parse_date_time(
      reported_date_clean,
      orders = "b d Y I:M:S p"
    ),
    
    # Time features from occurred_from_dt
    year = year(occurred_from_dt),
    month = month(occurred_from_dt, label = TRUE, abbr = TRUE),
    wday = wday(occurred_from_dt, label = TRUE, abbr = TRUE),
    hour = hour(occurred_from_dt),
    
    # Reporting delay in days
    days_to_report = as.numeric(difftime(reported_dt, occurred_from_dt, units = "days"))
  ) %>%
  select(-reported_date_clean)

saveRDS(crime_clean, "projects/Beverly Hills Crime/data/datacrime_clean_Feb.rds")
write_csv(crime_clean, "projects/Beverly Hills Crime/data/crime_clean_Feb.csv")
