library(tidyverse)
library(sf)
library(tidygeocoder)
library(showtext)
library(glue)

# Add Google fonts
font_add_google("Oswald", family = "oswald")
font_add_google("Roboto Condensed", family = "roboto")

# Add local font
font_add("Font Awesome 6 Brands", here::here("fonts/otfs/Font Awesome 6 Brands-Regular-400.otf"))

# Automatically enable the use of showtext for all plots
showtext_auto()

# Set DPI for high-resolution text rendering
showtext_opts(dpi = 300)

# Generate a social media caption with custom colors and font styling
social <- andresutils::social_caption(font_family = "roboto") 

# Construct the final plot caption with TidyTuesday details, data source, and social caption
cap <- paste0(
  "**Source**: Beverly Hills Police Department (BH PD) crime data, 2025–Feb 3, 2026 | **Graphic**: ", social
)

# ----------------------------
# 0) Settings
# ----------------------------
GEOCODER_METHOD <- "arcgis"   # change to "google" or "mapbox" if you have keys
CRS_TRACTS      <- 4269       # NAD83 (as you set)
CRS_ANALYSIS    <- 3310       # CA Albers (good for area/distance in CA)

# ----------------------------
# 1) Read + prep tracts
# ----------------------------
tracts <- st_read("projects/Beverly Hills Crime/map/tracts_study_area.gpkg") |>
  st_set_crs(CRS_TRACTS) |>
  st_transform(CRS_ANALYSIS)

stopifnot(all(st_is_valid(tracts)))
plot(st_geometry(tracts))

# ----------------------------
# 2) Read crimes + build geocoding address string
# ----------------------------
crime_df <- read_csv("projects/Beverly Hills Crime/data/crime_clean_Feb.csv")

crime_addr <- crime_df |>
  mutate(
    is_intersection = str_detect(block_address, " / "),
    address_for_geocode = case_when(
      is_intersection ~ paste0(str_replace(block_address, " / ", " & "), ", Beverly Hills, CA"),
      str_detect(block_address, " Block of ") ~ paste0(str_replace(block_address, " Block of ", " "), ", Beverly Hills, CA"),
      TRUE ~ paste0(block_address, ", Beverly Hills, CA")
    )
  )

# ----------------------------
# 3) Geocode unique addresses only
# ----------------------------
unique_addr <- crime_addr |>
  distinct(address_for_geocode, is_intersection)

addr_geocoded <- unique_addr |>
  geocode(
    address       = address_for_geocode,
    method        = GEOCODER_METHOD,
    lat           = lat,
    long          = lon,
    full_results  = FALSE
  )

# ----------------------------
# 4) Join geocodes back to all crimes
# ----------------------------
crime_geocoded <- crime_addr |>
  left_join(addr_geocoded, by = c("address_for_geocode", "is_intersection"))

# ----------------------------
# 5) QA: geocode coverage
# ----------------------------
geocode_qc <- crime_geocoded |>
  summarise(
    n_total = n(),
    pct_geocoded = mean(!is.na(lon) & !is.na(lat)),
    pct_geocoded_intersections = ifelse(
      sum(is_intersection) == 0,
      NA_real_,
      mean((!is.na(lon) & !is.na(lat))[is_intersection])
    ),
    pct_geocoded_blocks = ifelse(
      sum(!is_intersection) == 0,
      NA_real_,
      mean((!is.na(lon) & !is.na(lat))[!is_intersection])
    )
  )

print(geocode_qc)

# ----------------------------
# 6) Convert to sf points + match CRS
# ----------------------------
crime_pts <- crime_geocoded |>
  filter(!is.na(lon) & !is.na(lat)) |>
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = FALSE) |>
  st_transform(CRS_ANALYSIS)

# quick CRS check (they should match)
st_crs(tracts)
st_crs(crime_pts)

# ----------------------------
# 7) Spatial join: add tract fields to each crime
# ----------------------------
crime_with_tract <- st_join(crime_pts, tracts, join = st_within)

# sanity check: fraction of geocoded points that matched a tract
pct_matched_tract <- mean(!is.na(crime_with_tract$GEOID))
pct_matched_tract

# 2) Non-spatial file (drops geometry)
crime_with_tract |>
  st_drop_geometry() |>
  write_csv("projects/Beverly Hills Crime/data/crime_with_tract.csv")

st_write(
  tracts,
  "projects/Beverly Hills Crime/data/beverly_hills_tracts_3310.gpkg",
  delete_dsn = TRUE
)
