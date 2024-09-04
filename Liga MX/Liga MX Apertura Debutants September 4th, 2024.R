library(tidyverse)
library(worldfootballR)
library(showtext)
library(ggtext)
library(ggimage)

font_add_google("Oswald", "Oswald")
font_add_google("Chivo", "Chivo")
font_add("Font Awesome 6 Brands", "fonts/otfs/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()
showtext_opts(dpi = 300)

# add logo
add_logo <- function(plot_path, logo_path, logo_position, 
                     logo_scale = 10){
  # Requires magick R Package https://github.com/ropensci/magick
  # Useful error message for logo position
  if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
    stop("Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'")
  }
  # read in raw images
  plot <- magick::image_read(plot_path)
  logo_raw <- magick::image_read(logo_path)
  # get dimensions of plot for scaling
  plot_height <- magick::image_info(plot)$height
  plot_width <- magick::image_info(plot)$width
  # default scale to 1/10th width of plot
  # Can change with logo_scale
  logo <- magick::image_scale(logo_raw, as.character(plot_width/logo_scale))
  # Get width of logo
  logo_width <- magick::image_info(logo)$width
  logo_height <- magick::image_info(logo)$height
  # Set position of logo
  # Position starts at 0,0 at top left
  # Using 0.01 for 1% - aesthetic padding
  if (logo_position == "top right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "top left") {
    x_pos = 0.01 * plot_width
    y_pos = 0.01 * plot_height
  } else if (logo_position == "bottom right") {
    x_pos = plot_width - logo_width - 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  } else if (logo_position == "bottom left") {
    x_pos = 0.01 * plot_width
    y_pos = plot_height - logo_height - 0.01 * plot_height
  }
  # Compose the actual overlay
  magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
}

# LigaMX players making their LigaMX debut in 2024/2025
ligamx_debutants <- tm_league_debutants(country_name = "", league_url = "https://www.transfermarkt.us/liga-mx-apertura/startseite/wettbewerb/MEXA", debut_type = "pro", debut_start_year = 2023, debut_end_year = 2025)
glimpse(ligamx_debutants)


ligamx_debutants <- ligamx_debutants %>%
  filter(debut_date >= "2024-07-06") 

# Define a reference date (e.g., assuming the debut date is today)
reference_date <- Sys.Date()

# Function to convert the age string to a debut date
convert_to_debut_date <- function(age_string, ref_date) {
  # Extract years, months, and days from the string
  years <- str_extract(age_string, "\\d+ years") %>% str_extract("\\d+") %>% as.numeric()
  months <- str_extract(age_string, "\\d+ months") %>% str_extract("\\d+") %>% as.numeric()
  days <- str_extract(age_string, "\\d+ days") %>% str_extract("\\d+") %>% as.numeric()
  
  # Replace NAs with 0 for missing months/days
  years <- ifelse(is.na(years), 0, years)
  months <- ifelse(is.na(months), 0, months)
  days <- ifelse(is.na(days), 0, days)
  
  # Calculate the debut date
  debut_date <- ref_date - years(years) - months(months) - days(days)
  
  return(debut_date)
}

# Apply the function to the 'age_debut' column of your dataframe
debut_dates <- sapply(ligamx_debutants$age_debut, convert_to_debut_date, ref_date = reference_date)

# Convert numeric dates to Date format
debut_dates_formatted <- as.Date(debut_dates, origin = "1970-01-01")

# Add the formatted debut dates to the dataframe
ligamx_debutants_cleaned <- ligamx_debutants %>%
  mutate(age_debut_corrected = round(as.numeric(difftime(debut_date, debut_dates_formatted, units = "days") / 365.25), 1))

# Assuming ligamx_debutants_cleaned has a column 'age_at_debut' calculated as per previous steps
ligamx_debutants_unique <- ligamx_debutants_cleaned %>%
  group_by(debut_for) %>%               # Group by team
  slice_min(age_debut, n = 1) %>% # Select the youngest debutant per team
  ungroup() %>%
  unique()


# Rename the team names in the 'debut_for' column
ligamx_debutants_unique <- ligamx_debutants_unique %>%
  mutate(debut_for = case_when(
    debut_for == "CF América" ~ "América",
    debut_for == "CF Monterrey" ~ "Monterrey",
    debut_for == "CF Pachuca" ~ "Pachuca",
    debut_for == "Club León FC" ~ "León",
    debut_for == "Club Necaxa" ~ "Necaxa",
    debut_for == "Club Tijuana" ~ "Tijuana",
    debut_for == "Santos Laguna" ~ "Santos",
    debut_for == "Atlético de San Luis" ~ "Atlético",
    debut_for == "Atlas Guadalajara" ~ "Atlas",
    debut_for == "FC Juárez" ~ "FC Juárez",
    debut_for == "Puebla FC" ~ "Puebla",
    debut_for == "Querétaro FC" ~ "Querétaro",
    debut_for == "UNAM Pumas" ~ "UNAM",
    TRUE ~ debut_for  # Keep the original name if not matched
  ))


ligamx_debutants_unique <- ligamx_debutants_unique %>%
  mutate(logo = paste0("Bayesian Leagues Cup/images/logos/", debut_for, ".png"))



source("Bayesian Leagues Cup/social_caption.R")
caption <- paste0(social_caption(font_family = "Oswald", linkedin = "Andres Gonzalez", icon_color= "#007FFF", font_color = "grey50"))

ggplot(ligamx_debutants_unique, aes(x = age_debut_corrected, y = fct_reorder(player_name, -age_debut_corrected), label = age_debut)) +
  geom_bar(stat = "identity", fill = "#007FFF") +
  geom_text(hjust = 1.2, size = 3.5, color = "white", family = "Chivo") +
  geom_image(data = ligamx_debutants_unique, aes(y = fct_reorder(player_name, -age_debut_corrected), x = -1, image = logo), size = 0.047, by = "height") +
  geom_segment(inherit.aes=FALSE, aes(x= 21, xend= 21, y = 3, yend= 12.5), color = "#007FFF", arrow= arrow(length = unit(0.2, "cm"), type="closed", ends='both'), linewidth = 0.8, linejoin = "mitre") +
  annotate(geom="text", x= 21, y = 12.75, label="Youngest", size= 3.5, color = "#007FFF", family = "Oswald") +
  annotate(geom="text", x= 21, y = 2.75, label="Oldest", size= 3.5, color = "#007FFF", family = "Oswald") +
  labs(
    title = "LIGA MX YOUNGEST DEBUTANTS BY TEAM SO FAR <br> IN APERTURA 2024 WITH TIJUANA MAKING HISTORY",
    subtitle = "Youngest debutants from Transfermarkt as of September 4, 2024",
    x = "Age",
    y = NULL,
    caption = caption) +
  theme_minimal() +
  theme(
    text = element_text(family = "Oswald"),
    plot.title = element_markdown(face = "bold", size = 18),
    plot.subtitle = element_text(face = "bold", size = 8),
    plot.caption = element_markdown(size = 7),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.1, color = "grey80")
  )

ggsave("Bayesian Leagues Cup/images/Liga MX Debutants Apertura 2024.png", bg = "white", height = 7, width = 8)

plot_logo <- add_logo(
  plot_path = here::here("Bayesian Leagues Cup/images/Liga MX Debutants Apertura 2024.png"),
  logo_path = here::here("Bayesian Leagues Cup/images/logos/liga mx.png"),
  logo_position = "top right",
  logo_scale = 12)

magick::image_write(image = plot_logo, 
                    here::here("Bayesian Leagues Cup/images/Liga MX Debutants Apertura 2024.png"))


