library(tidyverse)
library(janitor)
library(here)
library(showtext)
library(glue)
library(ggtext)
library(elementalist)

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
social <- andresutils::social_caption(font_family = "roboto", bg_color = "#000000", font_color = "#FFFFFF", icon_color = "#FFFFFF") 

# Construct the final plot caption with TidyTuesday details, data source, and social caption
cap <- paste0(
  "**Graphic**: ", social
)

# File Paths
crimes_path <- here("projects/sheriff-substack/data", "PART_I_AND_II_CRIMES-YTD.csv")

# Import + clean names
crimes_raw <- read_csv(crimes_path, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    # Parse incident datetime (DST-safe)
    incident_date = force_tz(
      parse_date_time(
        incident_date,
        orders = "mdy HMS p",
        tz = "UTC"
      ),
      tzone = "America/Los_Angeles"
    ),
    
    # Parse reported date
    incident_reported_date = mdy(incident_reported_date),
    
    # Optional but useful derived fields
    incident_day  = as_date(incident_date),
    incident_hour = hour(incident_date),
    incident_month = month(incident_day, label = TRUE, abbr = TRUE),
    incident_year = year(incident_day),
    city = str_to_title(city),
    incident_id = unique(incident_id)
  )


compton_df <- crimes_raw %>%
  filter(city == "Compton") %>%
  count(category, name = "value") %>%
  slice_max(value, n = 10) %>%
  mutate(rank = row_number())

tag <- glue(
  "<span style='font-size:20pt;'>MOST-REPORTED<br>CRIMES</span><br>
   <span style='font-size:7pt;'>COMPTON · 2025</span>"
)


p <- compton_df %>%
  ggplot(aes(x = reorder(category, value), y = value)) +
  geom_col_theme(fill = "#FFFFFF", width = 0.92, element = element_rect_round(radius = 0.1)) +
  geom_text(aes(y = 0, label = rank), nudge_y = 25, hjust = 0, size = 4, family = "oswald", fontface = "bold") +
  geom_text(aes(label = value), hjust = 0, nudge_y = 25, size = 7, family = "oswald", fontface = "bold", color = "#FFFFFF") +
  geom_text(aes(y = 0, label = category), hjust = 0, nudge_y = 60, size = 4, family = "oswald", fontface = "bold") +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1700)) +
  coord_flip(clip = "off") +
  labs(
    title = "Los Angeles County Sheriff's Department",
    subtitle = cap,
    tag = tag) +
  theme_void() +
  theme(
    plot.title = element_text(size = 7, color = "#FFFFFF", margin = margin(t = 5), family = "roboto", face = "bold"),
    plot.subtitle = element_textbox_simple(color = "#FFFFFF", size = 3.5, margin = margin(t = 2, b = 10)),
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#000000", color = "#000000"),
    plot.tag.position = c(0.99, 0.1),
    plot.tag = element_textbox_simple(
      colour = "#FFFFFF",
      family = "oswald",
      hjust = 1,
      halign = 1,
      lineheight = 1.4,
      face = "bold"
    ),
    plot.margin = margin(l = 5)
  )

ggsave("projects/sheriff-substack/output/figures/lasd_compton_category_barchart_horizontal.png", p, width = 10, height = 4.5)  

