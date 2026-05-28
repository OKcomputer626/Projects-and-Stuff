library(tidyverse)
library(showtext)
library(glue)
library(ggtext)
library(scales)
library(sf)

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
social <- andresutils::social_caption(font_family = "roboto", bg_color = "grey90") 

# Construct the final plot caption with TidyTuesday details, data source, and social captionhttp://127.0.0.1:13477/graphics/plot_zoom_png?width=2392&height=926
cap <- paste0(
  "**Source**: Beverly Hills Police Department (BH PD) crime data, 2025–Feb 3, 2026 | **Graphic**: ", social
)

path <- "projects/Beverly Hills Crime/data"

crimes_df <- read_csv(paste0(path, "/crime_with_tract.csv"))

bh_tract <- st_read(paste0(path, "/beverly_hills_tracts_3310.gpkg"))

bh_tract <- bh_tract %>%
  mutate(GEOID = as.character(GEOID))

crimes_month <- crimes_df %>%
  filter(year == 2025,
         !is.na(GEOID),
         !GEOID == "06037214903") %>%
  count(month, GEOID, name = "total")

bh_tract_month <- bh_tract %>%
  left_join(crimes_month, by = "GEOID") %>%
  mutate(total = coalesce(total, 0L),
         month = factor(month, levels = month.abb, labels = month.name, ordered = TRUE)) %>%
  filter(!is.na(month))

bh_tract_month <- st_transform(bh_tract_month, 4326) %>%
  st_as_sf(sf_column_name = "geom")


p <- bh_tract_month %>%
  ggplot() +
  geom_sf(aes(fill = total), color = "#FFFFFF") +
  geom_richtext(
    data = function(d) {
      bb <- st_bbox(d)
      x_mid <- (bb["xmin"] + bb["xmax"]) / 2
      y_below <- bb["ymin"] - 0.01 * (bb["ymax"] - bb["ymin"])

      d %>%
        group_by(month) %>%
        summarise(
          avg_total = mean(total, na.rm = TRUE),
          x = x_mid,
          y = y_below,
          .groups = "drop"
        ) %>%
        mutate(
          label = paste0("Avg: <b>", sprintf("%.1f", avg_total), "</b><br>")
        )
    },
    aes(x = x, y = y, label = label),
    fill = NA,
    label.color = NA,
    hjust = 0.35,
    vjust = 0.4,
    size = 2.5,
    family = "roboto"
  ) +
  paletteer::scale_fill_paletteer_c(
    palette = "pals::kovesi.linear_blue_95_50_c20"
  ) +
  facet_wrap(vars(month), labeller = as_labeller(toupper)) +
  labs(
    title = "Beverly Hills Neighborhood Totals (2025)",
    subtitle = "Each panel represents one month in 2025.
Each shaded area is a Beverly Hills neighborhood.
Neighborhoods are coloured by the total number of observations recorded in that month. Darker shading indicates higher totals.",
    caption = cap,
    fill = "Total"
  ) +
  theme_void(base_family = "roboto") +
  theme(
    plot.background = element_rect(color = "grey90",
                                   fill = "grey90"),
    plot.margin = margin(5, 5, 5, 5),
    plot.title = element_text(family = "oswald", size = 14, hjust = 0.5, face = "bold"),
    plot.subtitle = element_textbox(
      size = 8.5, width = 1, hjust = 0.5, halign = 0.5, lineheight = 1,
      margin = margin(t = 5, b = 12)
    ),
    plot.caption = element_textbox_simple(
      halign = 0.5, width = 1.0, size = 5.5
    ),
    strip.text = element_text(
      family = "roboto",
      size = 8,
      color = "grey25"
    ),
    legend.title = element_text(size = 8), # Changes the legend title font size
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.55, "cm")
  )


ggsave("projects/Beverly Hills Crime/output/figures/crime_monthly_2025_plot.png",
       plot = p, width = 8, height = 8)    

