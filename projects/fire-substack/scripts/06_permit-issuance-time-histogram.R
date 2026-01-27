# Packages ----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(showtext)
library(glue)
library(ggtext)
library(ggh4x)

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
  "**Source**: Los Angeles County EPIC-LA (Disaster Recovery Permits) | ", "**Graphic**: ", social
)

# Read Clean Data ---------------------------------------------------------
clean_path <- "projects/fire-substack/output/data/bh_parcels_dins_2025_clean.csv"
df <- read_csv(clean_path)

plot4_df <- df %>%
  drop_na(fire_name) %>%
  mutate(
    total = roll_land_value + roll_imp_value
  ) %>%
  filter(total > 0) %>%
  group_by(fire_name) %>%
  summarise(
    land_share = sum(roll_land_value, na.rm = TRUE) / sum(total, na.rm = TRUE),
    imp_share  = sum(roll_imp_value,  na.rm = TRUE) / sum(total, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(land_share, imp_share),
    names_to = "component",
    values_to = "share"
  ) %>%
  mutate(component = recode(component,
                            land_share = "Land value",
                            imp_share  = "Improvement value"))

p <- plot4_df %>%
  ggplot(aes(x = fire_name, y = share, fill = component)) +
  geom_col(position = "fill") +
  geom_text(
    aes(label = scales::percent(share, accuracy = 1)),
    position = position_fill(vjust = 0.5),
    size = 2.8,
    color = "#F0EFEB",
    family = "roboto",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = rev(c("#008080", "#FF7F50"))
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  coord_flip(clip = "off") +
  labs(
    title = "Where the Exposure Lives: Land vs Improvement Value",
    subtitle = "Share of assessed value by fire area",
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 7, base_family = "roboto") +
  theme(
    plot.title = element_text(family = "oswald", face = "bold", size = 11),
    plot.subtitle = element_text(size = 5.5),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(hjust = 0, size = 4, margin = margin(t = 5)),
    plot.caption.position = "plot",
    legend.title = element_blank(),
    legend.position = "top",
    legend.text = element_text(margin = margin(l = 1, unit = "pt")),
    legend.key.spacing.x = unit(0.25, "cm"),
    legend.key.size = unit(0.25, "cm"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.margin = margin(5, 10, 5, 10),
    plot.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),
    panel.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB")
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave(
  "projects/fire-substack/output/figures/land_vs_improvement_share.png",
  p,
  width = 5,
  height = 2
)
