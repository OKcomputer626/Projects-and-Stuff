# Packages ----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(showtext)
library(glue)
library(ggtext)
library(ggh4x)
library(waffle)

# Add Google fonts
font_add_google("Oswald", family = "oswald")
font_add_google("Roboto Condensed", family = "roboto")

# Add local font
font_add("Font Awesome 6", here::here("fonts/otfs/Font Awesome 6 Free-Solid-900.otf"))
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

df_residential <- df %>%
  filter(use_type == "Residential",
         fire_name == "Palisades") %>%
  drop_na(year_built1) %>%
  filter(year_built1 >= 1885) %>%
  mutate(
    damage = str_squish(damage),
    decade = case_when(
      year_built1 < 1950 ~ "Before 1950",
      year_built1 >= 2010 ~ "2010s–2020s",
      TRUE ~ glue("{floor(as.numeric(year_built1) / 10) * 10}s")
    ),
    decade = factor(decade, levels = c(
      "Before 1950","1950s","1960s","1970s","1980s","1990s","2000s","2010s–2020s"
    )),
    damage = factor(damage, levels = c(
      "No Damage","Affected (1-9%)","Minor (10-25%)","Major (26-50%)","Destroyed (>50%)","No Data/Vacant"
    ))
  )

# Summarize data: group by damage and decade, then count occurrences
damage <- df_residential %>%
  count(damage, decade, name = "value") %>%
  group_by(decade) %>%
  mutate(pct = value / sum(value)) %>%
  ungroup()

# % Destroyed (>50%)
max_damage_freq <- df_residential %>%
  summarise(pct_destroyed = mean(damage == "Destroyed (>50%)", na.rm = TRUE) * 100) %>%
  pull(pct_destroyed)

# % built before 1960
before_1960 <- df_residential %>%
  summarise(pct_before_1960 = mean(year_built1 < 1960, na.rm = TRUE) * 100) %>%
  pull(pct_before_1960)


social <- andresutils::social_caption(
  bg_color = "#F0F8FF",
  font_family = "roboto",
  linkedin = "Andres Gonzalez"
)

title <- toupper("Damage Assessment from the Palisades Fire")

inspected_n <- df_residential %>%
  summarise(n = n()) %>%
  pull(n)

st <- paste0(
  "One of the wealthiest communities in the U.S. experienced one of the most destructive fires in its history. ",
  "The Palisades Fire, which began at 10:30 a.m. on January 7, 2026, in Los Angeles County, caused widespread destruction. ",
  "As of January 26, 2026, out of ", scales::comma(inspected_n), " inspected residential properties, ",
  "<span style='color:#118ab2;'>**Destroyed (>50%)**</span> accounted for **",
  round(max_damage_freq, 1),
  "%** of all damage assessments. ",
  "A significant portion of the inspected homes (**",
  round(before_1960, 1),
  "%**) were built before 1960, with a large share constructed during the 1950s. ",
  "These older homes—now generally more than 65 years old, and in some cases over a century old—span multiple construction eras and remain an integral part of the area’s character. ",
  "Other damage levels included <span style='color:#ef476f;'>**No Damage**</span>, ",
  "<span style='color:#f78c6b;'>**Affected (1-9%)**</span>, ",
  "<span style='color:#ffd166;'>**Minor (10-25%)**</span>, ",
  "<span style='color:#06d6a0;'>**Major (26-50%)**</span>, and ",
  "<span style='color:#073b4c;'>**No Data/Vacant**</span>. ",
  "This tragedy highlights the critical need to retrofit older homes with modern fire-resistant materials to mitigate future disasters."
)

cap <- paste0(
  st,
  "<br><br>**Note**: Each square represents approximately 1% of inspected homes within each decade.<br>**Data**: CAL FIRE Damage Inspection (DINS)<br>**Graphic**: ", social
)


p <- damage %>%
  ggplot() +
  geom_pictogram(
    aes(label = damage, color = damage, values = pct),
    flip = TRUE, n_rows = 5, size = 2.4,
    make_proportional = TRUE,
    family = "Font Awesome 6"
  ) +
  scale_label_pictogram(
    name = NULL,  # No legend title
    values = rep("home", 6)  # Use "home" icon for all damage levels
  ) +
  facet_wrap(~decade, nrow = 1, strip.position = "bottom") +
  scale_x_discrete(expand = expansion(mult = 0.05)) +
  scale_y_continuous(
    breaks = c(0, 5, 10, 15, 20),
    labels = scales::percent(c(0, .25, .50, .75, 1)),
    expand = c(0, 0),
    minor_breaks = NULL
  ) +
  scale_color_manual(
    values = c("#ef476f", "#f78c6b", "#ffd166", "#06d6a0", "#118ab2", "#073b4c")) +
  coord_fixed() + 
  labs(
    title = title,
    subtitle = cap
  ) +
  theme_minimal(         # Minimalistic theme for cleaner visuals
    base_family = "roboto",  # Font family for text
    base_size = 7           # Base font size
  ) +
  theme(
    legend.position = "none",  # Remove legend
    plot.title.position = "plot",
    plot.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),  # Light blue background
    panel.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),
    panel.grid.major = element_line(
      linewidth = 0.3,
      color = "grey90"  # Light grid lines
    ),
    plot.title = element_textbox_simple(
      margin = margin(b = 5, t = 5),
      family = "oswald",  # Font for title
      face = "bold",
      size = 12
    ),
    plot.subtitle = element_textbox_simple(
      margin = margin(b = 15, t = 5),
      lineheight = 1.2,  # Adjust subtitle spacing
      size = 6
    ),
    plot.margin = margin(5, 10, 5, 10)
  )

ggsave("projects/fire-substack/output/figures/damage_pictogram.png", plot = p, width = 6, height = 4.2)
