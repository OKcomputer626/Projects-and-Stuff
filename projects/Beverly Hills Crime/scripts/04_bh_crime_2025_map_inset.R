library(tidyverse)
library(showtext)
library(glue)
library(ggtext)
library(scales)
library(sf)
library(ggpattern)
library(patchwork)

# Add Google fonts
font_add_google("Oswald", family = "oswald")
font_add_google("Roboto Condensed", family = "roboto")

# Add local font
font_add("Font Awesome 6 Brands", here::here("fonts/otfs/Font Awesome 6 Brands-Regular-400.otf"))

# Automatically enable the use of showtext for all plots
showtext_auto()

# Set DPI for high-resolution text rendering
showtext_opts(dpi = 600)

# Generate a social media caption with custom colors and font styling
social <- andresutils::social_caption(font_family = "roboto") 

# Construct the final plot caption with TidyTuesday details, data source, and social captionhttp://127.0.0.1:13477/graphics/plot_zoom_png?width=2392&height=926
cap <- paste0(
  "**Source**: Beverly Hills Police Department (BH PD) crime data, 2025–Feb 3, 2026 | **Graphic**: ", social
)

path <- "projects/Beverly Hills Crime/data"
  
crimes_df <- read_csv(paste0(path, "/crime_with_tract.csv"))

bh_tract <- st_read(paste0(path, "/beverly_hills_tracts_3310.gpkg"))

bh_tract <- bh_tract %>%
  mutate(GEOID = as.character(GEOID))

crimes_2025 <- crimes_df %>%
  filter(year == 2025,
         !is.na(GEOID)) %>%
  count(GEOID, name = "total") %>%
  arrange(desc(total))

bh_tract_2025 <- bh_tract %>%
  left_join(crimes_2025, by = "GEOID") %>%
  mutate(total = coalesce(total, 0L))

labels_lu <- tribble(
    ~GEOID,         ~area,
    "06037700801",  "Post Office / City Center",
    "06037701000",  "Southwest",
    "06037700901",  "Southeast",
    "06037700902",  "South",
    "06037700600",  "Trousdale Estates",
    "06037700802",  "East",
    "06037700700",  "Flats",
    "06037214903",  "Beverly Grove West"
  )

id <- "06037214903"

# 1) Add labels ONCE (make a new object so you don't re-join repeatedly)
bh_tract_labeled <- bh_tract_2025 %>%
  mutate(GEOID = as.character(GEOID)) %>%   # safe join key
  left_join(labels_lu, by = "GEOID")        # creates/overwrites `area` from labels_lu

bh_label_pts <- bh_tract_labeled %>%
  select(GEOID, area, geom) %>%
  mutate(geom = st_point_on_surface(geom)) %>%
  st_as_sf(sf_column_name = "geom") %>%
  st_transform(4326)

bh_tract_ll <- st_transform(bh_tract_labeled, 4326) %>%
  st_as_sf(sf_column_name = "geom")

# 4) Plot
p <- ggplot() +
  geom_sf(
    data = filter(bh_tract_ll, GEOID != id),
    aes(fill = total),
    color = "#FFFFFF"
  ) +
  geom_sf_pattern(
    data = filter(bh_tract_ll, GEOID == id),
    aes(fill = total),
    pattern = "stripe",
    pattern_fill = "grey70",
    pattern_colour = "grey30",
    pattern_density = 0.18,
    pattern_spacing = 0.017,
    pattern_size = 0.08,
    color = "#FFFFFF"
  ) +
  geom_sf_label(
    data = filter(bh_label_pts, area != "Post Office / City Center"),
    aes(label = area),
    size = 2.7,
    fill = "#0e304a",
    family = "roboto",
    label.size = 0,
    fontface = "bold",
    alpha = 0.8,
    color = "#FFFFFF"
  ) +
  geom_sf_label(
    data = filter(bh_label_pts, area == "Post Office / City Center"),
    aes(label = area),
    size = 2.7,
    fill = "#0e304a",
    family = "roboto",
    label.size = 0,
    fontface = "bold",
    alpha = 0.8,
    color = "#FFFFFF",
    nudge_x = -0.0015,
    nudge_y = -0.004
  ) +
  coord_sf(
           expand = FALSE) +
  paletteer::scale_fill_paletteer_c(
    palette = "pals::kovesi.linear_blue_95_50_c20"
  ) +
  geom_textbox(
    data = crimes_2025 %>% slice_max(total, n = 1),
    aes(
      x = -118.46, y = 34.092,
      label = paste0(
        "With **", total, "** reported incidents, **",
        labels_lu$area[match(GEOID, labels_lu$GEOID)],
        "** has the highest total in 2025.<br><br>",
        "The striped area (**Beverly Grove West**) is shown for context because it sits **outside** the Beverly Hills city boundary."
      )
    ),
    hjust = 0, box.colour = NA, fill = NA, size = 4.2,
    color = "#0e304a",
    family = "roboto"
  ) +
  guides(fill = guide_colorbar(title.position = "top", 
                               title = "Incidents")) +
  labs(
    title = "Beverly Hills Neighborhood Crime (2025)",
    caption = cap
  ) +
  cowplot::theme_map() +
  theme(
    plot.background = element_rect(color = NA, fill = "#FFFFFF"),
    text = element_text(family = "roboto", color = "#0e304a"),
    plot.title = element_text(size = 20, margin = margin(b = 12), family = "oswald", face = "bold", color = "grey15"),
    plot.title.position = "plot",
    plot.subtitle = element_blank(),
    plot.caption = element_textbox_simple(
      size = 7,
      margin = margin(t = 12, b = 2)
    ),
    panel.background = element_rect(color = NA),
    legend.title = element_markdown(size = 10),
    legend.text = element_text(size = 8, color = "#0e304a"),
    legend.position = c(0.01, 0.92),
    legend.direction = "horizontal",
    legend.key.width = unit(10, "mm"),
    legend.key.height = unit(3, "mm")
  )


ggsave("projects/Beverly Hills Crime/output/figures/crime_map_2025.png",
  plot = p, width = 8, height = 7.4, dpi = 600
)

# Bar plot of average neighborhood crime -------

total_area <- bh_tract_ll %>%
  st_drop_geometry() %>%
  select(total, area) %>%
  filter(!area == "Beverly Grove West") %>%
  mutate(prop = total / sum(total),
         area = fct_reorder(area, -prop)) 

total_area2 <- total_area %>%
  mutate(
    is_max = prop == max(prop, na.rm = TRUE),
    lab = paste0(round(100 * prop, 1), "%")
  )

p2 <- total_area2 %>%
  ggplot(aes(x = prop, y = area, fill = is_max)) +
  geom_col(show.legend = FALSE) +
  geom_text(
    aes(
      label = lab,
      color = prop > 0.05,
      hjust = if_else(prop > .05, 1.2, -0.15)),   # inside vs outside
    size = 2,
    fontface = "bold",
    family = "roboto"
  ) +
  facet_wrap(~ area, ncol = 1, scales = "free_y") +
  scale_fill_manual(values = c(`TRUE` = "#0e304a", `FALSE` = "grey50")) +
  scale_color_manual(values = rev(c("#000000", "#FFFFFF")), guide = "none") +
  labs(
    tag = toupper("Share<br>of<br>Incidents")
  ) +
  scale_x_continuous(
    guide = "none", name = NULL, expand = c(0,0)) +
  scale_y_discrete(guide = "none", expand = expansion(add = c(0.8, 0.6))) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_family = "roboto") +
  theme(
    strip.text = element_text(
      hjust = 0, margin = margin(0, 0, 0, 0),
      size = rel(0.7), face = "bold",
      family = "roboto"
    ),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(color = "grey80", linewidth = .4),
    axis.ticks.x = element_line(color = "grey80", linewidth = .4),
    axis.title.y = element_blank(),
    plot.tag = element_textbox_simple(size = 14,
                            family = "oswald",
                            face = "bold",
                            color = "#0e304a",
                            halign = 0.5),
    plot.tag.position = c(0.72, 0.5)
  )

      
p_inset <- p +
  inset_element(p2, 0, 0.0, 0.4, 0.47)

ggsave("projects/Beverly Hills Crime/output/figures/crime_map_2025_bar_inset.png",
  plot = p_inset, width = 8, height = 7.4, dpi = 600)      
      
      

