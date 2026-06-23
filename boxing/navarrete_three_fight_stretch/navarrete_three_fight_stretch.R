library(tidyverse)
library(ggrepel)
library(showtext)
library(glue)
library(ggtext)

font_add_google("Oswald", family = "oswald")
font_add_google("Roboto Condensed", family = "roboto")

# Add local font
font_add("Font Awesome 6 Brands", here::here("fonts/otfs/Font Awesome 6 Brands-Regular-400.otf"))

# Automatically enable the use of showtext for all plots
showtext_auto()

# Set DPI for high-resolution text rendering
showtext_opts(dpi = 300)

# Generate a social media caption with custom colors and font styling
social <- andresutils::social_caption(font_family = "oswald", font_color = "grey20") 

# Construct the final plot caption with TidyTuesday details, data source, and social captionhttp://127.0.0.1:13477/graphics/plot_zoom_png?width=2392&height=926
cap <- paste0(
  "**Source**: CompuBox | **Graphic**: ", social
)

navarrete_last3_rounds <- tribble(
  ~fight_date, ~opponent, ~result, ~fighter, ~round,
  ~total_landed, ~total_thrown,
  ~jabs_landed, ~jabs_thrown,
  ~power_landed, ~power_thrown,
  
  # Navarrete vs Oscar Valdez — 12/07/24
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Navarrete", 1, 14, 50, 6, 24, 8, 26,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Navarrete", 2, 20, 79, 3, 31, 17, 48,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Navarrete", 3, 13, 47, 4, 20, 9, 27,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Navarrete", 4, 11, 46, 2, 20, 9, 26,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Navarrete", 5, 27, 89, 7, 42, 20, 47,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Navarrete", 6, 20, 69, 5, 27, 15, 42,
  
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Valdez", 1, 12, 40, 4, 23, 8, 17,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Valdez", 2, 16, 46, 3, 20, 13, 26,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Valdez", 3, 15, 34, 2, 15, 13, 19,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Valdez", 4, 11, 42, 3, 13, 8, 29,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Valdez", 5, 13, 39, 0, 9, 13, 30,
  "2024-12-07", "Oscar Valdez", "KO 6 Win", "Valdez", 6, 11, 31, 0, 8, 11, 23,
  
  # Navarrete vs Charly Suarez — 05/10/25
  "2025-05-10", "Charly Suarez", "NC 8", "Navarrete", 1, 14, 62, 5, 30, 9, 32,
  "2025-05-10", "Charly Suarez", "NC 8", "Navarrete", 2, 7, 46, 0, 16, 7, 30,
  "2025-05-10", "Charly Suarez", "NC 8", "Navarrete", 3, 14, 48, 4, 19, 10, 29,
  "2025-05-10", "Charly Suarez", "NC 8", "Navarrete", 4, 23, 59, 2, 10, 21, 49,
  "2025-05-10", "Charly Suarez", "NC 8", "Navarrete", 5, 17, 54, 2, 14, 15, 40,
  "2025-05-10", "Charly Suarez", "NC 8", "Navarrete", 6, 8, 44, 1, 12, 7, 32,
  "2025-05-10", "Charly Suarez", "NC 8", "Navarrete", 7, 15, 53, 2, 13, 13, 40,
  "2025-05-10", "Charly Suarez", "NC 8", "Navarrete", 8, 0, 0, 0, 0, 0, 0,
  
  "2025-05-10", "Charly Suarez", "NC 8", "Suarez", 1, 8, 49, 2, 16, 6, 33,
  "2025-05-10", "Charly Suarez", "NC 8", "Suarez", 2, 12, 65, 2, 22, 10, 43,
  "2025-05-10", "Charly Suarez", "NC 8", "Suarez", 3, 10, 67, 1, 19, 9, 48,
  "2025-05-10", "Charly Suarez", "NC 8", "Suarez", 4, 12, 60, 2, 13, 10, 47,
  "2025-05-10", "Charly Suarez", "NC 8", "Suarez", 5, 10, 59, 0, 18, 10, 41,
  "2025-05-10", "Charly Suarez", "NC 8", "Suarez", 6, 16, 52, 3, 14, 13, 38,
  "2025-05-10", "Charly Suarez", "NC 8", "Suarez", 7, 18, 60, 1, 8, 17, 52,
  "2025-05-10", "Charly Suarez", "NC 8", "Suarez", 8, 0, 0, 0, 0, 0, 0,
  
  # Navarrete vs Eduardo Nunez — 02/28/26
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 1, 6, 23, 2, 10, 4, 13,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 2, 9, 25, 6, 18, 3, 7,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 3, 14, 37, 2, 13, 12, 24,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 4, 15, 54, 7, 28, 8, 26,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 5, 23, 65, 6, 29, 17, 36,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 6, 36, 75, 10, 27, 26, 48,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 7, 27, 75, 4, 23, 23, 52,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 8, 26, 71, 5, 28, 21, 43,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 9, 38, 79, 9, 26, 29, 53,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 10, 42, 90, 3, 11, 39, 79,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Navarrete", 11, 0, 0, 0, 0, 0, 0,
  
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 1, 4, 23, 2, 17, 2, 6,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 2, 7, 28, 3, 8, 4, 20,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 3, 7, 25, 0, 6, 7, 19,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 4, 8, 30, 5, 15, 3, 15,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 5, 19, 48, 3, 9, 16, 39,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 6, 25, 60, 3, 9, 22, 51,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 7, 15, 44, 2, 6, 13, 38,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 8, 27, 69, 2, 5, 25, 64,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 9, 14, 51, 4, 13, 10, 38,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 10, 14, 67, 2, 11, 12, 56,
  "2026-02-28", "Eduardo Nunez", "TKO 11 Win", "Nunez", 11, 0, 0, 0, 0, 0, 0
) %>%
  mutate(
    fight_date = as.Date(fight_date),
    total_pct = if_else(total_thrown > 0, total_landed / total_thrown, NA_real_),
    jabs_pct = if_else(jabs_thrown > 0, jabs_landed / jabs_thrown, NA_real_),
    power_pct = if_else(power_thrown > 0, power_landed / power_thrown, NA_real_),
    fight_label = paste0(format(fight_date, "%b %Y"), "\nvs ", opponent)
  )

navarrete_last3_rounds %>%
  group_by(fight_date, opponent, result, fighter) %>%
  summarise(
    total_landed = sum(total_landed),
    total_thrown = sum(total_thrown),
    total_pct = total_landed / total_thrown,
    jabs_landed = sum(jabs_landed),
    jabs_thrown = sum(jabs_thrown),
    jabs_pct = jabs_landed / jabs_thrown,
    power_landed = sum(power_landed),
    power_thrown = sum(power_thrown),
    power_pct = power_landed / power_thrown,
    .groups = "drop"
  )

pos <- position_jitter(width = 0.15, height = 0, seed = 10)

navarrete_last3_rounds %>%
  filter(fighter == "Navarrete",
         total_landed > 0) %>%
  arrange(fight_date) %>%
  mutate(
    opponent_label = paste0(opponent, "\n", format(fight_date, "%b %Y")),
    opponent_label = factor(opponent_label, levels = unique(opponent_label))
  ) %>%
  ggplot(aes(x = opponent_label, y = total_landed)) +
  geom_point(
    position = pos,
    size = 3.2,
    shape = 21,
    fill = "grey",
    color = "#151922",
    alpha = 0.5
  ) +
  geom_text(
    aes(label = round),
    position = pos,
    family = "oswald",
    fontface = "bold",
    size = 2,
    color = "#151922",
    alpha = 0.5
  ) +
  ggfx::with_outer_glow(
    stat_summary(
      fun = median,
      color = "#4169E1",
      size = 0.9
    ),
    colour = "blue", sigma = 15, expand = 3
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    vjust = -1.2,
    family = "oswald",
    fontface = "bold",
    size = 3,
    color = "#4169E1"
  ) +
  annotate(
    "curve",
    x = 2.75, y = 38.2,
    xend = 3.02, yend = 42,
    curvature = -0.25,
    arrow = arrow(length = unit(0.05, "inches"), type = "closed"),
    color = "#151922",
    linewidth = 0.35
  ) +
  geom_richtext(
    data = tibble(
      x = 2.58,
      y = 38.1,
      label = "<b>Round 10</b> surge<br>was his highest output<br>in this stretch"
    ),
    aes(x = x, y = y, label = label),
    inherit.aes = FALSE,
    family = "roboto",
    size = 2.1,
    color = "grey20",
    fill = NA,
    label.color = NA,
    hjust = 0,
    vjust = 1
  ) +
  annotate(
    "text",
    x = 0.61,
    y = 40,
    label = "Punches landed\nby round",
    family = "roboto",
    fontface = "bold",
    size = 2.4,
    color = "grey20",
    hjust = 0.5,
    vjust = 0.5,
    lineheight = 0.85
  ) +
  labs(
    title = "Vaquero’s Three-Fight Stretch",
    subtitle = "From dropping Valdez three times, to the controversial Suarez no-contest, to stopping Núñez for unified gold, each badge shows one round while the <span style='color:#4169E1; font-weight:bold;'>blue dot marks his median punches landed</span>.",
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  coord_cartesian(clip = "off", ylim = c(NA, 42)) +
  theme_minimal(paper = "#F8F8FF", base_family = "roboto", ink = "grey20") +
  theme(
    plot.title = element_text(family = "oswald", face = "bold", size = 20),
    plot.title.position = "plot",
    plot.subtitle = element_textbox_simple(size = 10, family = "oswald", margin = margin(b = 10)),
    plot.caption = element_textbox_simple(margin = margin(t = 10), size = 6.5, family = "oswald"),
    plot.caption.position = "plot",
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linetype = "dashed", color = "grey90", linewidth = 0.2),
    plot.margin = margin(t = 10, l = 10, b = 5, r = 10)
  )

ggsave("boxing/navarrete_three_fight_stretch/navarrete_three_fight_stretch.png", width = 6, height = 6)












