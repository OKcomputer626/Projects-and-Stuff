# PACKAGES ------------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)
library(ggforce)
library(patchwork)

# DATA ----------------------------------------------------------------
df <- read_csv(
  "projects/ice-substack/output/data/incidents_clean.csv",
  show_col_types = FALSE
)

glimpse(df)

font_add_google("Bodoni Moda", "bodoni")
font_add_google("Open Sans", "os")

# Add local font
font_add("fb", here::here("fonts/otfs/Font Awesome 6 Brands-Regular-400.otf"))

# Automatically enable the use of showtext for all plots
showtext_auto()

# Set DPI for high-resolution text rendering
showtext_opts(dpi = 300)

# FATAL INCIDENTS (POINTS + LABELS) -----------------------------------
fatal_df <- df %>%
  filter(category == "fatal") %>%
  arrange(date) %>%
  mutate(
    stop  = row_number(),
    label = paste0(
      stop, ". ", city, "\n",
      format(date, "%b"), ". ", trimws(format(date, "%e"))
    )
  )

# STATE COUNTS (FOR CHOROPLETH FILL) ----------------------------------
state_counts <- fatal_df %>%
  mutate(region = tolower(state_name)) %>%
  count(region, name = "n")

# MARK-CIRCLE LABEL DATA ----------------------------------------------
fatal_df2 <- fatal_df %>%
  mutate(
    id     = row_number(),
    label2 = paste0(id, ". ", city, "\n", format(date, "%b. %e"))
  )

# MAP (STATE FILL + INCIDENT POINTS) ----------------------------------
map <- ggplot() +
  # states choropleth
  geom_polygon(
    data = us_states,
    aes(x = long, y = lat, group = group, fill = n),
    color = "#F0EFEB",
    linewidth = 0.2
  ) +
  scale_fill_gradient(low = "#D3D3D3", high = "#6c757d") +
  # labeled circles (ggforce)
  geom_mark_circle(
    data = fatal_df2,
    aes(x = longitude, y = latitude, label = label2, group = id),
    expand = unit(2.5, "mm"),
    con.cap = unit(0, "mm"),
    label.fontsize = 7.5,
    label.fill = NA,
    label.colour = "#000000",
    colour = "#A95C68",
    linewidth = 0.4,
    label.family = "os"
  ) +
  # points
  geom_point(
    data = fatal_df,
    aes(x = longitude, y = latitude),
    shape = 21,
    size = 4.2,
    fill = "#000000",
    color = "#F0EFEB"
  ) +
  coord_map("albers", lat0 = 45.5, lat1 = 29.5) +
  theme_void() +
  theme(
    text = element_text(family = "bodoni"),
    plot.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),
    legend.position = "none"
  )

# TIMELINE DATA --------------------------------------------------------
df_timeline <- fatal_df %>%
  arrange(date) %>%
  mutate(
    id   = stop,
    side = if_else(id %% 2 == 0, "bottom", "top"),
    
    # layout positions
    y_stem_end = if_else(side == "top", 1.40, 0.60),
    y_circle   = if_else(side == "top", 1.40, 0.60),
    y_city     = if_else(side == "top", 1.58, 0.42),
    
    # label (city bold, date below)
    city_lbl = paste0(
      "<b>", city_name, "</b><br>",
      format(date, "%b"), ". ", trimws(format(date, "%e"))
    )
  )

# TIMELINE RANGE (WITH PADDING) ----------------------------------------
x_start <- min(df_timeline$date) - as.difftime(10, units = "days")
x_end   <- max(df_timeline$date) + as.difftime(10, units = "days")

# MONTH LABELS ("PILLS") -----------------------------------------------
df_months <- tibble(
  month = seq(
    from = as.Date(format(x_start, "%Y-%m-01")),
    to   = as.Date(format(x_end,   "%Y-%m-01")),
    by   = "month"
  ),
  abbr = format(month, "%b")
)

# TIMELINE PLOT --------------------------------------------------------
timeline <- ggplot() +
  theme_void() +
  theme(
    plot.background  = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),
    panel.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB")
  ) +
  
  # baseline
  geom_segment(
    aes(x = x_start, xend = x_end, y = 1, yend = 1),
    linewidth = 0.7,
    color = "#343434"
  ) +
  
  # stems (baseline -> circles)
  geom_segment(
    data = df_timeline,
    aes(x = date, xend = date, y = 1, yend = y_stem_end),
    linewidth = 0.7,
    color = "#343434"
  ) +
  
  # month pills on baseline
  geom_label(
    data = df_months,
    aes(x = month, y = 1, label = abbr),
    family = "bodoni",
    fontface = "bold",
    size = 3,
    color = "#343434",
    fill = "#F0EFEB",
    label.size = 0.9,
    label.padding = unit(0.35, "lines"),
    label.r = unit(0.35, "lines")
  ) +
  
  # circle outlines
  geom_point(
    data = df_timeline,
    aes(x = date, y = y_circle),
    shape = 21,
    size = 5,
    stroke = 1.1,
    fill = "#F0EFEB",
    color = "#343434"
  ) +
  
  # id numbers inside circles
  geom_text(
    data = df_timeline,
    aes(x = date, y = y_circle, label = id),
    family = "os",
    fontface = "bold",
    size = 2.1,
    color = "#343434"
  ) +
  
  # city labels (rich text)
  geom_richtext(
    data = df_timeline,
    aes(x = date, y = y_city, label = city_lbl),
    family = "os",
    size = 2.5,
    lineheight = 1.2,
    color = "#343434",
    fill = NA,
    label.color = NA
  ) +
  
  coord_cartesian(
    xlim   = c(x_start - 10, x_end + 7),
    ylim   = c(0.4, 1.60),
    expand = FALSE,
    clip   = "off"
  )

# TITLE / SUBTITLE / CAPTION ------------------------------------------
title <- paste0(
  "<span style='font-family:bodoni;font-weight:700;color:#343434;font-size:34pt;'>FATAL SHOOTINGS</span><br>",
  "<span style='font-family:bodoni;font-weight:700;color:#343434;font-size:28pt;'>By Federal Immigration Agents</span><br>"
)

subtitle <- paste0(
  "<span style='font-family:os;color:#495057;font-size:10pt;'>Aggregated dataset of shootings involving ICE and CBP agents since June 2025. Points are numbered chronologically.</span>"
)

caption <- paste0(
  "<span style='font-family:fb;color:#343434;'>&#xf16d;</span>",
  "<span style='font-family:os;color:#343434;'> gonzalez_afc </span>",
  "<span style='font-family:os;color:#343434;'> • Sources: Gun Violence Archive + local news reports • Updated Jan. 29, 2026</span>"
)

# COMBINE PLOTS --------------------------------------------------------
p <- (map / timeline) +
  plot_layout(heights = c(3.2, 1)) +
  plot_annotation(
    title = title,
    subtitle = subtitle,
    caption = caption,
    theme = theme(
      plot.title    = element_textbox_simple(halign = 0.5),
      plot.subtitle = element_textbox_simple(halign = 0.5, margin = margin(t = -8)),
      plot.caption  = element_textbox_simple(halign = 0.98, size = 6.5, margin = margin(t = 20)),
      plot.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),
      plot.margin     = margin(t = 20, b = 10)
    )
  )

# EXPORT ----------------------------------------------------------------
ggsave(
  "projects/ice-substack/output/figures/map_ice_fatal_shootings.png",
  p,
  height = 8, width = 8, units = "in", dpi = 300
)