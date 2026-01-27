# Packages ----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(showtext)
library(glue)
library(ggtext)

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
  "**Source**:  Los Angeles County EPIC-LA (Disaster Recovery Permits) | **Graphic**: ", social
)

# Read Clean Data ---------------------------------------------------------
clean_path <- "projects/fire-substack/output/data/epicla_eaton_palisades_clean.csv"
df <- read_csv(clean_path)

plot3_df <- df %>%
  select(disaster_type, status, apply_date, issuance_date) %>%
  drop_na(disaster_type) %>%
  filter(status == "Issued") %>%
  mutate(
    apply_date    = as.Date(mdy_hms(apply_date)),
    issuance_date = as.Date(mdy_hms(issuance_date)),
    days_to_issue = as.numeric(issuance_date - apply_date)
  ) %>%
  filter(!is.na(days_to_issue), days_to_issue >= 0)

p <- plot3_df %>%
  ggplot(aes(x = days_to_issue, fill = disaster_type, color = disaster_type)) +
  geom_histogram(bins = 30, alpha = 0.7) +
  scale_color_manual(
    values = c("#002642", "#840032")
  ) +
  scale_fill_manual(
    values = c("#002642", "#840032")
  ) +
  facet_wrap(~ disaster_type, scales = "free_y",
             labeller = labeller(
               disaster_type = c(
                 "Eaton Fire (01-2025)" = "<span style = 'color: #002642;'>Eaton Fire</span>",
                 "Palisades Fire (01-2025)" = "<span style = 'color: #840032;'>Palisades Fire</span>"
               )
             )) +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(0, 300),
                         expand = c(0, 0)),
      scale_y_continuous(limits = c(0, 8),
                         expand = c(0, 0))
    )
  ) +
  labs(
    title = "How Long Do Rebuild Permits Take to Get Issued?",
    subtitle = "Distribution of days from application to issuance (Issued cases only)",
    caption = cap,
    x = "Days to Issuance",
    y = NULL
  ) +
  theme_minimal(base_size = 7, base_family = "roboto") +
  theme(
    plot.title = element_markdown(
      size = 12,
      face = "bold",
      family = "oswald"
    ),
    plot.title.position = "plot",
    plot.subtitle = element_text(
      size = 7,
      margin = margin(b = 7)
    ),
    plot.caption = element_textbox_simple(
      size = 4,
      hjust = 0,
      margin = margin(t = 5)
    ),
    plot.caption.position = "plot",
    plot.margin = margin(5, 5, 5, 5),
    legend.position = "none",
    strip.text = element_markdown(
      face = "bold",
      family = "roboto",
      size = 10
    ),
    axis.line.x = element_line(
      color = "#000000",
      linewidth = 0.5
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave(
  "projects/fire-substack/output/figures/permit-issuance-time-histogram.png",
  p,
  width = 7,
  height = 5
)
