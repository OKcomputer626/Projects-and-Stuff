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
  "**Source**: Los Angeles County EPIC-LA (Disaster Recovery Permits) | ",
  "**N**: 5,198 cases | ",
  "**Graphic**: ", social
)

# Read Clean Data ---------------------------------------------------------
clean_path <- "projects/fire-substack/output/data/epicla_eaton_palisades_clean.csv"
df <- read_csv(clean_path)

plot2_monthly_df <- df %>%
  select(disaster_type, apply_date) %>%
  drop_na(disaster_type) %>%
  mutate(
    apply_date = as.Date(mdy_hms(apply_date)),
    app_month  = floor_date(apply_date, unit = "month"),
    disaster_type = case_when(
      disaster_type == "Palisades Fire (01-2025)" ~ "Palisades Fire",
      disaster_type == "Eaton Fire (01-2025)"     ~ "Eaton Fire",
      TRUE ~ disaster_type
    )
  ) %>%
  filter(app_month >= as.Date("2025-01-01")) %>%
  count(disaster_type, app_month, name = "value") %>%
  arrange(disaster_type, app_month) %>%
  mutate(
    month_label = ifelse(
      month(app_month) == 1,
      paste0("Jan\n", year(app_month)),
      format(app_month, "%b")
    ),
    month_label = factor(month_label, levels = unique(month_label))
  )

title_txt <- glue(
  "Monthly Rebuilding Applications After the ",
  "<span style='color: #002642;'>Eaton Fire</span>",
  " and ",
  "<span style='color: #840032;'>Palisades Fire</span>"
)

p <- plot2_monthly_df %>%
  ggplot(aes(x = month_label, y = value, fill = disaster_type, color = disaster_type)) +
  geom_col(width = 1, alpha = 0.7) +
  scale_color_manual(
    values = c("#002642", "#840032")
  ) +
  scale_fill_manual(
    values = c("#002642", "#840032")
  ) +
  labs(
    title = title_txt,
    subtitle = "Residential plan/permit cases in unincorporated LA County tied to rebuilding or temporary housing",
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  facet_wrap(~ disaster_type, scales = "free_y",
             labeller = labeller(
               disaster_type = c(
                 "Eaton Fire" = "<span style = 'color: #002642;'>Eaton Fire</span>",
                 "Palisades Fire" = "<span style = 'color: #840032;'>Palisades Fire</span>"
               )
             )) +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(limits = c(0, 800),
                         expand = c(0, 0)),
      scale_y_continuous(limits = c(0, 40),
                         expand = c(0, 0))
    )
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
  "projects/fire-substack/output/figures/disaster_apps_by_month.png",
  p,
  width = 7,
  height = 5
)



