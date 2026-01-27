# Packages ----------------------------------------------------------------
library(tidyverse)
library(janitor)
library(showtext)
library(glue)
library(ggtext)
library(ggarrow)

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
social <- andresutils::social_caption(font_family = "roboto", icon_color = "#00BFFF") 

# Construct the final plot caption with TidyTuesday details, data source, and social caption
cap <- paste0(
  "**Source**:  Los Angeles County EPIC-LA (Disaster Recovery Permits) | **Graphic**: ", social
)

# Read Clean Data ---------------------------------------------------------
clean_path <- "projects/fire-substack/output/data/epicla_eaton_palisades_clean.csv"
df <- read_csv(clean_path)

df_clean <- df %>%
  mutate(
    status = str_squish(status),
    
    # 1) standardize status names (fix duplicates / near-duplicates)
    status_clean = case_when(
      status %in% c("Hold", "On Hold") ~ "On Hold",
      status %in% c("Review", "In Review") ~ "In Review",
      status %in% c("New", "New - Online") ~ "New",
      
      TRUE ~ status
    ),
    
    # 2) collapse into clean buckets
    status_bucket = case_when(
      status_clean %in% c(
        "Open", "New", "In Review",
        "Approved", "Approved Ready for Permit",
        "Approved Pending Clearances",
        "Zoning Cleared"
      ) ~ "Active / In Progress",
      
      status_clean %in% c(
        "Waiting for Applicant",
        "On Hold"
      ) ~ "Waiting / Stuck",
      
      status_clean %in% c(
        "Issued", "Finaled", "Completed"
      ) ~ "Done / Closed (Successful)",
      
      status_clean %in% c(
        "Withdrawn"
      ) ~ "Closed (Not moving forward)",
      
      TRUE ~ "Other"
    )
  )

# 1) count per bucket (this is the dataset your bar chart needs)
df_bucket_counts <- df_clean %>%
  count(status_bucket, name = "value")

p <- df_bucket_counts %>%
  ggplot(aes(x = reorder(status_bucket, value), y = value)) +
  geom_bar(stat = "identity", fill = "#00BFFF", width = 0.65) +
  geom_richtext(
    aes(label = scales::comma(value)),
    vjust = -0.2,
    family = "roboto",
    fontface = "bold",
    fill = "#191970",
    text.colour = "#F0EFEB",
    size = 2.1,
    label.colour = NA,
    label.r = unit(0, "pt"),
    label.padding = unit(2, "pt")
  ) +
  geom_arrow_segment(
    data = tibble(x = 1.5, y = 1200, xend = 1.02, yend = 250),
    aes(x = x, y = y, xend = xend, yend = yend),
    inherit.aes = FALSE,
    linewidth = 0.3,
    colour = "grey30"
  ) +
  geom_richtext(
    data = tibble(
      x = 1.5,
      y = 1200,
      label = "Almost none are being<br><b>abandoned or denied</b>"
    ),
    aes(x = x, y = y, label = label),
    size = 1.7,
    color = "grey30",
    vjust = 0,
    hjust = 0.4,
    family = "roboto",
    fill = NA,
    label.color = NA
  ) +
  scale_y_continuous(
    limits = c(0, 3500),
    breaks = seq(0, 3500, by = 500),
    labels = scales::comma,
    expand = c(0,0)
  ) +
  labs(
    title = "Most Rebuild Permits Are Still Active",
    subtitle = "Over <span style='color:#00BFFF;'><b>3,300</b></span> cases remain “In Progress,” while about 1,400 have already been successfully closed",
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 7, base_family = "roboto") +
  theme(
    plot.title = element_text(
      size = 12,
      family = "oswald",
      face = "bold"
    ),
    plot.title.position = "plot",
    plot.subtitle = element_markdown(
      size = 6.5,
      margin = margin(b = 12)
    ),
    plot.caption = element_textbox_simple(
      size = 4,
      hjust = 0,
      margin = margin(t = 7)
    ),
    plot.caption.position = "plot",
    plot.margin = margin(5, 5, 5, 5),
    plot.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),
    panel.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )


ggsave(
  "projects/fire-substack/output/figures/permit_status_bucket_counts.png",
  p,
  width = 5,
  height = 4
)


