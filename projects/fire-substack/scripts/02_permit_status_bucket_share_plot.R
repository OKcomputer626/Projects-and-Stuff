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
  count(disaster_type, status_bucket, name = "value")

bucket_levels <- c(
  "Active / In Progress",
  "Waiting / Stuck",
  "Done / Closed (Successful)",
  "Closed (Not moving forward)"
)

# 2) prep plot data + % inside each disaster
df_bucket_plot <- df_bucket_counts %>%
  mutate(
    status_bucket = factor(status_bucket, levels = bucket_levels),
    disaster_type = case_when(
      disaster_type == "Palisades Fire (01-2025)" ~ "Palisades Fire",
      disaster_type == "Eaton Fire (01-2025)"     ~ "Eaton Fire",
      TRUE ~ disaster_type
    )
  ) %>%
  group_by(disaster_type) %>%
  mutate(pct = value / sum(value),
         label = scales::percent(pct, accuracy = 1)) %>%
  ungroup()

p <- df_bucket_plot %>%
  ggplot(aes(x = disaster_type, y = value, fill = status_bucket)) +
  geom_bar(position="fill", stat="identity") +
  geom_text(
    aes(label = if_else(status_bucket %in% c("Active / In Progress", "Done / Closed (Successful)"), scales::percent(pct, accuracy = 1), NA_character_)),
    position = position_fill(vjust = 0.5),
    size = 2.8,
    color = "#F0EFEB",
    fontface = "bold",
    family = "roboto",
    show.legend = FALSE,
    na.rm = TRUE
  ) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, by = 0.2),
    expand = c(0, 0)
  ) +
  scale_fill_manual(values = c("#003049", "#d62828", "#f77f00", "#fcbf49")) +
  coord_flip(clip = "off") +
  labs(
    title = "Most Fire-Rebuild Permits Are Still in Progress",
    subtitle = "Eaton and Palisades cases are dominated by “Active/In Progress,” with smaller shares waiting on applicants or already finalized",
    x = NULL,
    y = NULL,
    caption = cap
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
  "projects/fire-substack/output/figures/permit_status_bucket_share_by_fire.png",
  p,
  width = 5,
  height = 2
)









