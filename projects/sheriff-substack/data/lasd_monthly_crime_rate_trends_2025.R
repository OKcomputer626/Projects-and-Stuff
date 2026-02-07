library(tidyverse)
library(janitor)
library(here)
library(showtext)
library(glue)
library(gghighlight)
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
social <- andresutils::social_caption(font_family = "roboto", bg_color = "#F0EFEB") 

# Construct the final plot caption with TidyTuesday details, data source, and social caption
cap <- paste0(
  "**Source**: Los Angeles County Sheriff’s Department (LASD) Open Data | ",
  "**Graphic**: ", social
)

# File Paths
crimes_path <- here("projects/sheriff-substack/data", "PART_I_AND_II_CRIMES-YTD.csv")

# Import + clean names
crimes_raw <- read_csv(crimes_path, show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    # Parse incident datetime (DST-safe)
    incident_date = force_tz(
      parse_date_time(
        incident_date,
        orders = "mdy HMS p",
        tz = "UTC"
      ),
      tzone = "America/Los_Angeles"
    ),
    
    # Parse reported date
    incident_reported_date = mdy(incident_reported_date),
    
    # Optional but useful derived fields
    incident_day  = as_date(incident_date),
    incident_hour = hour(incident_date),
    incident_month = month(incident_day, label = TRUE, abbr = TRUE),
    incident_year = year(incident_day),
    city = str_to_title(city),
    incident_id = unique(incident_id)
  )

glimpse(crimes_raw)

units_names_9 <- crimes_raw %>%
  filter(incident_year == 2025) %>%
  count(unit_name, name = "total_crimes") %>%
  slice_max(total_crimes, n = 9) %>%
  pull(unit_name)

crimes_by_unit <- crimes_raw %>%
  filter(unit_name %in% units_names_9,
         incident_year == 2025) %>%
  count(unit_name, incident_month, name = "total_crimes") %>%
  mutate(unit_name = str_to_title(unit_name))


# Example population per station (replace ... with actual numbers)
unit_population <- tibble(
  unit_name = c("LAKEWOOD", "CENTURY", "LANCASTER", "EAST LOS ANGELES",
                "PALMDALE", "SANTA CLARITA VALLEY", "COMPTON", "TEMPLE", "NORWALK"),
  incorporated = c(240962, 66589, 174050, 59139, 167451, 233169, 94182, 130620, 149908),
  unincorporated = c(1997, 117226, 31680, 117319, 42406, 57031, 20934, 56805, 66833)
) %>%
  mutate(total_population = incorporated + unincorporated,
         unit_name = str_to_title(unit_name))


# Merge with population and calculate crime rate per 10k residents
crime_rates <- crimes_by_unit %>%
  left_join(unit_population, by = "unit_name") %>%
  mutate(crime_rate_per_10k = total_crimes / total_population * 10000) %>%
  arrange(desc(crime_rate_per_10k))

p <- crime_rates %>%
  ggplot(aes(x = incident_month, y = crime_rate_per_10k, color = unit_name, group = unit_name)) +
  geom_line(linewidth = 1) +
  geom_point(data = crime_rates %>%
               group_by(unit_name) %>%
               slice_max(incident_month)) +
  gghighlight(use_direct_label = FALSE,
              unhighlighted_params = list(linewidth = 0.4, colour = alpha("grey85", 1))) +
  facet_wrap(vars(unit_name)) +
  geom_text(data = crime_rates %>%
              group_by(unit_name) %>%
              slice_max(incident_month),
            aes(label = round(crime_rate_per_10k)), vjust = 0.5, hjust = -0.5, family = "roboto", fontface = "bold", size = 2.7) +
  scale_color_manual(values = c("#3A5C43", "#9C4035", "#C88630", "#385D65", "#432323", "#2F5755", "#430A5D", "#5F374B", "#182747" )) +
  coord_cartesian(clip = "off") +
  labs(
    title = "How Crime Rates Differ Across LAPD Stations in 2025",
    subtitle = "Monthly incidents per 10,000 residents based on total station-area population",
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 7.5, base_family = "roboto") +
  theme(
    plot.title = element_text(size = 18, family = "oswald", face = "bold"),
    plot.subtitle = element_text(size = 8.5, margin = margin(b = 7)),
    plot.caption = element_textbox_simple(hjust = 0, margin = margin(t = 10), size = 5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill = "#F0EFEB", color = "#F0EFEB"),
    strip.text = element_text(size = 8.5, face = "bold"),
    legend.position = "none",
    plot.margin = margin(5, 12, 5, 12),
    panel.grid.minor.y = element_blank()
  )

ggsave("projects/sheriff-substack/output/figures/lasd_monthly_crime_rates_2025.png", p, width = 7, height = 7)
