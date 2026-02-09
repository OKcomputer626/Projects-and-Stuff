library(tidyverse)
library(showtext)
library(glue)
library(ggtext)
library(slider)
library(scales)

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
  "**Source**: Beverly Hills Police Department (BH PD) crime data, 2025–Feb 3, 2026 | **Graphic**: ", social
)

crime_df <- read_csv("projects/Beverly Hills Crime/data/crime_clean_Feb.csv")


# --- 1) Make your daily table (your code, with 2 small upgrades: doy + safer weekend) ---
crime_daily <- crime_df %>%
  mutate(occurred_from_dt = as_date(occurred_from_dt)) %>%
  filter(year(occurred_from_dt) %in% c(2025, 2026)) %>%
  count(occurred_from_dt, name = "crimes") %>%
  complete(
    occurred_from_dt = seq(min(occurred_from_dt), max(occurred_from_dt), by = "day"),
    fill = list(crimes = 0L)
  ) %>%
  mutate(
    dow_num    = wday(occurred_from_dt),                 # 1=Sun ... 7=Sat
    dow        = wday(occurred_from_dt, label = TRUE, abbr = TRUE),
    is_weekend = as.integer(dow_num %in% c(1, 7)),
    doy        = yday(occurred_from_dt) - 1L,            # 0..365 (nice for cyclic smooth)
    dow = factor(dow, ordered = FALSE),
    month = month(occurred_from_dt, label = TRUE, abbr = TRUE),
    index      = row_number()
  )


p1 <- crime_daily %>%
  ggplot(aes(x = crimes)) +
  geom_histogram(binwidth = 1, fill = "#1F51FF", color = "grey25") +
  scale_x_continuous(breaks = seq(0, max(crime_daily$crimes), by = 1)) +
  scale_y_continuous(
    expand = c(0,0),
    limits = c(0, 85)
  ) +
  labs(
    title = "Crime Count Distribution in Beverly Hills",
    subtitle = "Reported incidents from January 2025 through February 3, 2026",
    x = NULL,
    y = NULL,
    caption = cap
  ) +
  theme_minimal(base_size = 7, base_family = "roboto") +
  theme(
    plot.title = element_text(size = 14, family = "oswald", face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 7)),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(hjust = 0, size = 4, margin = margin(t = 5)),
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = margin(5, 5, 5, 5),
    axis.line.x = element_line(linewidth = 0.5)
  )

ggsave("projects/Beverly Hills Crime/output/figures/beverly_hills_crime_count_distribution_2025_2026.png", p1, width = 5, height = 4)

crime_daily2 <- crime_daily %>%
  arrange(occurred_from_dt) %>%
  mutate(roll7 = slide_dbl(crimes, mean, .before = 6, .complete = TRUE))

p2 <- crime_daily2 %>%
  ggplot(aes(x = occurred_from_dt, y = crimes)) +
  geom_line(color = "#1F51FF", alpha = 0.3) +
  geom_line(aes(y = roll7), linewidth = 0.9, alpha = 1, color = "#1F51FF") +
  scale_x_date(
    breaks = "1 month",
    expand = expansion(mult = c(0.01, 0.01)),
    labels = function(x) {
      lab <- format(x, "%b")
      lab[format(x, "%Y-%m") == "2025-01"] <- "Jan\n2025"
      lab[format(x, "%Y-%m") == "2026-02"] <- "Feb\n2026"
      lab
    }
  ) +
  scale_y_continuous(
    limits = c(0, 12),
    breaks = scales::breaks_pretty()
  ) +
  labs(
    title = "Daily Crime Counts in Beverly Hills",
    subtitle = "Reported incidents per day, January 2025 through February 3, 2026 (7-day rolling average overlaid)",
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 7, base_family = "roboto") +
  theme(
    plot.title = element_text(size = 14, family = "oswald", face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 7)),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(hjust = 0, size = 4, margin = margin(t = 5)),
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.1),
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave("projects/Beverly Hills Crime/output/figures/beverly_hills_daily_crime_trend_2025_2026.png", p2, width = 7, height = 4)


end_date <- max(crime_daily$occurred_from_dt, na.rm = TRUE)

crime_365 <- crime_daily %>%
  filter(occurred_from_dt >= (end_date - 364),
         occurred_from_dt <= end_date) %>%
  group_by(month) %>%
  summarise(total = sum(crimes)) %>%
  ungroup()

p3 <- crime_365 %>%
  ggplot(aes(x = month, y = total)) +
  geom_col(fill = "#1F51FF") +
  geom_text(aes(label = total), nudge_y = -5, size = 2.8, color = "#FFFFFF", family = "roboto", fontface = "bold") +
  scale_y_continuous(
    expand = c(0,0),
    breaks = breaks_pretty()
  ) +
  labs(
    title = "Monthly Crime Counts in Beverly Hills",
    subtitle = "Monthly incident counts over the 365 days ending February 3, 2026",
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_minimal(base_size = 7, base_family = "roboto") +
  theme(
    plot.title = element_text(size = 14, family = "oswald", face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(hjust = 0, size = 4, margin = margin(t = 5)),
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = 0.1),
    axis.line.x = element_line(linewidth = 0.5),
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave("projects/Beverly Hills Crime/output/figures/beverly_hills_daily_crime_barchart_last_365_days.png", plot = p3, width = 5, height = 3.8)

crime_dow <- crime_daily %>%
  group_by(dow) %>%
  summarise(total = sum(crimes)) %>%
  ungroup() %>%
  mutate(
    is_weekend = dow %in% c("Sat", "Sun"),
    pct = total / sum(total),
    dow = factor(dow, levels = c("Mon","Tue","Wed","Thu","Fri","Sat","Sun"))
  )

p4 <- crime_dow %>%
  ggplot(aes(x = fct_rev(dow), y = pct, fill = is_weekend)) +
  geom_col() +
  geom_text(
    aes(label = percent(pct, accuracy = 1)),
    fontface = "bold",
    size = 2.8,
    family = "oswald",
    hjust = -0.2
  ) +
  scale_x_discrete(
    expand = c(0,0)
  ) +
  scale_y_continuous(
    expand = c(0,0),
    breaks = breaks_pretty(),
    labels = percent_format(accuracy = 1)
  ) +
  scale_fill_manual(
    values = c("FALSE" = "#1F51FF", "TRUE" = "#FFCD1F")
  ) +
  labs(
    title = "Share of Beverly Hills Crime by Day of Week",
    subtitle = "Percent of reported incidents by day, January 2025 through February 3, 2026",
    caption = cap,
    x = NULL,
    y = NULL
  ) +
  coord_flip(clip = "off") +
  theme_minimal(base_size = 7, base_family = "roboto") +
  theme(
    plot.title = element_text(size = 14, family = "oswald", face = "bold"),
    plot.subtitle = element_text(margin = margin(b = 10)),
    plot.title.position = "plot",
    plot.caption = element_textbox_simple(hjust = 0, size = 4, margin = margin(t = 10)),
    plot.caption.position = "plot",
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 7.5),
    plot.margin = margin(5, 30, 5, 5),
    legend.position = "none"
  )

ggsave("projects/Beverly Hills Crime/output/figures/beverly_hills_crime_by_day_of_week_2025_2026.png", plot = p4, width = 5, height = 3)


# m_nbi  <- gamlss2(crimes ~ s(index, k=25) + dow + s(doy, bs="cc", k=15),
#                   family = NBI,  data = crime_daily2)
# 
# m_nbii <- gamlss2(crimes ~ s(index, k=25) + dow + s(doy, bs="cc", k=15),
#                   family = NBII, data = crime_daily2)
# 
# AIC(m_nbi, m_nbii)
# 
# 
# # 1) Pick your final model (use the one with lower AIC)
# m_final <- m_nbi
# 
# # 2) Build the next 14 days with the SAME predictors
# last_date  <- max(crime_daily2$occurred_from_dt)
# last_index <- max(crime_daily2$index)
# 
# future_14 <- tibble(
#   occurred_from_dt = seq(last_date + days(1), by = "day", length.out = 14)
# ) %>%
#   mutate(
#     dow   = factor(wday(occurred_from_dt, label = TRUE, abbr = TRUE), ordered = FALSE),
#     doy   = (yday(occurred_from_dt) - 1L) %% 365,
#     index = last_index + row_number()
#   )
# 
# # 3) Get the predictive distribution for each day
# pf <- procast(m_final, newdata = future_14, drop = TRUE)
# 
# # 4) Summarize: mean + prediction intervals
# forecast_14 <- future_14 %>%
#   transmute(
#     date = occurred_from_dt,
#     mean = mean(pf),
#     lo80 = quantile(pf, 0.10),
#     hi80 = quantile(pf, 0.90),
#     lo95 = quantile(pf, 0.05),
#     hi95 = quantile(pf, 0.95)
#   )
# 
# forecast_14
# 
# recent_14 <- crime_daily2 %>%
#   arrange(occurred_from_dt) %>%
#   tail(14) %>%
#   select(date = occurred_from_dt, observed = crimes)
# 
# recent_14
# forecast_14
# 
# forecast_14 %>%
#   ggplot(aes(x = date, y = mean)) +
#   geom_line() +
#   geom_point() +
#   geom_ribbon(aes(ymin = lo95, ymax = hi95), alpha = 0.15) +
#   geom_ribbon(aes(ymin = lo80, ymax = hi80), alpha = 0.25) +
#   labs(x = NULL, y = "Predicted daily crimes")
# 
# crime_daily2_lags <- crime_daily2 %>%
#   arrange(index) %>%
#   mutate(
#     lag1  = dplyr::lag(crimes, 1),
#     lag7  = dplyr::lag(crimes, 7),
#     roll7 = slider::slide_dbl(crimes, mean, .before = 6, .complete = TRUE),
#     month = month(occurred_from_dt)
#   )
# 
# 
# 
# 
# df_roll7 <- crime_daily2_lags %>% filter(!is.na(roll7))
# 
# m_base <- gamlss2(
#   crimes ~ s(index, k = 25) + dow,
#   family = PO,
#   data = df_roll7
# )
# 
# m_forecast_simple <- gamlss2(
#   crimes ~ dow + roll7,
#   family = PO,
#   data = df_roll7
# )
# 
# m_forecast_trend <- gamlss2(
#   crimes ~ s(index, k = 25) + dow + roll7,
#   family = PO,
#   data = df_roll7
# )
# 
# AIC(m_base, m_forecast_simple, m_forecast_trend)
