library(tidyverse)
library(worldfootballR)
library(showtext)
library(sysfonts)
library(ggtext)


font_add_google("Lato", "Lato")
font_add_google("Roboto", "Roboto")
font_add("Font Awesome 6 Brands", "fonts/otfs/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()
showtext_opts(dpi = 300)



mex_teams <- fb_match_results(country = "MEX", gender = "M", season_end_year = 2025)
usa_teams <- fb_match_results(country = "USA", gender = "M", season_end_year = 2024)


mex_teams <- mex_teams %>%
  drop_na(HomeGoals) %>%
  pivot_longer(
    cols = c("Home", "Away"),
    names_to = "Home_Away",
    values_to = "Team"
  ) %>%
  select(-Round)


usa_teams <- usa_teams %>%
  drop_na(HomeGoals) %>%
  pivot_longer(
    cols = c("Home", "Away"),
    names_to = "Home_Away",
    values_to = "Team"
  ) %>%
  select(-Round)

mex_teams <- mex_teams %>%
  pivot_longer(
    cols = c("HomeGoals", "AwayGoals"),
    names_to = "Home_Away_Goals",
    values_to = "Goals"
  )

usa_teams <- usa_teams %>%
  pivot_longer(
    cols = c("HomeGoals", "AwayGoals"),
    names_to = "Home_Away_Goals",
    values_to = "Goals"
  )

mex_teams <- mex_teams %>%
  filter((Home_Away == "Home" & Home_Away_Goals == "HomeGoals") | (Home_Away == "Away" & Home_Away_Goals == "AwayGoals"))

usa_teams <- usa_teams %>%
  filter((Home_Away == "Home" & Home_Away_Goals == "HomeGoals") | (Home_Away == "Away" & Home_Away_Goals == "AwayGoals"))



mex_teams <- mex_teams %>%
  mutate(Goals = as_factor(Goals))

usa_teams <- usa_teams %>%
  mutate(Goals = as_factor(Goals))

df <- bind_rows(mex_teams, usa_teams)

write_csv(df, "Bayesian Leagues Cup/data/Liga MX and MLS Matches.csv")

source("Bayesian Leagues Cup/social_caption.R")
caption <- paste0(social_caption(font_family = "Lato", linkedin = "Andres Gonzalez", icon_color= "gray50"))

df %>%
  group_by(Competition_Name, Goals) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  ggplot(aes(x = Goals, y = freq, fill = Competition_Name)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values = c("#2B8B41", "#001F5B")) +
  scale_y_continuous(expand = c(0,0),
                     breaks = seq(0, .35, by = 0.05)) +
  labs(x = "Number of Goals",
       y = "Proportion of Matches",
       title = "Goal Proportion Distribution in Liga MX and MLS",
       caption = caption) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.caption = element_textbox_simple(size = 7.5),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    axis.text = element_text(color = "black"),
    legend.title = element_blank(),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8),
    legend.key.height = unit(0.1, "cm"),
    legend.key.width = unit(0.6, "cm"),
    legend.key.spacing.y = unit(0.2, "cm"))

ggsave("Bayesian Leagues Cup/images/Goal Proportion Distribution in Liga MX and MLS.png", bg = "white", width = 7, height = 4)
