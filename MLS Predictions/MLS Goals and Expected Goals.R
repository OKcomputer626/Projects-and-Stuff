library(tidyverse)
library(worldfootballR)
library(showtext)
library(ggh4x)
library(patchwork)
library(grid)
library(png)
library(sysfonts)
library(ggtext)

font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")
font_add("Font Awesome 6 Brands", "fonts/otfs/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()
showtext_opts(dpi = 300)


df <- read_csv("MLS Predictions/MLS Results.csv")

df <- df %>%
  pivot_longer(
    cols = c("Home", "Away"),
    names_to = "Home_Away",
    values_to = "Team"
  ) %>%
  select(-Round) %>%
  drop_na(HomeGoals)

df <- df %>%
  pivot_longer(
    cols = c("HomeGoals", "AwayGoals"),
    names_to = "Home_Away_Goals",
    values_to = "Goals"
  )

df <- df %>%
  filter((Home_Away == "Home" & Home_Away_Goals == "HomeGoals") | (Home_Away == "Away" & Home_Away_Goals == "AwayGoals"))

df_prop <- df %>%
  group_by(Team, Goals) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup()

df_prop <- df_prop %>% 
  mutate(Goals = as_factor(Goals))

df_avg <- df %>%
  group_by(Team) %>%
  summarise(avg_goals = mean(Goals)) %>%
  ungroup()

# Extract the average goals for each team
team_names <- df_avg$Team
avg_goals <- df_avg$avg_goals

# Define the range of goals to calculate probabilities for
goal_range <- 0:6

# Number of teams
num_teams <- length(team_names)

# Initialize a matrix to store probabilities
prob_matrix <- matrix(nrow = length(goal_range), ncol = num_teams)

# Calculate probabilities for each team and store in the matrix
for (i in 1:num_teams) {
  prob_matrix[, i] <- dpois(goal_range, avg_goals[i])
}

# Convert the matrix to a data frame
prob_df <- as_tibble(prob_matrix)

# Set the column names to the team names
colnames(prob_df) <- team_names

# Add the goal range as a column
prob_df$Goals <- as_factor(goal_range)

prob_df <- prob_df %>%
  pivot_longer(!Goals, names_to = "Team", values_to = "Prob") %>%
  arrange(Team) %>%
  relocate(Team) %>%
  mutate(type = "Expected")

df_prop <- df_prop %>%
  mutate(type = "Observed")

df_cleaned <- prob_df %>%
  full_join(df_prop, by = c("Team", "Goals", "type", "Prob" = "freq")) %>%
  rename(Type = type) %>%
  arrange(Team)

# Define colors
colors <- c("Observed" = "#0F2143", "Expected" = "#0F2143")

# Function to create individual plots
create_plot <- function(data, avg_goals, logo_path, show_title_y = FALSE, show_title_x = FALSE, show_text_y = FALSE, show_text_x = FALSE) {
  team <- unique(data$Team)
  avg_goal <- avg_goals %>% filter(Team == team) %>% pull(avg_goals)
  
  # Read the logo image
  logo <- png::readPNG(logo_path) %>%
    rasterGrob(interpolate = TRUE)
  
  p <- ggplot(data) +
    geom_col(aes(x = Goals, y = ifelse(Type == "Observed", Prob, NA), fill = "Observed"), width = 0.35) +
    geom_col(aes(x = Goals, y = ifelse(Type == "Expected", Prob, NA), fill = "Expected"), width = 0.65, alpha = 0.35) +
    scale_fill_manual(values = colors, 
                      breaks = c("Observed", "Expected"), 
                      labels = c("Observed",  paste("Poisson (λ =", round(avg_goal, 2), ")"))) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(0, 0.5), breaks = seq(0, 1, by = 0.1)) +
    labs(title = team) +
    coord_cartesian(clip = 'off') +
    annotation_custom(logo, xmin = 6, xmax = 7, ymin = 0.5, ymax = 0.6) + # Adjust these values as needed
    theme_minimal() +
    theme(
      text = element_text(family = "Montserrat"),
      plot.title = element_text(size = 8, face = "bold", hjust = 0), # Align title to top right
      axis.title.x = if(show_title_x) element_text(size = 7, color = "black") else element_blank(),
      axis.title.y = if(show_title_y) element_text(size = 7, color = "black") else element_blank(),
      axis.text.x = if(show_text_x) element_text(size = 7, color = "black") else element_blank(),
      axis.text.y = if(show_text_y) element_text(size = 7, color = "black") else element_blank(),
      panel.grid = element_line(linetype = "dashed", linewidth = 0.15, color = "gray80"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(linewidth = 0.3),
      axis.ticks = element_line(linewidth = 0.3),
      axis.ticks.length = unit(0.07, "cm"),
      legend.position = c(0.75, 0.9),
      legend.text = element_text(size = 6.5, margin = margin(l = 0.08, unit = "cm"), family = "Roboto"),
      legend.title = element_blank(),
      legend.key.height = unit(0.2, "cm"),
      legend.key.width = unit(0.45, "cm"),
      legend.key.spacing.y = unit(0.08, "cm")
    )
  
  if (show_title_x || show_title_y) {
    p <- p + labs(x = if(show_title_x) "Goals Scored" else NULL, y = if(show_title_y) "Probability" else NULL)
  }
  return(p)
}

# Load the logos as a named list (update paths accordingly)
logo_paths <- c(
  "Atlanta Utd" = "MLS Predictions/MLS Team Logos/Atlanta Utd.png",
  "Austin" = "MLS Predictions/MLS Team Logos/Austin.png",
  "CF Montréal" = "MLS Predictions/MLS Team Logos/CF Montréal.png",
  "Charlotte" = "MLS Predictions/MLS Team Logos/Charlotte.png",
  "Crew" = "MLS Predictions/MLS Team Logos/Crew.png",
  "D.C. United" = "MLS Predictions/MLS Team Logos/D.C. United.png",
  "Dynamo FC" = "MLS Predictions/MLS Team Logos/Dynamo FC.png",
  "FC Cincinnati" = "MLS Predictions/MLS Team Logos/FC Cincinnati.png",
  "FC Dallas" = "MLS Predictions/MLS Team Logos/FC Dallas.png",
  "Fire" = "MLS Predictions/MLS Team Logos/Fire.png",
  "Inter Miami" = "MLS Predictions/MLS Team Logos/Inter Miami.png",
  "LA Galaxy" = "MLS Predictions/MLS Team Logos/LA Galaxy.png",
  "LAFC" = "MLS Predictions/MLS Team Logos/LAFC.png",
  "Minnesota Utd" = "MLS Predictions/MLS Team Logos/Minnesota Utd.png",
  "Nashville" = "MLS Predictions/MLS Team Logos/Nashville.png",
  "NE Revolution" = "MLS Predictions/MLS Team Logos/NE Revolution.png",
  "NY Red Bulls" = "MLS Predictions/MLS Team Logos/NY Red Bulls.png",
  "NYCFC" = "MLS Predictions/MLS Team Logos/NYCFC.png",
  "Orlando City" = "MLS Predictions/MLS Team Logos/Orlando City.png",
  "Philadelphia" = "MLS Predictions/MLS Team Logos/Philadelphia.png",
  "Portland Timbers" = "MLS Predictions/MLS Team Logos/Portland Timbers.png",
  "Rapids" = "MLS Predictions/MLS Team Logos/Rapids.png",
  "RSL" = "MLS Predictions/MLS Team Logos/RSL.png",
  "Seattle" = "MLS Predictions/MLS Team Logos/Seattle.png",
  "SJ Earthquakes" = "MLS Predictions/MLS Team Logos/SJ Earthquakes.png",
  "Sporting KC" = "MLS Predictions/MLS Team Logos/Sporting KC.png",
  "St. Louis" = "MLS Predictions/MLS Team Logos/St. Louis.png",
  "Toronto FC" = "MLS Predictions/MLS Team Logos/Toronto FC.png",
  "Vancouver W'caps" = "MLS Predictions/MLS Team Logos/Vancouver W'caps.png"
)

df_split <- split(df_cleaned, df_cleaned$Team)

plots <- lapply(seq_along(df_split), function(i) {
  team <- names(df_split)[i]
  create_plot(df_split[[i]], df_avg, logo_paths[team],
              show_title_y = ((i - 1) %% 6 == 0), 
              show_title_x = (i >= 25 & i <= 29), 
              show_text_y = ((i - 1) %% 6 == 0), 
              show_text_x = (i >= 25 & i <= 29))
})

combined_plot <- wrap_plots(plots)
print(combined_plot)


#from social caption file
source("MLS Predictions/social_caption.R")
caption = paste0(social_caption(font_family = "Montserrat", linkedin = "Andres Gonzalez", icon_color= "#0F2143"))

MLS_plot <- combined_plot +
  plot_annotation(title = "Comparing Observed and Poisson-Modeled Goals for MLS Teams 2024",
                  caption = caption,
                  theme = theme(text = element_text(family = "Montserrat"),
                                plot.title = element_text(face = "bold"),
                                plot.caption = element_textbox_simple()))

ggsave("MLS Predictions/Output/MLS Goals and Expected Goals.png", MLS_plot, bg = "white", width = 10, height = 10)
