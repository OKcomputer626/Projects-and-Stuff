library(tidyverse)
library(worldfootballR)
library(showtext)
library(ggh4x)
library(patchwork)
library(grid)
library(png)

font_add_google("Montserrat", "Montserrat")
font_add_google("Roboto", "Roboto")
showtext_auto()
showtext_opts(dpi = 300)


df <- read_csv("MLS Results.csv")

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
                      labels = c("Observed",  paste("Expected (λ =", round(avg_goal, 2), ")"))) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(0, 0.5), breaks = seq(0, 1, by = 0.1)) +
    labs(title = team) +
    coord_cartesian(clip = 'off') +
    annotation_custom(logo, xmin = 6, xmax = 7, ymin = 0.5, ymax = 0.6) + # Adjust these values as needed
    theme_minimal() +
    theme(
      text = element_text(family = "Montserrat"),
      plot.title = element_text(size = 6, face = "bold", hjust = 0), # Align title to top right
      axis.title.x = if(show_title_x) element_text(size = 5.5, color = "black") else element_blank(),
      axis.title.y = if(show_title_y) element_text(size = 5.5, color = "black") else element_blank(),
      axis.text.x = if(show_text_x) element_text(size = 5.5, color = "black") else element_blank(),
      axis.text.y = if(show_text_y) element_text(size = 5.5, color = "black") else element_blank(),
      panel.grid = element_line(linetype = "dashed", linewidth = 0.15, color = "gray80"),
      panel.grid.minor = element_blank(),
      axis.line = element_line(linewidth = 0.3),
      axis.ticks = element_line(linewidth = 0.3),
      axis.ticks.length = unit(0.07, "cm"),
      legend.position = c(0.75, 0.9),
      legend.text = element_text(size = 4, margin = margin(l = 0.08, unit = "cm"), family = "Roboto"),
      legend.title = element_blank(),
      legend.key.height = unit(0.2, "cm"),
      legend.key.width = unit(0.4, "cm"),
      legend.key.spacing.y = unit(0.08, "cm")
    )
  
  if (show_title_x || show_title_y) {
    p <- p + labs(x = if(show_title_x) "Goals Scored" else NULL, y = if(show_title_y) "Probability" else NULL)
  }
  return(p)
}

# Load the logos as a named list (update paths accordingly)
logo_paths <- c(
  "Atlanta Utd" = "MLS logo/Atlanta Utd.png",
  "Austin" = "MLS logo/Austin.png",
  "CF Montréal" = "MLS logo/CF Montréal.png",
  "Charlotte" = "MLS logo/Charlotte.png",
  "Crew" = "MLS logo/Crew.png",
  "D.C. United" = "MLS logo/D.C. United.png",
  "Dynamo FC" = "MLS logo/Dynamo FC.png",
  "FC Cincinnati" = "MLS logo/FC Cincinnati.png",
  "FC Dallas" = "MLS logo/FC Dallas.png",
  "Fire" = "MLS logo/Fire.png",
  "Inter Miami" = "MLS logo/Inter Miami.png",
  "LA Galaxy" = "MLS logo/LA Galaxy.png",
  "LAFC" = "MLS logo/LAFC.png",
  "Minnesota Utd" = "MLS logo/Minnesota Utd.png",
  "Nashville" = "MLS logo/Nashville.png",
  "NE Revolution" = "MLS logo/NE Revolution.png",
  "NY Red Bulls" = "MLS logo/NY Red Bulls.png",
  "NYCFC" = "MLS logo/NYCFC.png",
  "Orlando City" = "MLS logo/Orlando City.png",
  "Philadelphia" = "MLS logo/Philadelphia.png",
  "Portland Timbers" = "MLS logo/Portland Timbers.png",
  "Rapids" = "MLS logo/Rapids.png",
  "RSL" = "MLS logo/RSL.png",
  "Seattle" = "MLS logo/Seattle.png",
  "SJ Earthquakes" = "MLS logo/SJ Earthquakes.png",
  "Sporting KC" = "MLS logo/Sporting KC.png",
  "St. Louis" = "MLS logo/St. Louis.png",
  "Toronto FC" = "MLS logo/Toronto FC.png",
  "Vancouver W'caps" = "MLS logo/Vancouver W'caps.png"
)

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
ggsave("MLS Goals and Expected Goals.png", combined_plot, bg = "white", width = 8, height = 8)





# Simulate some soccer match data (goals scored in 10 matches)
goals_scored <- c(2, 1, 3, 0, 2, 1, 4, 2, 3, 1)

# Prior parameters for Gamma distribution (shape = alpha, rate = beta)
alpha_prior <- 2
beta_prior <- 1

# Posterior parameters
alpha_posterior <- alpha_prior + sum(goals_scored)
beta_posterior <- beta_prior + length(goals_scored)

# Gamma prior and posterior
lambda <- seq(0, 10, by = 0.01)
prior <- dgamma(lambda, shape = alpha_prior, rate = beta_prior)
posterior <- dgamma(lambda, shape = alpha_posterior, rate = beta_posterior)

# Plotting prior and posterior distributions
df <- data.frame(lambda = rep(lambda, 2), 
                 density = c(prior, posterior), 
                 type = rep(c("Prior", "Posterior"), each = length(lambda)))

ggplot(df, aes(x = lambda, y = density, color = type)) +
  geom_line() +
  labs(title = "Gamma Prior and Posterior Distributions",
       x = expression(lambda),
       y = "Density") +
  theme_minimal()
