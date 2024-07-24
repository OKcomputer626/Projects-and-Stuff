library(tidyverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(grid)
library(patchwork)

font_add_google("Lato", "Lato")
font_add_google("Roboto", "Roboto")
font_add("Font Awesome 6 Brands", "fonts/otfs/Font Awesome 6 Brands-Regular-400.otf")
showtext_auto()
showtext_opts(dpi = 300)


df <- read_csv("Bayesian Leagues Cup/data/Liga MX and MLS Matches.csv")

liga_mx <- df %>%
  filter(Competition_Name %in% "Liga MX") %>%
  group_by(Team) %>%
  summarise(avg_goals = mean(Goals)) %>%
  ungroup()

mex_prop <- df %>%
  filter(Competition_Name %in% "Liga MX") %>%
  group_by(Team, Goals) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n)) %>%
  ungroup() %>%
  mutate(Goals = as_factor(Goals))

# Extract the team names from the Liga MX and the average goals for each team
team_names <- liga_mx %>%
  pull(Team)

mex_avg_gls <- liga_mx %>%
  pull(avg_goals)

# Define the range of goals to calculate probabilities for
goal_range <- 0:6

# Number of teams
num_teams <- length(team_names)

# Initialize a matrix to store probabilities
prob_matrix <- matrix(nrow = length(goal_range), ncol = num_teams)

# Calculate probabilities for each team and store in the matrix
for (i in 1:num_teams) {
  prob_matrix[, i] <- dpois(goal_range, mex_avg_gls[i])
}

# Convert the matrix to a data frame
df_mex <- as_tibble(prob_matrix)

# Set the column names to the team names
colnames(df_mex) <- team_names

# Add the goal range as a column
df_mex <- df_mex %>%
  mutate(Goals = as_factor(goal_range))


df_mex <- df_mex %>%
  pivot_longer(!Goals, names_to = "Team", values_to = "Prob") %>%
  arrange(Team) %>%
  relocate(Team) %>%
  mutate(type = "Expected")

mex_prop <- mex_prop %>%
  mutate(type = "Observed")

liga_mx_cleaned <- df_mex %>%
  full_join(mex_prop, by = c("Team", "Goals", "type", "Prob" = "freq")) %>%
  rename(Type = type) %>%
  arrange(Team)

# Define colors
colors <- c("Observed" = "#2B8B41", "Expected" = "#2B8B41")

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
    scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0), limits = c(0, 0.8), breaks = seq(0, 1, by = 0.1)) +
    labs(title = team) +
    coord_cartesian(clip = 'off') +
    annotation_custom(logo, xmin = 6, xmax = 7, ymin = 0.8, ymax = 0.92) + # Adjust these values as needed
    theme_minimal() +
    theme(
      text = element_text(family = "Lato"),
      plot.title = element_text(size = 9, face = "bold", hjust = 0), # Align title to top right
      axis.title.x = if(show_title_x) element_text(size = 7.5, color = "black") else element_blank(),
      axis.title.y = if(show_title_y) element_text(size = 7.5, color = "black") else element_blank(),
      axis.text.x = if(show_text_x) element_text(size = 7.5, color = "black") else element_blank(),
      axis.text.y = if(show_text_y) element_text(size = 7.5, color = "black") else element_blank(),
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
  "América" = "Bayesian Leagues Cup/images/Liga MX logos/America.png",
  "Atlas" = "Bayesian Leagues Cup/images/Liga MX logos/Atlas.png",
  "Atlético" = "Bayesian Leagues Cup/images/Liga MX logos/San Luis.png",
  "Cruz Azul" = "Bayesian Leagues Cup/images/Liga MX logos/Cruz Azul.png",
  "FC Juárez" = "Bayesian Leagues Cup/images/Liga MX logos/FC Juarez.png",
  "Guadalajara" = "Bayesian Leagues Cup/images/Liga MX logos/Chivas.png",
  "León" = "Bayesian Leagues Cup/images/Liga MX logos/Leon.png",
  "Mazatlán" = "Bayesian Leagues Cup/images/Liga MX logos/Mazatlan.png",
  "Monterrey" = "Bayesian Leagues Cup/images/Liga MX logos/Monterrey.png",
  "Necaxa" = "Bayesian Leagues Cup/images/Liga MX logos/Necaxa.png",
  "Pachuca" = "Bayesian Leagues Cup/images/Liga MX logos/Pachuca.png",
  "Puebla" = "Bayesian Leagues Cup/images/Liga MX logos/Puebla.png",
  "Querétaro" = "Bayesian Leagues Cup/images/Liga MX logos/Queretaro.png",
  "Santos" = "Bayesian Leagues Cup/images/Liga MX logos/Santos.png",
  "Tijuana" = "Bayesian Leagues Cup/images/Liga MX logos/Tijuana.png",
  "Toluca" = "Bayesian Leagues Cup/images/Liga MX logos/Toluca.png",
  "UANL" = "Bayesian Leagues Cup/images/Liga MX logos/Tigres.png",
  "UNAM" = "Bayesian Leagues Cup/images/Liga MX logos/Pumas.png"
)

df_split <- split(liga_mx_cleaned, liga_mx_cleaned$Team)

plots <- lapply(seq_along(df_split), function(i) {
  team <- names(df_split)[i]
  create_plot(df_split[[i]], liga_mx, logo_paths[team],
              show_title_y = ((i - 1) %% 3 == 0), 
              show_title_x = (i >= 16 & i <= 18), 
              show_text_y = ((i - 1) %% 3 == 0), 
              show_text_x = (i >= 16 & i <= 18))
})

# Combine plots using patchwork
combined_plot <- wrap_plots(plots, ncol = 3)


# from social caption file
source("Bayesian Leagues Cup/social_caption.R")
caption <- paste0(social_caption(font_family = "Lato", linkedin = "Andres Gonzalez", icon_color= "#2B8B41"))

LigaMX_plot <- combined_plot +
  plot_annotation(title = "Comparing Observed and Poisson-Modeled Goals for Liga MX Teams Clausura 2024",
                  caption = caption,
                  theme = theme(text = element_text(family = "Lato"),
                                plot.title = element_text(face = "bold"),
                                plot.caption = element_textbox_simple()))

ggsave("Bayesian Leagues Cup/images/Liga MX Goals and Expected Goals.png", LigaMX_plot, bg = "white", width = 8, height = 12)
