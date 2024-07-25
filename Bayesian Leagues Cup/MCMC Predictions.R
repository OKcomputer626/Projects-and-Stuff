library(tidyverse)
library(rstan)
library(janitor)
library(bayesplot)

df <- read_csv("Bayesian Leagues Cup/data/Liga MX and MLS Matches.csv")

glimpse(df)

avg_goals <- mean(df$Goals)
var_goals <- var(df$Goals)

# Calculate alpha and beta
alpha <- (avg_goals^2) / var_goals
beta <- avg_goals / var_goals

# Define the Poisson-Gamma model in Stan
gp_model <- "
  data {
    int<lower = 0> N;          // Number of observations
    int<lower = 0> goals[N];   // Number of goals
    real<lower = 0> alpha;     // Shape parameter for Gamma prior
    real<lower = 0> beta;      // Rate parameter for Gamma prior
  }
  parameters {
    real<lower = 0> lambda;    // Average goals per match
  }
  model {
    goals ~ poisson(lambda);   // Likelihood: Goals follow a Poisson distribution with mean lambda
    lambda ~ gamma(alpha, beta); // Prior: lambda follows a Gamma distribution with shape alpha and rate beta
  }
"

df_filtered <- df %>%
  filter(Date >= "2024-06-19") %>%
  select(Team, Goals)

# Function to fit the model for each team with chosen prior
fit_team_model <- function(team_goals) {
  data_list <- list(N = length(team_goals), goals = team_goals, alpha = alpha, beta = beta)
  fit <- stan(model_code = gp_model, data = data_list, chains = 4, iter = 1000*2, seed = 84735)
  return(fit)
}


# Fit the model for each team
team_models <- lapply(split(df_filtered$Goals, df_filtered$Team), fit_team_model)

mcmc_trace(team_models[[1]], pars = "lambda", size = 0.1)

# Histogram of the Markov chain values
mcmc_hist(team_models[[1]], pars = "lambda") + 
  yaxis_text(TRUE) + 
  ylab("count")

# Density plot of the Markov chain values
mcmc_dens(team_models[[1]], pars = "lambda") + 
  yaxis_text(TRUE) + 
  ylab("density")

mcmc_dens_overlay(team_models[[1]], pars = "lambda") + 
  ylab("density")

# Calculate the effective sample size ratio
neff_ratio(team_models[[1]], pars = c("lambda"))

mcmc_acf(team_models[[1]], pars = "lambda")

rhat(team_models[[1]], pars = "lambda")

america <- as.data.frame(team_models[[1]], pars = "lp__", include = FALSE)


# Set the seed
set.seed(1)

# Predict a value of Y' for each pi value in the chain
america <- america %>% 
  mutate(y_predict = rpois(length(lambda), lambda = lambda))

ggplot(america, aes(x = y_predict)) + 
  stat_count()

team_models <- read_rds("team_models.RData")

matches <- read_csv("Bayesian Leagues Cup/data/matches.csv")


# Function to predict goals for the next match using the fitted model
predict_goals <- function(team, team_models) {
  fit <- team_models[[team]]
  posterior_lambda <- extract(fit)$lambda
  predicted_goals <- rpois(length(posterior_lambda), lambda = posterior_lambda)
  return(predicted_goals)  # Return the full distribution of predicted goals
}

# Define the function to calculate points for a match
calculate_points <- function(home_goals, away_goals) {
  if (home_goals > away_goals) {
    return(c(3, 0))  # Home team wins
  } else if (home_goals < away_goals) {
    return(c(0, 3))  # Away team wins
  } else {
    return(c(1, 1))  # Draw
  }
}

# Initialize a list to store the expected points for each team
team_points <- list()

# Loop through each match
for (match in 1:nrow(matches)) {
  team1 <- matches$Team[match]
  team2 <- matches$Opponent[match]
  
  team1_goals <- predict_goals(team1, team_models)
  team2_goals <- predict_goals(team2, team_models)
  
  # Calculate the average predicted points for both teams
  team1_points <- numeric(length(team1_goals))
  team2_points <- numeric(length(team2_goals))
  
  for (k in 1:length(team1_goals)) {
    points <- calculate_points(team1_goals[k], team2_goals[k])
    team1_points[k] <- points[1]
    team2_points[k] <- points[2]
  }
  
  if (!is.null(team_points[[team1]])) {
    team_points[[team1]] <- team_points[[team1]] + mean(team1_points)
  } else {
    team_points[[team1]] <- mean(team1_points)
  }
  
  if (!is.null(team_points[[team2]])) {
    team_points[[team2]] <- team_points[[team2]] + mean(team2_points)
  } else {
    team_points[[team2]] <- mean(team2_points)
  }
}

# Convert the list to a tibble for easier viewing
expected_points_tibble <- tibble(
  Team = names(team_points),
  ExpectedPoints = unlist(team_points)
)

# Display the tibble
print(expected_points_tibble)