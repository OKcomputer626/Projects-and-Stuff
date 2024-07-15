library(tidyverse)

# Define the prior alpha and beta values
prior_params <- tibble(
  alpha = c(9.6, 19.2, 192),
  beta = c(5, 10, 100)
  )

# Observed data
n <- 8 # Number of matches
x <- 12 # Number of goals

# Update the parameters with observed data
posterior_params <- prior_params %>%
  mutate(alpha = alpha + x,
         beta = beta + n)

# Create a sequence of goal values
goal_range <- 0:6

# Compute the density values for each combination of parameters
negative_binomial <- lapply(1:length(posterior_params$alpha), function(i) {
  tibble(
    x = goal_range,
    density = dnbinom(goal_range, size = posterior_params$alpha[i], mu = posterior_params$alpha[i] / posterior_params$beta[i]),
    Parameters = paste0("Posterior P. Gamma, α = ", prior_params$alpha[i], ", β = ", prior_params$beta[i])
  )
})

negative_binomial <- do.call(rbind, negative_binomial)


# Plot the Posterior Negative Binomial distributions
negative_binomial %>%
  ggplot(aes(x = x, y = density, color = Parameters, shape = Parameters, fill = Parameters)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_color_manual(values = c("#3D405B", "#81B29A", "#E07A5F"),
                     breaks = c("Posterior P. Gamma, α = 9.6, β = 5", "Posterior P. Gamma, α = 19.2, β = 10", "Posterior P. Gamma, α = 192, β = 100")) +
  scale_shape_manual(values = c(16,16,25),
                     breaks = c("Posterior P. Gamma, α = 9.6, β = 5", "Posterior P. Gamma, α = 19.2, β = 10", "Posterior P. Gamma, α = 192, β = 100")) +
  scale_fill_manual(values = c("#3D405B","#81B29A","#E07A5F"),
                    breaks = c("Posterior P. Gamma, α = 9.6, β = 5", "Posterior P. Gamma, α = 19.2, β = 10", "Posterior P. Gamma, α = 192, β = 100")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, .35, by = 0.05),
                     limits = c(0, .35),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  labs(title = "Probability Distribution: Posterior Poisson-Gamma",
       x = "Goals",
       y = "Probability") +
  theme_minimal() +
  theme(
    text = element_text(family = "Montserrat"),
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    axis.line = element_line(linewidth = 0.3),
    axis.ticks = element_line(linewidth = 0.3),
    legend.position = "inside",
    legend.position.inside = c(0.8, 0.8),
    legend.title = element_blank(),
    legend.key.height = unit(0.1, "cm"),
    legend.key.width = unit(0.6, "cm"),
    legend.key.spacing.y = unit(0.2, "cm"),
    legend.text = element_text(size = 6.5, family = "Roboto")
  ) 

ggsave("MLS Predictions/Output/Posterior Poisson Gamma Distribution.png", width = 7, height = 4, bg = "white")

