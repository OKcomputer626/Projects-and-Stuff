library(tidyverse)

# Define the parameters for the Gamma distribution (which leads to the Negative Binomial distribution)
shape_params <- c(9.6, 19.2, 192)
rate_params <- c(5, 10, 100)

lambda_poisson <- 1.92

# Calculate the probability of scoring x goals for a range of goals (0 to 6)
goals <- 0:6

# Compute the density values for each combination of parameters
nb_densities <- lapply(1:length(shape_params), function(i) {
  tibble(
    x = goals,
    density = dnbinom(goals, size = shape_params[i], mu = shape_params[i] / rate_params[i]),
    Parameters = paste0("Poisson Gamma, α = ", shape_params[i], ", β = ", rate_params[i])
  )
})

# Compute the Poisson distribution for comparison
poisson_density <- tibble(
  x = goals,
  density = dpois(goals, lambda = lambda_poisson),
  Parameters = paste0("Poisson λ = ", lambda_poisson)
)

density_data <- do.call(rbind, nb_densities)
density_data <- bind_rows(density_data, poisson_density)

# Plot
density_data %>%
  ggplot(aes(x = x, y = density, color = Parameters, shape = Parameters, fill = Parameters)) +
  geom_col(data = subset(density_data, Parameters %in% "Poisson λ = 1.92"), width = 0.8) +
  geom_line(data = subset(density_data, !Parameters %in% "Poisson λ = 1.92"), linewidth = 0.6) +
  geom_point(data = subset(density_data, !Parameters %in% "Poisson λ = 1.92"), size = 2) +
  scale_color_manual(values = c("#3D405B", "#81B29A", "#E07A5F", "grey50"),
                     breaks = c("Poisson Gamma, α = 9.6, β = 5", "Poisson Gamma, α = 19.2, β = 10", "Poisson Gamma, α = 192, β = 100", "Poisson λ = 1.92")) +
  scale_shape_manual(values = c(16,16,25,1),
                     breaks = c("Poisson Gamma, α = 9.6, β = 5", "Poisson Gamma, α = 19.2, β = 10", "Poisson Gamma, α = 192, β = 100", "Poisson λ = 1.92")) +
  scale_fill_manual(values = c("#3D405B","#81B29A","#E07A5F","grey50"),
                    breaks = c("Poisson Gamma, α = 9.6, β = 5", "Poisson Gamma, α = 19.2, β = 10", "Poisson Gamma, α = 192, β = 100", "Poisson λ = 1.92")) +
  scale_y_continuous(labels = scales::percent,
                     breaks = seq(0, .30, by = 0.05),
                     limits = c(0, .30),
                     expand = c(0,0)) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) +
  labs(title = "Probability Distribution: Poisson-Gamma",
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

ggsave("MLS Predictions/Output/Poisson Gamma Distribution.png", width = 7, height = 4, bg = "white")

