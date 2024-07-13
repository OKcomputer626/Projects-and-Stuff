library(ggtext)

# Define the parameters for the gamma distribution
shape_params <- c(9.6, 19.2, 192)
rate_params <- c(5, 10, 100)

# Generate a sequence of values for plotting the density
x_values <- seq(0, 4, length.out = 1000)

# Compute the density values for each combination of parameters
densities <- lapply(1:length(shape_params), function(i) {
  data.frame(
    x = x_values,
    density = dgamma(x_values, shape = shape_params[i], rate = rate_params[i]),
    Parameters = paste0("α = ", shape_params[i], ", β = ", rate_params[i])
  )
})

# Combine the density data into a single data frame
density_data <- do.call(rbind, densities)

# Plot the gamma distributions
ggplot(density_data, aes(x = x, y = density, color = as_factor(Parameters))) +
  geom_line(linewidth = 0.6) +
  theme_classic() +
  labs(
    title = "Gamma Distribution: Examples of **Shape (α)** and **Rate (β)** Parameters",
    x = expression(lambda ~ (goal ~ rate)),
    y = NULL
  ) +
  scale_color_manual(values = c("#3D405B", "#81B29A", "#E07A5F"),
                     breaks = c("α = 9.6, β = 5", "α = 19.2, β = 10", "α = 192, β = 100")) +
  scale_x_continuous(breaks = seq(0, 4, 0.5)) +
  scale_y_continuous(breaks = seq(0, 3, 0.5)) +
  theme(
    text = element_text(family = "Roboto"),
    plot.title = element_markdown(hjust = 0.5),
    legend.title = element_blank(),
    axis.text = element_text(color = "black"),
    legend.position = "inside",
    legend.position.inside = c(0.7, 0.7),
    legend.key.size = unit(0.5, "cm")
  ) 

ggsave("MLS Predictions/Output/Gamma Distribuition.png", width = 7, height = 4)
