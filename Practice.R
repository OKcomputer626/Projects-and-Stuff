

# Define the parameters for the gamma distribution
shape_params <- c(19.2, 9.6, 192)
rate_params <- c(10, 5, 100)

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
ggplot(density_data, aes(x = x, y = density, color = Parameters)) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(
    title = "Gamma Distribution - Example of α and β parameters",
    x = expression(lambda ~ (goal ~ rate)),
    y = "Density",
    color = "Parameters"
  ) +
  scale_color_manual(values = c("purple", "orange", "cyan"))

