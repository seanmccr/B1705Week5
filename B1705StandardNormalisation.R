# ----- B1705 Week 5 | Standardisation and Normalisation | 14.02.2024 -----
# ----- PRE-LECTURE WORK -----

# Standardisation; This gives a mean of 0 and SD of 1 for each variable.
# Normalisation; the scales each variable to a range between 0 and 1.

# ----- 1. Standardisation -----
rm(list=ls())

set.seed(123) # Ensure reproducibility

# Generate synthetic data
data <- data.frame(
  X = rnorm(100, mean = 50, sd = 10), # Normally distributed data
  Y = runif(100, min = 200, max = 400) # Uniformly distributed data
)

# Original Data
hist(data$X, main = "Original X", xlab = "X")
hist(data$Y, main = "Original Y", xlab = "Y")

# We can standardise the data by using the scale() function. 
# This gives a mean of 0 and SD of 1 for each variable.
library(psych)
# Standardise data
data_standardised <- as.data.frame(scale(data))
# Summary to verify standardisation
describe(data_standardised) # using the psych library

# Plotting
par(mfrow = c(2, 2))

# Original Data
hist(data$X, main = "X", xlab = "X")
hist(data$Y, main = "Y", xlab = "Y")

# Standardised Data
hist(data_standardised$X, main = "Standardised X", xlab = "X")
hist(data_standardised$Y, main = "Standardised Y", xlab = "Y")

# ----- 2. Normalisation -----
# We can also normalise the original data. This scales each variable to a range between 0 and 1
# Normalize data function
normalise <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Apply normalisation
data_normalised <- as.data.frame(lapply(data, normalise))

# Summary to verify normalisation
describe(data_normalised)

# Plotting
par(mfrow = c(2, 2))

# Original Data
hist(data$X, main = "Original X", xlab = "X")
hist(data$X, main = "Original Y", xlab = "Y")

# Normalised Data
hist(data_normalised$X, main = "Normalised X", xlab = "X")
hist(data_normalised$Y, main = "Normalised Y", xlab = "Y")

# ----- 3. Further Visualisations -----

library(ggplot2)
library(reshape)
# Add a 'Type' column to each dataset
data$Type <- 'Original'
data_standardised$Type <- 'Standardised'
data_normalised$Type <- 'Normalised'

# Combine the datasets
combined_data <- rbind(data, data_standardised, data_normalised)

# Melt the combined data for ggplot2
library(reshape2)
data_melted <- melt(combined_data, id.vars = 'Type', variable.name = 'Vector', value.name = 'Value')

# Distribution plots
ggplot(data_melted, aes(x = Value, fill = Type)) + 
  geom_histogram(alpha = 0.6, position = "identity", bins = 30) + 
  facet_wrap(~Vector, scales = 'free') + 
  theme_minimal() + 
  scale_fill_brewer(palette = "Set1") + 
  labs(title = "Distribution of Vectors by Type", x = "Value", y = "Count")

# Box plots
ggplot(data_melted, aes(x = Vector, y = Value, color = Type)) + 
  geom_boxplot() + 
  facet_wrap(~Type, scales = 'free') + 
  theme_minimal() + 
  scale_color_brewer(palette = "Set2") + 
  labs(title = "Box Plot of Vectors by Preprocessing Type", x = "Vector", y = "Value")

# ----- LECTURE WORK -----


