# ----- B1705 Week 5 | Effect Sizes | 14.02.2024 -----
# ----- LECTURE WORK -----
# Cohen's D: used to measure the difference between two groups’ means
# in relation to their standard deviation, quantifying the size of the effect

# Pearsons correlation co-efficient: measure of the strength and direction 
# of a linear relationship between two continuous variables

# Odd's Ratios: used to show the relationship between two binary (yes/no) variables, commonly 
# used in studies to compare how likely an event is to happen in one group versus another:
# If the OR is greater than 1, it means the event is more likely in the first group.
# If it’s less than 1, the event is less likely in the first group.
# An OR of 1 means there’s no difference between the groups

# ----- 1. Cohen's D -----
# Code to produce data and dataframes for use 
rm(list=ls())

# Load  packages
library(effsize)
library(psych)

# Function to generate data
generate_data <- function(n = 100, mean1 = 50, sd1 = 10, mean2 = 60, sd2 = 10) {
  set.seed(123) # For reproducibility
  
  # Generate data for two groups
  group1 <- rnorm(n, mean1, sd1)
  group2 <- rnorm(n, mean2, sd2)
  
  # Combine into a data frame
  data <- data.frame(
    score = c(group1, group2),
    group = factor(c(rep("Group1", n), rep("Group2", n)))
  )
  
  return(data)
}

# Generate the dataset
data <- generate_data()

## Dataset Two
# Set seed for reproducibility
set.seed(123)

# Parameters
n <- 100           # Number of observations per group
mean_group1 <- 50  # Mean of group 1
mean_group2 <- 52  # Mean of group 2 (slightly higher to ensure a small effect size)
sd_both <- 6     # Standard deviation for both groups

# Generate data
group1 <- rnorm(n, mean_group1, sd_both)
group2 <- rnorm(n, mean_group2, sd_both)

# Combine into a data frame
data2 <- data.frame(
  score = c(group1, group2),
  group = factor(c(rep("Group1", n), rep("Group2", n)))
)

##### 1.2. Visualising distribution #####
library(ggplot2)

ggplot(data, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Scores by Group", x = "Group", y = "Score")

# Look for between-group differences using t-test
t.test(score ~ group, data = data, alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

##### 1.3. Calculate Cohen's D using effsize #####
d <- cohen.d(data$score,data$group)
print(d)

# ----- 2. Pearsons R -----
# Sample data set.seed(123) 
# For reproducibility
x <- rnorm(100, mean = 50, sd = 10) # Variable 1
y <- x * 1.5 + rnorm(100, mean = 0, sd = 5) # Variable 2, correlated with x

# Calculate Pearson's correlation
pearson_r <- cor(x, y, method = "pearson")
cat("\nPearson R Result:\n")
print(pearson_r)

# Scatter plot of x and y
plot(x, y, main = paste("Scatter Plot with Pearson's R =", round(pearson_r, 2)), xlab = "Variable X", ylab = "Variable Y", pch = 19, col = "blue")

# Test for significance
cor_test_result <- cor.test(x, y, method = "pearson")
cat("\nCor Test Result:\n")
print(cor_test_result)

# ----- 3. Odds Ratios -----
# Set seed for reproducibility
set.seed(123)

# Generate synthetic data
n <- 1000 # Number of observations

# Randomly assign treatment or control group
group <- sample(c("Treatment", "Control"), size = n, replace = TRUE)

# Simulate outcome based on group

# Assuming treatment group has lower odds of disease
outcome <- ifelse(group == "Treatment",
                  rbinom(n, 1, 0.3), # 30% probability of disease in treatment group
                  rbinom(n, 1, 0.5)) # 50% probability of disease in control group

# Create data frame
data3 <- data.frame(group, outcome)

# Create a 2x2 table
table_data <- table(data3$group, data3$outcome)
colnames(table_data) <- c("No Disease", "Disease")

# Calculate Odds Ratio and its 95% Confidence Interval using fisher.test
or_test <- fisher.test(table_data)

# Extract the Odds Ratio and its confidence interval
odds_ratio <- or_test$estimate
conf_interval <- or_test$conf.int

# Print the results
cat("Odds Ratio:", odds_ratio, "\n")
cat("95% Confidence Interval:", conf_interval, "\n")

# Calculate proportions
prop_table <- prop.table(table_data, 1) # Proportions by row

# Bar plot
barplot(prop_table, beside = TRUE, col = c("lightblue", "salmon"),
        legend = TRUE, args.legend = list(x = "topright"),
        names.arg = c("No Disease", "Disease"),
        xlab = "Outcome", ylab = "Proportion",
        main = "Proportion of Disease by Group")






