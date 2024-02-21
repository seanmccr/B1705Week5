# ----- B1705 Week 5 | Odds Ratios | 14.02.2024 -----

# ----- Step 1: Creating Datasets and Loading Libraries -----
# Define the counts of recovery vs. no recovery for both treatment and control groups
treatment_recovered <- 60    # Patients recovered with treatment
treatment_not_recovered <- 40 # Patients not recovered with treatment
control_recovered <- 30       # Patients recovered without treatment
control_not_recovered <- 70   # Patients not recovered without treatment

# Create a matrix to represent this data
data_matrix <- matrix(c(treatment_recovered, treatment_not_recovered,
                        control_recovered, control_not_recovered),
                      nrow = 2, byrow = TRUE,
                      dimnames = list(c("Treatment", "Control"),
                                      c("Recovered", "Not_Recovered")))

# Look at the matrix
data_matrix

# ----- Step 2: Calculating Odds Ratio -----

# Calculate the Odds Ratio manually
treatment_odds <- treatment_recovered / treatment_not_recovered
control_odds <- control_recovered / control_not_recovered
odds_ratio <- treatment_odds / control_odds

# Print the Odds Ratio
odds_ratio

# ----- Step 3: Calculate the Odds Ratio Using a Predefined Function -----

# Calculate the Odds Ratio using Fisher's Exact Test
fisher_result <- fisher.test(data_matrix)

# The odds ratio is given in the result, along with the confidence interval
fisher_odds_ratio <- fisher_result$estimate
conf_int <- fisher_result$conf.int

# Print the results
fisher_odds_ratio
conf_int

# ----- Step 4: Interpret the Results -----

# When interpreting the odds ratio:
  
#  An OR of 1 suggests no association between the treatment and recovery.

# An OR greater than 1 suggests an increased odds of recovery associated with the treatment.

# An OR less than 1 suggests a decreased odds of recovery associated with the treatment.

# ----- Step 5: Calculate Confidence Intervals -----

# Extracting the confidence interval from the Fisher's Exact Test
lower_ci <- conf_int[1]
upper_ci <- conf_int[2]

# Printing the confidence interval
cat("95% CI for OR: [", lower_ci, ",", upper_ci, "]", "\n")






