# ---------------- Load Required Libraries ----------------
# Install required packages if they are not already installed
required_packages <- c("readr", "ggplot2", "car", "MASS", "lmtest", "GGally")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages)) install.packages(missing_packages)

# Load libraries
library(readr)   # For reading the data
library(ggplot2) # For data visualization
library(car)     # For diagnostic checks
library(MASS)    # For stepwise variable selection
library(lmtest)  # For statistical tests (e.g., Breusch-Pagan test)
library(GGally)  # For correlation pair plots

# ---------------- Load and Inspect Data ----------------
# Define the file path for the dataset
file_path <- "C:/Users/MSI/Downloads/bloodpress.txt"

# Load the data from a tab-delimited file
data <- read_delim(file_path, delim = "\t")

# Display the first few rows of the data
cat("\n--- First Few Rows of the Data ---\n")
print(head(data))

# Drop ID column
data <- subset(data, select = -c(Pt))

# Visualize correlation matrix
ggpairs(data)

# ---------------- Diagnostic Checks ----------------
# Fit the full model
model_full <- lm(BP ~ Age + Weight + BSA + Dur + Pulse + Stress, data = data)

# Multicollinearity Check: Variance Inflation Factor (VIF)
cat("\n--- Multicollinearity Check: Variance Inflation Factor ---\n")
vif_values <- vif(model_full)
print(vif_values)

cor(data$Weight, data$BSA)

# Convert VIF values to a data frame for plotting
vif_df <- data.frame(Variable = names(vif_values), VIF = vif_values)

# Set a threshold to indicate high VIF
high_vif_threshold <- 5

# Create a ggplot bar plot to visualize VIF values
ggplot(vif_df, aes(x = Variable, y = VIF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = high_vif_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, max(vif_df$VIF) + 1)) +
  labs(title = "Variance Inflation Factor (VIF) for Regression Model",
       y = "VIF",
       x = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------- Reduced Model ----------------
# Fit a reduced model
model_reduced <- lm(BP ~ Age + BSA + Dur + Pulse + Stress, data = data)
summary(model_reduced)

# ---------------- Reduced Model Diagnostic Plots ----------------
# 1. Linearity Check: Residuals vs Fitted values
plot(model_reduced, which = 1)

# 2. Normal Q-Q Plot
plot(model_reduced, which = 2)  # Normal Q-Q

# 3. Scale-Location Plot
plot(model_reduced, which = 3)  # Scale-Location

# 4. Cook's Distance Plot
plot(model_reduced, which = 4)  # Cook's Distance

# 5. Multicollinearity Check: Variance Inflation Factor (VIF)
cat("\n--- Multicollinearity Check: Variance Inflation Factor ---\n")
vif_values <- vif(model_reduced)
print(vif_values)

# Convert VIF values to a data frame for plotting
vif_df2 <- data.frame(Variable = names(vif_values), VIF = vif_values)

# Create a ggplot bar plot to visualize VIF values
ggplot(vif_df2, aes(x = Variable, y = VIF)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_hline(yintercept = high_vif_threshold, linetype = "dashed", color = "red") +
  scale_y_continuous(limits = c(0, max(vif_df$VIF) + 1)) +
  labs(title = "Variance Inflation Factor (VIF) for Regression Model",
       y = "VIF",
       x = "Variable") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---------------- Hypotheses Testing ----------------
# 1. Null hypothesis: All coefficients of predictors are zero (no effect).
# 2. Alternative hypothesis: At least one predictor has a non-zero effect.

# Perform an F-test for the overall significance of the model
anova_full <- anova(model_reduced)
cat("ANOVA Table:\n")
print(anova_full)

cat(sprintf("F-statistic: %.2f, p-value: %.4f\n", anova_full$`F value`[1], anova_full$`Pr(>F)`[1]))
if (anova_full$`Pr(>F)`[1] < 0.05) {
  cat("Result: Reject the null hypothesis. The model is significant.\n")
} else {
  cat("Result: Failed to reject the null hypothesis. The model is not significant.\n")
}

# ---------------- Log-Transformed Reduced Model ----------------
cat("\n--- Log-Transformed Model ---\n")
# Fit a log-transformed model
model_log <- lm(log(BP) ~ Age + BSA + Dur + Pulse + Stress, data = data)

# Display the summary of the log-transformed model
print(summary(model_log))

# Diagnostic plots for the log-transformed model
plot(model_log, main = "Diagnostic Plots for Log-Transformed Model")
if (bp_test$p.value < 0.05) {
  cat("Homoscedasticity assumption violated. Log-transformed model may better satisfy assumptions.\n")
}

# ---------------- Final Model Diagnostic Plots ----------------
cat("\n--- Diagnostic Plots for Final Model ---\n")

# 1. Residuals vs Fitted Plot
plot(model_log, which = 1)  # Residuals vs Fitted

# 2. Normal Q-Q Plot
plot(model_log, which = 2)  # Normal Q-Q

# 3. Scale-Location Plot
plot(model_log, which = 3)  # Scale-Location

# 4. Cook's Distance Plot
plot(model_log, which = 4)  # Cook's Distance