# Loading the state dataset
data("state")

# Combine state.abb (abbreviations), state.name, and other numeric data into a data frame
state_data <- data.frame(
  Abbreviation = state.abb,
  State = state.name,
  Region = state.region,
  Population = state.x77[, "Population"],
  Income = state.x77[, "Income"],
  Illiteracy = state.x77[, "Illiteracy"],
  LifeExp = state.x77[,"Life Exp"],
  Murder = state.x77[,"Murder"],
  HSGrad = state.x77[,"HS Grad"],
  Area = state.x77[,"Area"],
  Frost = state.x77[, "Frost"]
)

# 1. Data Overview
# Display structure and dimensions of the dataset
str(state_data)
cat("\nNumber of observations:", nrow(state_data), "\n")
cat("Number of variables:", ncol(state_data), "\n\n")

# 2. Summary Statistics for a numerical variable (e.g., Life Expectancy)
summary_stats <- function(var) {
  cat("Summary Statistics for", var, "\n")
  cat("Mean:", mean(state_data[[var]], na.rm = TRUE), "\n")
  cat("Median:", median(state_data[[var]], na.rm = TRUE), "\n")
  cat("Standard Deviation:", sd(state_data[[var]], na.rm = TRUE), "\n")
  cat("Minimum:", min(state_data[[var]], na.rm = TRUE), "\n")
  cat("Maximum:", max(state_data[[var]], na.rm = TRUE), "\n")
}
summary_stats("LifeExp")  # Use Life Expectancy column for analysis

# 3. Distribution Visualization (e.g., Life Expectancy)
# Histogram for Life Expectancy
par(mfrow = c(1, 1))
hist(state_data$LifeExp, breaks = 5, col = "black", xlab = "Life Expectancy", 
     main = "Histogram of Life Expectancy across States")
# Boxplot for Life Expectancy
par(mfrow = c(1, 1))
boxplot(state_data$LifeExp, horizontal = TRUE, col = "green", main = "Boxplot of Life Expectancy across States")

# 4. Categorical Variable Analysis (e.g., Region)
# Bar plot for Region (categorical variable)
par(mfrow = c(1, 1))
barplot(table(state_data$Region), col = "black", main = "Bar Plot of States by Region", 
        xlab = "Region", ylab = "Count")

# 5. Correlation Analysis (e.g., Murder and Life Expectancy)
correlation <- cor(state_data$Murder, state_data$LifeExp, use = "complete.obs")
cat("\nPearson Correlation between Murder and Life Expectancy:", correlation, "\n")

# 6. Scatter Plot Visualization
par(mfrow = c(1, 1))
plot(state_data$Murder, state_data$LifeExp, main = "Scatter Plot of Murder vs Life Expectancy",
     xlab = "Murder", ylab = "Life Expectancy", pch = 19, col = "black")
abline(lm(state_data$LifeExp ~ state_data$Murder), col = "red")  # Add regression line

# 7. Multiple Regression
# Fit a linear regression model predicting Life Expectancy using Murder and HS Grad
model <- lm(LifeExp ~ Murder + HSGrad, data = state_data)
print(summary(model))

# 8. Model Diagnostics
# Plot the residuals of the regression model
# Plot the residuals vs fitted values
par(mfrow = c(1, 1))  # Ensure only one plot is shown
plot(model, which = 1, pch = 16)  # Residuals vs Fitted with solid dots

# Plot the Normal Q-Q plot
par(mfrow = c(1, 1))  # Ensure only one plot is shown
plot(model, which = 2, pch = 16)  # Normal Q-Q with solid dots

# Plot the Scale-Location plot
par(mfrow = c(1, 1))  # Ensure only one plot is shown
plot(model, which = 3, pch = 16)  # Scale-Location with solid dots

# Plot the Cook's distance plot
par(mfrow = c(1, 1))  # Ensure only one plot is shown
plot(model, which = 4, pch = 16)  # Cook's distance with solid dots

# 9. Principal Component Analysis (PCA)
# Perform PCA on numerical variables only (Population, Income, Illiteracy, LifeExp, Murder, HSGrad, Area and Frost)
numerical_data <- state_data[, c("Population", "Income", "Illiteracy", "LifeExp", "Murder", "HSGrad", "Area", "Frost")]
pca <- prcomp(numerical_data, scale. = TRUE)
summary(pca)

# 10. PCA Interpretation
# Calculate the percentage of variance explained by each principal component
explained_variance <- summary(pca)$importance[2,]  # Extracting the proportion of variance explained

# Plot the scree plot (Explained variance)
par(mfrow = c(1, 1))
plot(explained_variance * 100, type = "b", 
     xlab = "Principal Component", 
     ylab = "Variance Explained (%)", 
     main = "Scree Plot with Variance Explained in Percentage",
     pch = 16, col = "black")

# Biplot for PCA interpretation
par(mfrow = c(1, 1))
biplot(pca, main = "PCA Biplot of State Data", cex=0.7, xlabs=rep("", nrow(state_data)), xlim=c(-0.4,0.4), ylim=c(-0.2, 0.2))