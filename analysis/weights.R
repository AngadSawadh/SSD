# 1. Load your CSV file
data <- read.csv("./data/weight_change_dataset.csv")  # Replace with the actual path to your CSV file

# 2. Data Overview
# Display structure and dimensions of the dataset
str(data)
cat("\nNumber of observations:", nrow(data), "\n")
cat("Number of variables:", ncol(data), "\n\n")

# 3. Summary Statistics for a numerical variable (e.g., Final.Weight..lbs.)
summary_stats <- function(var) {
  cat("Summary Statistics for", var, "\n")
  cat("Mean:", mean(data[[var]], na.rm = TRUE), "\n")
  cat("Median:", median(data[[var]], na.rm = TRUE), "\n")
  cat("Standard Deviation:", sd(data[[var]], na.rm = TRUE), "\n")
  cat("Minimum:", min(data[[var]], na.rm = TRUE), "\n")
  cat("Maximum:", max(data[[var]], na.rm = TRUE), "\n")
}
summary_stats("Final.Weight..lbs.")  # Use Final.Weight..lbs. column for analysis

# 4. Distribution Visualization (e.g., Final.Weight..lbs.)
# Histogram for Final.Weight..lbs.
par(mfrow = c(1, 1))
hist(data$Final.Weight..lbs., breaks = 5, col = "black", xlab = "Final.Weight..lbs.", 
     main = "Histogram of Final.Weight..lbs.")

# Boxplot for Final.Weight..lbs.
par(mfrow = c(1, 1))
boxplot(data$Final.Weight..lbs., horizontal = TRUE, col = "green", main = "Boxplot of Final.Weight..lbs.")

# 5. Categorical Variable Analysis (e.g., Gender)
# Bar plot for Gender (categorical variable)
par(mfrow = c(1, 1))
barplot(table(data$Gender), col = "black", main = "Bar Plot of Gender", 
        xlab = "Gender", ylab = "Count")

# 6. Correlation Analysis (e.g., BMR..Calories. and Final.Weight..lbs.)
correlation <- cor(data$BMR..Calories., data$Final.Weight..lbs., use = "complete.obs")
cat("\nPearson Correlation between BMR..Calories. and Final.Weight..lbs.:", correlation, "\n")

# 7. Scatter Plot Visualization
par(mfrow = c(1, 1))
plot(data$BMR..Calories., data$Final.Weight..lbs., main = "Scatter Plot of BMR..Calories. vs Final.Weight..lbs.",
     xlab = "BMR..Calories.", ylab = "Final.Weight..lbs.", pch = 19, col = "black")
abline(lm(data$Final.Weight..lbs. ~ data$BMR..Calories.), col = "red")  # Add regression line

# 8. Multiple Regression
# Fit a linear regression model predicting Final.Weight..lbs. using BMR..Calories. and  Stress.Level
model <- lm(Final.Weight..lbs. ~ Current.Weight..lbs. +  Stress.Level, data = data)
print(summary(model))

# 9. Model Diagnostics
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

# 10. Principal Component Analysis (PCA)
# Perform PCA on numerical variables only
numerical_data <- data[, c("Current.Weight..lbs.", "BMR..Calories.", "Daily.Calories.Consumed", "Daily.Caloric.Surplus.Deficit", "Weight.Change..lbs.", "Stress.Level", "Final.Weight..lbs.")]
pca <- prcomp(numerical_data, scale. = TRUE)
summary(pca)

# 11. PCA Interpretation
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
biplot(pca, main = "PCA Biplot of Weights Data", cex=0.7, xlabs=rep("", nrow(data)), xlim=c(-0.4,0.4), ylim=c(-0.2, 0.2))
