# Loading the iris dataset
data("iris")

# 1. Data Overview
# Display structure and dimensions of the dataset
str(iris)
cat("\nNumber of observations:", nrow(iris), "\n")
cat("Number of variables:", ncol(iris), "\n\n")

# 2. Summary Statistics for a numerical variable (e.g., Sepal.Length)
summary_stats <- function(var) {
  cat("Summary Statistics for", var, "\n")
  cat("Mean:", mean(iris[[var]], na.rm = TRUE), "\n")
  cat("Median:", median(iris[[var]], na.rm = TRUE), "\n")
  cat("Standard Deviation:", sd(iris[[var]], na.rm = TRUE), "\n")
  cat("Minimum:", min(iris[[var]], na.rm = TRUE), "\n")
  cat("Maximum:", max(iris[[var]], na.rm = TRUE), "\n")
}
summary_stats("Sepal.Length")  # Use Sepal.Length column for analysis

# 3. Distribution Visualization (e.g., Sepal.Length)
# Histogram for Sepal Length
par(mfrow = c(1, 1))
hist(iris$Sepal.Length, breaks = 5, col = "black", xlab = "Sepal Length", 
     main = "Histogram of Sepal Length")

# Boxplot for Sepal Length
par(mfrow = c(1, 1))
boxplot(iris$Sepal.Length, horizontal = TRUE, col = "green", main = "Boxplot of Sepal Length")

# 4. Categorical Variable Analysis (e.g., Species)
# Bar plot for Species (categorical variable)
par(mfrow = c(1, 1))
barplot(table(iris$Species), col = "black", main = "Bar Plot of Species", 
        xlab = "Species", ylab = "Count")

# 5. Correlation Analysis (e.g., Sepal.Length and Petal.Length)
correlation <- cor(iris$Sepal.Length, iris$Petal.Length, use = "complete.obs")
cat("\nPearson Correlation between Sepal Length and Petal Length:", correlation, "\n")

# 6. Scatter Plot Visualization
par(mfrow = c(1, 1))
plot(iris$Sepal.Length, iris$Petal.Length, main = "Scatter Plot of Sepal Length vs Petal Length",
     xlab = "Sepal Length", ylab = "Petal Length", pch = 19, col = "black")
abline(lm(iris$Petal.Length ~ iris$Sepal.Length), col = "red")  # Add regression line

# 7. Multiple Regression
# Fit a linear regression model predicting Petal Length using Sepal Length and Sepal Width
model <- lm(Petal.Length ~ Sepal.Length + Petal.Width, data = iris)
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
# Perform PCA on numerical variables only (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
numerical_data <- iris[, 1:4]
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
biplot(pca, main = "PCA Biplot of Iris Data", cex=0.7, xlabs=rep("", nrow(iris)), xlim=c(-0.2,0.4), ylim=c(-0.2, 0.1))