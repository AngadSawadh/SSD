# 1. Load your CSV file
data <- read.csv("./data/cars.csv")  # Replace with the actual path to your CSV file

# 2. Data Overview
# Display structure and dimensions of the dataset
str(data)
cat("\nNumber of observations:", nrow(data), "\n")
cat("Number of variables:", ncol(data), "\n\n")

# 3. Summary Statistics for a numerical variable (e.g., Price)
summary_stats <- function(var) {
  cat("Summary Statistics for", var, "\n")
  cat("Mean:", mean(data[[var]], na.rm = TRUE), "\n")
  cat("Median:", median(data[[var]], na.rm = TRUE), "\n")
  cat("Standard Deviation:", sd(data[[var]], na.rm = TRUE), "\n")
  cat("Minimum:", min(data[[var]], na.rm = TRUE), "\n")
  cat("Maximum:", max(data[[var]], na.rm = TRUE), "\n")
}
summary_stats("Price")  # Use Price column for analysis

# 4. Distribution Visualization (e.g., Price)
# Histogram for Price
par(mfrow = c(1, 1))
hist(data$Price/100000, breaks = 5, col = "black", xlab = "Price", 
     main = "Histogram of Price (in Lakhs)")

# Boxplot for Price
par(mfrow = c(1, 1))
boxplot(data$Price/100000, horizontal = TRUE, col = "green", main = "Boxplot of Price (in Lakhs)")

# 5. Categorical Variable Analysis (e.g., Owner_Type)
# Bar plot for Owner_Type (categorical variable)
par(mfrow = c(1, 1))
barplot(table(data$Owner_Type), col = "black", main = "Bar Plot of Owner_Type", 
        xlab = "Owner_Type", ylab = "Count")

# 6. Correlation Analysis (e.g., Engine and Price)
correlation <- cor(data$Engine, data$Price, use = "complete.obs")
cat("\nPearson Correlation between Engine and Price:", correlation, "\n")

# 7. Scatter Plot Visualization
par(mfrow = c(1, 1))
plot(data$Engine, data$Price, main = "Scatter Plot of Engine vs Price",
     xlab = "Engine", ylab = "Price", pch = 19, col = "black")
abline(lm(data$Price ~ data$Engine), col = "red")  # Add regression line

# 8. Multiple Regression
# Fit a linear regression model predicting Price using Engine and Mileage
model <- lm(Price ~ Engine + Mileage, data = data)
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

# Removing Ford Mustang
new_data = data[-3,]
impr_model <- lm(Price ~ Engine + Power, data = new_data)
print(summary(impr_model))

# Plot the residuals of the regression model
# Plot the residuals vs fitted values
par(mfrow = c(1, 1))  # Ensure only one plot is shown
plot(impr_model, which = 1, pch = 16)  # Residuals vs Fitted with solid dots

# Plot the Normal Q-Q plot
par(mfrow = c(1, 1))  # Ensure only one plot is shown
plot(impr_model, which = 2, pch = 16)  # Normal Q-Q with solid dots

# Plot the Scale-Location plot
par(mfrow = c(1, 1))  # Ensure only one plot is shown
plot(impr_model, which = 3, pch = 16)  # Scale-Location with solid dots

# Plot the Cook's distance plot
par(mfrow = c(1, 1))  # Ensure only one plot is shown
plot(impr_model, which = 4, pch = 16)  # Cook's distance with solid dots

# 10. Principal Component Analysis (PCA)
# Perform PCA on numerical variables only (Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
numerical_data <- new_data[, c("Kilometers_Driven", "Mileage", "Engine", "Power", "Price")]
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
biplot(pca, main = "PCA Biplot of Cars Data", cex=0.7, xlabs=rep("", nrow(new_data)), xlim=c(-0.4,0.4), ylim=c(-0.2, 0.2))
