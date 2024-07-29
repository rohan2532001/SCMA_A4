# Install devtools if not already installed
install.packages("devtools")

# Use devtools to install ggbiplot from GitHub
devtools::install_github("vqv/ggbiplot")
# Load necessary libraries
library(tidyverse)
library(psych)

# Load the dataset
survey_data <- read.csv("C:\\Users\\HP\\Downloads\\Survey (1).csv")

# Inspect the data
head(survey_data)
summary(survey_data)

# Convert necessary columns to numeric (if not already)
# We will use only numeric columns for PCA and Factor Analysis
numeric_columns <- sapply(survey_data, is.numeric)
survey_data_numeric <- survey_data[, numeric_columns]

# Handle missing values (remove rows with NAs or use imputation)
survey_data_numeric <- na.omit(survey_data_numeric)

# Identify and remove constant columns (zero variance)
constant_columns <- sapply(survey_data_numeric, function(x) sd(x, na.rm = TRUE) == 0)
survey_data_numeric <- survey_data_numeric[, !constant_columns]

# Check dimensions of the remaining data
if (ncol(survey_data_numeric) == 0) {
  stop("No valid columns left for analysis after removing constant and all-NA columns.")
}

# Standardize the data
survey_data_scaled <- scale(survey_data_numeric)

# Perform PCA
pca_result <- prcomp(survey_data_scaled, center = TRUE, scale. = TRUE)

# Summary of PCA results
summary(pca_result)

# Base R plot for PCA
plot(pca_result$x[,1], pca_result$x[,2], 
     xlab = paste("PC1 -", round(summary(pca_result)$importance[2,1] * 100, 1), "%"),
     ylab = paste("PC2 -", round(summary(pca_result)$importance[2,2] * 100, 1), "%"),
     main = "PCA - Principal Component Analysis",
     pch = 19, col = "blue")

# Add labels to the points
text(pca_result$x[,1], pca_result$x[,2], labels = rownames(survey_data_numeric), pos = 4, cex = 0.7)

# Determine the number of factors for Factor Analysis using Parallel Analysis
fa_parallel <- fa.parallel(survey_data_scaled, fa = "both", n.iter = 100)

# Perform Factor Analysis using the number of factors suggested by Parallel Analysis
fa_result <- fa(survey_data_scaled, nfactors = fa_parallel$nfact, rotate = "varimax")

# Print Factor Analysis results
print(fa_result)

# Factor loadings plot
fa.diagram(fa_result)
