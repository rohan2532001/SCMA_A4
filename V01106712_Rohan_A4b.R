# Install factoextra package
install.packages("factoextra")

# Install other necessary packages if not already installed
install.packages("tidyverse")
install.packages("cluster")
install.packages("Rtsne")
# Load necessary libraries
library(tidyverse)
library(cluster)
library(factoextra)
library(Rtsne)

# Load the dataset
survey_data <- read.csv("C:\\Users\\HP\\Downloads\\Survey (1).csv")

# Inspect the data
head(survey_data)
summary(survey_data)

# Convert necessary columns to numeric (if not already)
# We will use only numeric columns for clustering
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

# Elbow Method to determine the optimal number of clusters
fviz_nbclust(survey_data_scaled, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2) +  # Replace 3 with the number of clusters identified
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(survey_data_scaled, kmeans, method = "silhouette") +
  labs(subtitle = "Silhouette method")

# Perform K-means clustering with the optimal number of clusters
set.seed(123)  # For reproducibility
num_clusters <- 3  # Replace with the optimal number determined
kmeans_result <- kmeans(survey_data_scaled, centers = num_clusters, nstart = 25)

# Add cluster assignments to the original data
survey_data$Cluster <- as.factor(kmeans_result$cluster)

# Summary of cluster assignments
table(survey_data$Cluster)

# Visualize the clusters using PCA
fviz_cluster(kmeans_result, data = survey_data_scaled, geom = "point", ellipse.type = "norm")

# Alternatively, using t-SNE
tsne_result <- Rtsne(survey_data_scaled, dims = 2, perplexity = 30)
tsne_data <- data.frame(tsne_result$Y, Cluster = survey_data$Cluster)
ggplot(tsne_data, aes(x = X1, y = X2, color = Cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "t-SNE visualization of clusters")
