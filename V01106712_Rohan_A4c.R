# Load necessary libraries
library(MASS)  # For isoMDS function

# Read the data from CSV file
data <- read.csv("C:\\Users\\HP\\Downloads\\icecream (1).csv", header = TRUE)

# Calculate the distance matrix
# Assuming the data is in a suitable format for distance calculation
distance_matrix <- dist(data)

# Apply Classical MDS
mds_result <- cmdscale(distance_matrix, k = 2)  # k is the number of dimensions

# If data is not linear, apply non-metric MDS
# mds_result <- isoMDS(distance_matrix, k = 2)

# Plot the results
plot(mds_result, type = "n")  # type = "n" creates an empty plot
text(mds_result, labels = rownames(data), cex = 0.7)  # Add text labels

# Interpretation: The plot shows the relative positions of each point in reduced dimensions.
# Points that are closer together are more similar based on the distance metric used.
