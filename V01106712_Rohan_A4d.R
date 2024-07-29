# Install necessary packages
install.packages("conjoint")
install.packages("dplyr")

# Load the packages
library(conjoint)
library(dplyr)
# Load the data
data <- read.csv("C:\\Users\\HP\\Downloads\\pizza_data (1).csv")

# View the first few rows of the data
head(data)
# Example: Assuming data contains columns 'respondent_id', 'attribute_1', 'attribute_2', ..., 'rating'
conjoint_data <- data %>%
  select(respondent_id, attribute_1, attribute_2, attribute_3, rating) # Adjust column names as needed
# Define the levels for each attribute
levels_attribute_1 <- c("Level 1", "Level 2", "Level 3") # Replace with actual levels
levels_attribute_2 <- c("Level A", "Level B")            # Replace with actual levels
# Continue defining for all attributes
# Define the design matrix
design_matrix <- expand.grid(levels_attribute_1, levels_attribute_2) # Continue for all attributes

# Convert to data frame
design_matrix <- data.frame(design_matrix)

# Perform the conjoint analysis
conjoint_results <- caModel(data = conjoint_data, 
                            design.matrix = design_matrix, 
                            nfactor = length(levels_attribute_1) + length(levels_attribute_2) - 1) # Adjust nfactor as needed
# Print the conjoint analysis results
print(conjoint_results)

# Plot importance scores
caImportance(conjoint_results)
