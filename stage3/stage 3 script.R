# Load necessary library
library(tidyverse)

# URL of the dataset
url <- "https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-in-Biotechnology-and-Life-Sciences/refs/heads/main/datasets/dataset_wisc_sd.csv"

# Read the dataset directly from the URL
cancer_data <- read.csv(url)

# View the first few rows
head(cancer_data)

# Check structure
str(cancer_data)

# Check for missing values
sum(is.na(cancer_data))

# Remove rows with missing values
cancer_data_clean <- na.omit(cancer_data)

# Check for which columns have missing values
colSums(is.na(cancer_data_clean))

# Remove 'id' column if it exists
cancer_data_clean <- cancer_data_clean %>% select(-id)

# Convert diagnosis to factor
cancer_data_clean$diagnosis <- as.factor(cancer_data_clean$diagnosis)

# Extract numeric features only
features <- cancer_data_clean %>% select(-diagnosis)

# Standardize the data (important for PCA)
features_scaled <- scale(features)

# Apply PCA
pca_result <- prcomp(features_scaled, center = TRUE, scale. = TRUE)

# Scree plot to visualize variance explained
library(factoextra)
fviz_eig(pca_result)

# Convert PCA results into a dataframe
pca_df <- as.data.frame(pca_result$x[, 1:2]) # Keep first 2 PCs
pca_df$diagnosis <- cancer_data_clean$diagnosis  # Add back diagnosis labels

# Visualize the Elbow Method to determine the optimal number of clusters
fviz_nbclust(features_scaled, kmeans, method = "wss")

# Apply K-Means clustering with 2 clusters (k=2)
set.seed(42)
kmeans_result <- kmeans(features_scaled, centers = 2, nstart = 25)

# Visualize the results
pca_df$Cluster <- as.factor(kmeans_result$cluster)
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering on PCA-transformed Data (k=2)") +
  theme_minimal()

# Create a contingency table to compare clusters and actual diagnosis
table(pca_df$Cluster, pca_df$diagnosis)

# Save PCA results (first two principal components)
write.csv(pca_df, "pca_results.csv", row.names = FALSE)

# Save K-Means clustering results
write.csv(pca_df, "kmeans_clustering_results.csv", row.names = FALSE)








