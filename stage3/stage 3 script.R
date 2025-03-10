# Load necessary libraries
library(tidyverse)
library(factoextra)

# URL of the dataset
url <- "https://raw.githubusercontent.com/PacktPublishing/Machine-Learning-in-Biotechnology-and-Life-Sciences/refs/heads/main/datasets/dataset_wisc_sd.csv"

# Read the dataset directly from the URL
cancer_data <- read.csv(url)

# Remove 'id' column if it exists and ensure diagnosis is a factor
cancer_data <- cancer_data %>% select(-id)
cancer_data$diagnosis <- as.factor(cancer_data$diagnosis)

# Check for missing values
if (sum(is.na(cancer_data)) > 0) {
  cat("Missing values detected. Removing rows with missing values.\n")
  cancer_data_clean <- na.omit(cancer_data)
} else {
  cancer_data_clean <- cancer_data
}

# Extract numeric features for PCA and K-Means
features <- cancer_data_clean %>% select(-diagnosis)

# Check column types in the dataset
sapply(features, class)

# Remove the non-numeric column 'concave.points_worst'
features_clean <- features %>% select(-concave.points_worst)

# Scale the remaining numeric data
features_scaled <- scale(features_clean)

# Check for missing values
sum(is.na(features_scaled))

# Check for infinite values
sum(is.infinite(features_scaled))

# Remove rows with missing or infinite values
features_scaled_clean <- features_scaled[complete.cases(features_scaled) & !apply(features_scaled, 1, function(x) any(is.infinite(x))), ]

# If there are non-numeric columns, remove them or convert them to numeric
# For example, let's convert any non-numeric columns to numeric (if appropriate):
features <- features %>% mutate(across(where(is.character), as.factor))

# Now, let's convert any factors to numeric by applying as.numeric() (if needed)
features <- features %>% mutate(across(where(is.factor), ~ as.numeric(as.factor(.))))

# Check the structure again to ensure all columns are numeric
str(features)



# Standardize the features (important for PCA)
features_scaled <- scale(features)

# Apply PCA
pca_result <- prcomp(features_scaled, center = TRUE, scale. = TRUE)

# Visualizing the Scree Plot (variance explained by each component)
fviz_eig(pca_result)

# Convert PCA results into a dataframe for the first 2 principal components
pca_df <- as.data.frame(pca_result$x[, 1:2]) # Keep first 2 PCs
pca_df$diagnosis <- cancer_data_clean$diagnosis  # Add back diagnosis labels

# Visualize the first two principal components, color-coded by diagnosis
ggplot(pca_df, aes(x = PC1, y = PC2, color = diagnosis)) +
  geom_point(size = 3) +
  labs(title = "PCA on Cancer Dataset") +
  theme_minimal()

# Determine the optimal number of clusters using the Elbow Method
fviz_nbclust(features_scaled, kmeans, method = "wss")

# Apply K-Means clustering with 2 clusters (k=2)
set.seed(42)
kmeans_result <- kmeans(features_scaled, centers = 2, nstart = 25)

# Visualize the K-Means clustering result on PCA-transformed data
pca_df$Cluster <- as.factor(kmeans_result$cluster)
ggplot(pca_df, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 3) +
  labs(title = "K-Means Clustering on PCA-transformed Data (k=2)") +
  theme_minimal()

# Create a contingency table to compare clusters and actual diagnosis
cat("\nContingency Table: Comparison between Clusters and Actual Diagnosis\n")
cluster_diagnosis_table <- table(pca_df$Cluster, pca_df$diagnosis)
print(cluster_diagnosis_table)

# Check the clustering accuracy (if required)
accuracy <- sum(cluster_diagnosis_table[1,1], cluster_diagnosis_table[2,2]) / sum(cluster_diagnosis_table)
cat("\nClustering accuracy: ", accuracy, "\n")
