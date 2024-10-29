# Load the BOD dataset and save it to a CSV
write.csv(BOD, "BOD_Dataset.csv", row.names = FALSE)

# Read the dataset back into R
BOD_data <- read.csv("BOD_Dataset.csv")

# Binarize the demand data based on a threshold
threshold <- 15
BOD_data$binarized_demand <- ifelse(BOD_data$demand > threshold, 1, 0)

# Define the distance functions
euclidean_dist <- function(x, y) {
  sqrt(sum((x - y) ^ 2))
}

minkowski_dist <- function(x, y, r) {
  sum(abs(x - y) ^ r)^(1/r)
}

cosine_similarity <- function(x, y) {
  dot_product <- sum(x * y)
  magnitude_x <- sqrt(sum(x^2))
  magnitude_y <- sqrt(sum(y^2))
  
  if (magnitude_x == 0 || magnitude_y == 0) {
    return(NA)  # Avoid division by zero
  } else {
    return(dot_product / (magnitude_x * magnitude_y))
  }
}

# Get the number of demands
n <- nrow(BOD_data)
demand_matrix <- as.matrix(BOD_data$demand)

# Calculate distance matrices
euclidean_matrix <- matrix(NA, n, n)
minkowski_matrix <- matrix(NA, n, n)
cosine_matrix <- matrix(NA, n, n)

# Calculate the distance matrices
for (i in 1:n) {
  for (j in 1:n) {
    # Using demand values for distance measures
    euclidean_matrix[i, j] <- euclidean_dist(demand_matrix[i, , drop = FALSE], demand_matrix[j, , drop = FALSE])
    minkowski_matrix[i, j] <- minkowski_dist(demand_matrix[i, , drop = FALSE], demand_matrix[j, , drop = FALSE], r = 3) # r can be 1, 2, or 3
    cosine_matrix[i, j] <- cosine_similarity(demand_matrix[i, , drop = FALSE], demand_matrix[j, , drop = FALSE])
  }
}

# Print the distance matrices
cat("Euclidean Distance Matrix:\n")
print(euclidean_matrix)
cat("\nMinkowski Distance Matrix:\n")
print(minkowski_matrix)
cat("\nCosine Similarity Matrix:\n")
print(cosine_matrix)

# Clustering using K-means based on Euclidean distance
set.seed(123) # For reproducibility
k <- 2 # Number of clusters
# Since K-means uses Euclidean distance, we will use the original demand matrix
kmeans_result <- kmeans(demand_matrix, centers = k)

# Add cluster assignment to the original dataset
BOD_data$cluster <- kmeans_result$cluster

# Print the clustering result
cat("\nK-means Clustering Results:\n")
print(BOD_data)
