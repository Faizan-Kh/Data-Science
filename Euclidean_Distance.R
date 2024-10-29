# Load the BOD dataset and save it to a CSV
write.csv(BOD, "BOD_Dataset.csv", row.names = FALSE)

# Read the dataset back into R
BOD_data <- read.csv("BOD_Dataset.csv")

# Define a generalized Minkowski distance function
minkowski_dist <- function(x, y, r) {
  if (r == Inf) {
    return(max(abs(x - y)))  # L∞ norm (Supremum distance)
  } else {
    return((sum(abs(x - y)^r))^(1/r))  # Lr norm (Minkowski distance)
  }
}

# Get the number of demands
n <- length(BOD_data$demand)

# Initialize matrices to hold the distances
manhattan_distances <- matrix(NA, n, n) 
euclidean_distances <- matrix(NA, n, n)
supremum_distances <- matrix(NA, n, n)

# Set values for r
r_values <- c(1, 2, Inf)  # 1 = Manhattan, 2 = Euclidean, Inf = Supremum

# Calculate the distances for each pair
for (i in 1:n) {
  for (j in 1:n) {
    manhattan_distances[i, j] <- minkowski_dist(BOD_data$demand[i], BOD_data$demand[j], r_values[1])
    euclidean_distances[i, j] <- minkowski_dist(BOD_data$demand[i], BOD_data$demand[j], r_values[2])
    supremum_distances[i, j] <- minkowski_dist(BOD_data$demand[i], BOD_data$demand[j], r_values[3])
  }
}

# Print the distance matrices
cat("Manhattan Distance Matrix (r=1):\n")
print(manhattan_distances)
cat("\nEuclidean Distance Matrix (r=2):\n")
print(euclidean_distances)
cat("\nSupremum Distance Matrix (r=∞):\n")
print(supremum_distances)
