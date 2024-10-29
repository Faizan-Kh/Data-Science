# Load the BOD dataset
write.csv(BOD, "BOD_Dataset.csv", row.names = FALSE)
BOD_data <- read.csv("BOD_Dataset.csv")

# Define the Minkowski distance function
minkowski_distance <- function(p, q, r) {
  return(sum(abs(p - q)^r)^(1/r))
}

# Set the value of r (e.g., 1 for Manhattan, 2 for Euclidean)
r_value <- 2  # Change this value for different distances (1, 2, etc.)

# Number of observations
n <- nrow(BOD_data)

# Create a matrix to store distances
distances <- matrix(NA, n, n)

# Calculate Minkowski distance for all pairs
for (i in 1:n) {
  for (j in 1:n) {
    distances[i, j] <- minkowski_distance(c(BOD_data$Time[i], BOD_data$demand[i]), 
                                          c(BOD_data$Time[j], BOD_data$demand[j]), 
                                          r_value)
  }
}

# Print the distance matrix
print(distances)
