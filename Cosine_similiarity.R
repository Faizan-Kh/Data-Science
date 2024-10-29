# Load the BOD dataset and save it to a CSV
write.csv(BOD, "BOD_Dataset.csv", row.names = FALSE)

#or load it instead
#load("BOD")

# Read the dataset back into R
BOD_data <- read.csv("BOD_Dataset.csv")

# Define a function to calculate cosine similarity
cosine_similarity <- function(x, y) {
  dot_product <- sum(x * y)
  magnitude_x <- sqrt(sum(x^2))
  magnitude_y <- sqrt(sum(y^2))
  
  # Calculate cosine similarity
  if (magnitude_x == 0 || magnitude_y == 0) {
    return(NA)  # Avoid division by zero
  } else {
    return(dot_product / (magnitude_x * magnitude_y))
  }
}

# Get the number of demands
n <- length(BOD_data$demand)

# Initialize a matrix to hold the cosine similarities
cosine_similarities <- matrix(NA, n, n)

# Calculate the cosine similarity for each pair
for (i in 1:n) {
  for (j in 1:n) {
    cosine_similarities[i, j] <- cosine_similarity(BOD_data$demand[i], BOD_data$demand[j])
  }
}

# Print the cosine similarity matrix
cat("Cosine Similarity Matrix:\n")
print(cosine_similarities)
