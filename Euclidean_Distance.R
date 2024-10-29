write.csv(BOD, "BOD_Dataset.csv", row.names = FALSE)
BOD_data <- read.csv("BOD_Dataset.csv")

euclidean_dist <- function(x, y) {
  sqrt((x - y)^2)
}

n <- length(BOD_data$demand)
distances <- matrix(NA, n, n) 

for (i in 1:n) {
  for (j in 1:n) {
    distances[i, j] <- euclidean_dist(BOD_data$demand[i], BOD_data$demand[j])
  }
}

print(distances)
