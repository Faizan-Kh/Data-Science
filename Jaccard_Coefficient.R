threshold <- 15  # Example threshold
binary_demand <- ifelse(BOD$demand > threshold, 1, 0)


n <- length(binary_demand)

# Initialize counts for M11, M01, and M10
M_11 <- 0  
M_01 <- 0  
M_10 <- 0  

# Loop through each pair to count M11, M01, and M10
for (i in 1:n) {
  for (j in 1:n) {
    if (i != j) { 
      if (binary_demand[i] == 1 && binary_demand[j] == 1) {
        M_11 <- M_11 + 1
      } else if (binary_demand[i] == 0 && binary_demand[j] == 1) {
        M_01 <- M_01 + 1
      } else if (binary_demand[i] == 1 && binary_demand[j] == 0) {
        M_10 <- M_10 + 1
      }
    }
  }
}

# Calculate Jaccard Coefficient
jaccard_coefficient <- M_11 / (M_11 + M_01 + M_10)

print(jaccard_coefficient)