# Load the BOD dataset
BOD_data <- read.csv("BOD_Dataset.csv")

# Extract the Time and Demand columns
time <- BOD_data$Time
demand <- BOD_data$demand

# Calculate means
mean_time <- mean(time)
mean_demand <- mean(demand)

# Calculate covariance
covariance <- sum((time - mean_time) * (demand - mean_demand)) / length(time)

# Calculate standard deviations
std_dev_time <- sqrt(sum((time - mean_time)^2) / length(time))
std_dev_demand <- sqrt(sum((demand - mean_demand)^2) / length(demand))

# Calculate Pearson correlation coefficient
correlation_coefficient <- covariance / (std_dev_time * std_dev_demand)

# Print results
cat("Mean Time:", mean_time, "\n")
cat("Mean Demand:", mean_demand, "\n")
cat("Covariance:", covariance, "\n")
cat("Standard Deviation of Time:", std_dev_time, "\n")
cat("Standard Deviation of Demand:", std_dev_demand, "\n")
cat("Pearson Correlation Coefficient:", correlation_coefficient, "\n")
