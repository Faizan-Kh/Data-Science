threshold <- mean(BOD_data$demand)

#Converting the data to binary 
binary_demand <- ifelse(BOD_data$demand > threshold, 1, 0)

n <- length(binary_demand)
P_11 <- sum(binary_demand %*% t(binary_demand))  # Both 1s
P_00 <- sum((1 - binary_demand) %*% t(1 - binary_demand)) #Both 0s

#Total number of comparisons
N <- n * n

#Calculating Simple Matching Coefficients
SMC <- (P_11 + P_00) / N

print(SMC)