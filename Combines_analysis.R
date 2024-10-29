# Load the BOD dataset
write.csv(BOD, "BOD_Dataset.csv", row.names = FALSE)
BOD_data <- read.csv("BOD_Dataset.csv")

# Define a threshold for demand
threshold <- 5

# Create a new column for binarized demand
BOD_data$binarized_demand <- ifelse(BOD_data$demand > threshold, 1, 0)

# Plotting demand over time
library(ggplot2)

ggplot(BOD_data, aes(x = Time, y = demand)) +
  geom_line() +
  geom_point(aes(color = as.factor(binarized_demand)), size = 2) +
  labs(title = "BOD Demand Over Time",
       x = "Time (Days)",
       y = "Demand",
       color = "Binarized Demand") +
  theme_minimal()
