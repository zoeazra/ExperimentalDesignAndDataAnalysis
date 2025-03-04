# Load necessary libraries
library(MASS)

# Read and display data from coup.txt
data <- read.table("coups.txt", header = TRUE, row.names = 1)
data

str(data)