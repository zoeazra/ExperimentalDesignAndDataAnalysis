# Load necessary libraries
library(ggplot2)
library(dplyr)
library(car)  # For ANOVA
library(lmtest)  # For model comparison

# Load the dataset
crops_data <- read.table("crops.txt", header=TRUE)
View(crops_data)


