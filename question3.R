# To access the correct file, import MASS
# install.packages('MASS')
# install.packages('ggplot2')
# install.packages ('dplyr')

# Load the packages and dataset
library(MASS)
library(dplyr)
library(ggplot2)

data(npk)

# START OF 3.a

# Set block and plot dimensions
n_blocks <- 1:6
n_plots <- 4
additives <- c("N", "P", "K")

# Create a list to randomly distribute over blocks
random_distributed <- list()

# Create empty 4x3 (plots x additives) grid as data frame
for (block in n_blocks) {
  blocks <- data.frame(matrix(0, nrow = n_plots, ncol = length(additives)))
  colnames(blocks) <- additives
  
  # Assign 2x 1 in each additive randomly
  for (additive in additives) {
    # Choose a random row
    row <- sample(1:n_plots, 2, replace = FALSE)
    # Insert 1 to signify presence of additive for selected rows
    blocks[row, additive] <- 1
  }
  
  # Save and assign block data
  blocks$Block <- block
  random_distributed[[block]] <- blocks
  
}

random_distributed


# START OF 3.b

#Q: Make a plot to show the average yield per block for the soil treated with 
# nitrogen and for the soil that did not receive nitrogen, and comment.

#Pseudo 2: calc average for each
# Pseudo 3: create bar plot for each block with and without next to each other
# Yield on y, block on x

# Group npk data by block and N
grouped_data <- group_by(npk, block, N)

# Calculate the average yield for each group
average_yield_block = summarise(grouped_data, Average_Yield = mean(yield))
# average_yield_block

# Obtain data for each block where N=0 and N=1
absent_nitrogen <- filter(average_yield_block, N==0)
present_nitrogen <- filter(average_yield_block, N==1)

# Plot the average yield
avg_yield_plot <- ggplot(average_yield_block, aes(x = factor(block), y = Average_Yield, fill = factor(N)))
avg_yield_plot

