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
# The yield is not taking into account due to possible unkown variance/ 
# dependence on factors.

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
# The yield is only (at this moment) possible to be collected from the dataset 
# and cannot be randomized due to unkown variance/ dependencies.

# Group npk data by block and N
grouped_data <- group_by(npk, block, N)

# Calculate the average yield for each group
average_yield_block = summarise(grouped_data, Average_Yield = mean(yield))
# average_yield_block

# Obtain data for each block where N=0 and N=1
absent_nitrogen <- filter(average_yield_block, N==0)
present_nitrogen <- filter(average_yield_block, N==1)

# Plot the average yield
avg_yield_plot <- ggplot(average_yield_block, aes(x = block, y = Average_Yield, fill = N)) + 
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Block Number", y = "Average Yield", fill = "Nitrogen") +
  ggtitle("Average Yield per Block, With and Without Nitrogen")

avg_yield_plot

# START OF 3.c

# ANOVA (two-way)
# Indpenedent factors: block and N
# response variable: yield
# perform test to check normality

# Interpreting the table
# Df = degrees of freedom
# sum of squares = total var from blocks
# mean sq = sum of squares for block/degrees of freedom
# F value = ratio of variation from block to residual
  # 3.395 times larger than residuals
#Pr(>F) = p-value for if block significant effect on yield (response variable)
  # Less than 0.05 so statistically significant (5% level)

# F value N: larger effect on yield compared to block
# P-value of N is highly significant (1% level)

# residuals sum sq represents variation, unexplainable by block or N

# Both N and p have statistically significant effect, but Nitrogen has a higher 
# effect on yield.

two_way_anova <- aov(yield ~ block + N, data = npk)
summary(two_way_anova)



