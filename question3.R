# To access the correct file, import MASS
# install.packages('MASS')

# Set block and plot dimensions
n_blocks <- 1:6
n_plots <- 4
additives <- c("N", "P", "K")

# Create a list to randomly distribute over blocks
random_distributed <- list()

# Create empty 4x3 grid as df
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
