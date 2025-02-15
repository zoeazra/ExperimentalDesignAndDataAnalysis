# To access the correct file, import MASS
# install.packages('MASS')

# Set block and plot dimensions
n_blocks <- 1:6
n_plots <- 4
additives <- c("N", "P", "K")

# Create all possible combinations
possible_plots <- expand.grid(N = c(0, 1), P = c(0, 1), K = c(0, 1))
#possible_plots

# Randomly assign combinations
random_additives <- list()

for (block in n_blocks) {
  random_additives[[block]] <- sample(1:nrow(possible_plots), n_plots, replace = TRUE)
}

random_additives

# Less computational way to assign randomly but adhere to max 2

# Create empty 4x3 grid
for (block in n_blocks) {
  blocks <- data.frame(matrix(0, nrow = n_plots, ncol = length(additives)))
  colnames(blocks) <- additives
  
  # Assign 2x in each additive randomly
  for (additive in additives) {
    # Choose a random row
    row <- sample(1:n_plots, 2, replace = FALSE)
  }
}

blocks
row

