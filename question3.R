# To access the correct file, import MASS
# install.packages('MASS')

# Set block and plot dimensions
blocks <- 1:6
plots <- 4
additives <- 3

# Create all possible combinations
possible_plots <- expand.grid(N = c(0, 1), P = c(0, 1), K = c(0, 1))
#possible_plots

# Randomly assign combinations
random_additives <- list()

for (block in blocks) {
  random_additives[[block]] <- sample(1:nrow(possible_plots), plots, replace = TRUE)
}

random_additives

# Less computational way to assign randomly but adhere to max 2

# Create empty 4x3 grid
for (block in blocks) {
  block_grid <- matrix(0, nrow = plots, ncol = additives)
}

block_grid

