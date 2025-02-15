# To access the correct file, import MASS
# install.packages('MASS')

# Set block and plot dimensions
blocks <- 1:6
plots <- 4

# Create all possible combinations
possible_plots <- expand.grid(N = c(0, 1), P = c(0, 1), K = c(0, 1))
possible_plots
