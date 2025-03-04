# Load necessary libraries
library(MASS)

# Read and display data from coup.txt
data <- read.table("coups.txt", header = TRUE, row.names = 1)
data

str(data)

# To access full dataset use .
#miltcoup is response var
# Use glm since counting, lm not correct
poisson_reg <- glm (miltcoup ~ ., data = data, family = poisson)
summary(poisson_reg)

# Remove insignificant pieces one by one (summary)

poisson_reg1 <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote + popn + size + numregim, 
                    data = data,
                    family = poisson)
summary(poisson_reg1)        