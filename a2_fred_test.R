# Load necessary libraries
library(MASS)

# Read and display data from coup.txt
data <- read.table("coups.txt", header = TRUE, row.names = 1)
names(data)

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

poisson_reg2 <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote + popn + numregim, 
                    data = data,
                    family = poisson)
summary(poisson_reg2)  

poisson_reg3 <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote + popn, 
                    data = data,
                    family = poisson)
summary(poisson_reg3)  

poisson_reg4 <- glm(miltcoup ~ oligarchy + pollib + parties + pctvote, 
                    data = data,
                    family = poisson)
summary(poisson_reg4)


# Everything p < 0.05
poisson_reg5 <- glm(miltcoup ~ oligarchy + pollib + parties, 
                    data = data,
                    family = poisson)
summary(poisson_reg5)


# Get all columns of which to take average
cols <- data[, setdiff(names(data), c("miltcoup", "pollib"))]

# Take means
cols_means <- colMeans(cols)
cols_means

# store the data
data2 <- data.frame(pollib = c(0, 1, 2), t(cols_means))
data2

# Make prediction from data2 and last model
predicted_coups <- predict(poisson_reg5, newdata = data2, type = 'response')
predicted_coups

data.frame(pollib = c(0, 1, 2), predicted_coups = predicted_coups)

# Nodig om andere dingen nou ook te predicten of enkel coups en daarvan averages?
