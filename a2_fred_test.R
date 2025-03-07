# Load necessary libraries
library(MASS)

# Read and display data from coup.txt
data <- read.table("coups.txt", header = TRUE, row.names = 1)
names(data)

# To access full dataset use .
#miltcoup is response var
# Use glm since counting, lm not correct
#pollib is nmot linear maybe so do the following to transform into a factor
data$pollib <- as.factor(data$pollib)
poisson_reg <- glm (miltcoup ~ ., data = data, family = poisson)
summary(poisson_reg)

# We determine that variables for which p_value > 0.05 are not statistically significant, meaning we determine 
#taht they do not have a significant effect on our response variable, in this case 'miltcoup' which signifies the number of military coups. 
# We canb therefore state, that based on our data that the following variables have a significant effect: oligarchy, pollib and parties.

# We can interpret the data further by looking at the sign of the coefficient. This details that 
# an increase oligarchy, and parties, increases miltcoup. While in increase in pollib indicate a decrease in miltcoup.
# we also see that pollib1 is slightly significant (0.05 < p < 0.1) whilst pollib2 is quite significant with a negative effect, meaning that a higher degree of political 
#liberization experrience a lower amount of military coups.
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
