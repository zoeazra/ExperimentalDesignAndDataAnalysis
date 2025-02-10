# To install the necessary package, uncomment the next line
# install.packages("ggplot2")

# Importing library and reading data
library(ggplot2)
set.seed(0)
data = read.delim("cholesterol.txt", sep=' ')

# Question 1A:

# A scatter plot for correlation between Before and After8weeks
plot(data$Before, data$After8weeks, 
     main = 'Regression for Before and After 8 weeks of margarine', 
     xlab='Before', 
     ylab='After')
abline(lm(After8weeks ~ Before, data = data), col = "red")

# Some QQ-plots to try out
#qplot(sample = Before, data = data, main = 'QQ-plot', xlab='theoretical', ylab = 'sample')
#qplot(sample = After8weeks, data = data, main = 'QQ-plot', xlab='theoretical', ylab = 'sample')

qqnorm(data$Before, main = "Q-Q Plot for Before")
qqline(data$Before, col = "red")

qqnorm(data$After8weeks, main = "Q-Q Plot for After 8 Weeks")
qqline(data$After8weeks, col = "red")

# Histograms to visualize distribution of Before and After8weeks
hist(data$Before, probability = TRUE, 
     main = 'Data distribution for Cholesterol level Before margarine',
     xlab = 'Cholesterol level (mmol/L)',
     ylab = 'Probability')
lines(density(data$Before), col = "red", lwd = 2)

hist(data$After8weeks, probability = TRUE, 
     main = 'Data distribution for Cholesterol level After margarine',
     xlab = 'Cholesterol level (mmol/L)',
     ylab = 'Probability')
lines(density(data$Before), col = "red", lwd = 2)

# Question 1B:
# If p > 0.05 --> fail to reject normality (data is normal)
shapiro.test(data$Before)
shapiro.test(data$After8weeks)



