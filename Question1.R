# To install the necessary package, uncomment the next line
# install.packages("ggplot2")

# Importing library and reading data
library(ggplot2)
set.seed(0)
data = read.delim("cholesterol.txt", sep=' ')

################## QUESTION 1A #####################

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


##################### QUESTION 1B ########################

# H0: The margarine diet has no effect, i.e. the mean cholesterol levels Before and After 8 weeks are the same
# H1: The margarine diet reduces cholesterol levels --> mean_before > mean_after
# Paired t-test
# Assumption: the differences between Before and After should be normally distributed
# Outcome: if p < 0.05, reject H0
t.test(data$Before, data$After8weeks, paired = TRUE, alternative = "greater")

# Visualizing distribution of the differences
difference = data$Before - data$After8weeks

hist(difference, probability = TRUE,
     main = 'Data distribution of differences between Before and After8weeks',
     col = 'lightblue',
     xlab = 'Difference (Before - After8weeks)',
     ylab = 'Proportion')
lines(density(difference), col = 'red', lwd = 2)

qqnorm(difference,
       main = 'QQ-plot for differences',
       xlab = 'Theoretical Quantiles',
       ylab = 'Sample Quantiles')
qqline(difference, col = "red")

# Testing normality of the data
# If p > 0.05 --> fail to reject normality (data is normal)
shapiro.test(difference)

##################### QUESTION 1C ########################
# calculating 97% CI for mu using t-score
n = length(data$After8weeks)
sample_mean = mean(data$After8weeks)
sample_sd = sd(data$After8weeks)
critical_value = qt(1-0.015, df=17)
standard_error = sample_sd / sqrt(n)

left_bound = sample_mean - critical_value * standard_error
right_bound = sample_mean + critical_value * standard_error

cat("97% Confidence Interval for mu: [", left_bound, ",", right_bound, "]\n")

# calculating 97% CI for mu with bootstrapping
bootstrap_ci = function(x, conf_level = 0.97, B = 10000) {
  alpha = 1 - conf_level
  Bstats = lapply(1:B, FUN = function(i) {
    boot_sample = sample(x, size = length(x), replace = TRUE)
    mean(boot_sample)
  } )
  Bstats = unlist(Bstats)
  quantile(Bstats, prob = c(alpha/2, 1-alpha/2))
}

set.seed(42)
bootstrap_ci(data$After8weeks)

##################### QUESTION 1D ########################




##################### QUESTION 1E ########################
median(data$After8weeks)
wilcox.test(data$After8weeks, mu = 6, alternative = "less")

# Count how many values in After8weeks are less than 4.5
count_below_4.5 = sum(data$After8weeks < 4.5)
percentage_below_4.5 = (count_below_4.5 / length(data$After8weeks)) * 100
cat("Percentage of cholesterol levels below 4.5:", percentage_below_4.5, "%\n")

# H0: The fraction of cholesterol levels below 4.5 is at most 25%
# H1: The fraction is greater than 25%
# if the p-value is small (<0.05), we reject H0 and conclude that the fraction is significantly greater than 25%.
binom.test(count_below_4.5, length(data$After8weeks), p = 0.25, alternative = "greater")

