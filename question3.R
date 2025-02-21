# To access the correct file, import MASS
# install.packages('MASS')
# install.packages('ggplot2')
# install.packages ('dplyr')
# install.packages("lme4")

# Load the packages and dataset
library(MASS)
library(dplyr)
library(ggplot2)
library(lme4)

data(npk)


# START OF 3.a
# rewrite to output data similar to the npk set

# Set block and plot dimensions
n_blocks <- 6
n_plots <- 4

# Create a dataframe to randomly distribute over blocks
random_distributed <- data.frame(block = rep(n_blocks, each = n_plots),
                                 N = rep(0, n_plots*n_blocks),
                                 P = rep(0, n_plots*n_blocks),
                                 K = rep(0, n_plots*n_blocks))

# Iterate over blocks for index and sampling
for (block in 1:n_blocks) {
  
  idx <- (n_plots * (block-1) + 1): (n_plots * block)
  
  random_distributed[sample(idx, 2), "N"] <- 1
  random_distributed[sample(idx, 2), "P"] <- 1
  random_distributed[sample(idx, 2), "K"] <- 1
}

random_distributed


# START OF 3.b

# Group npk data by block and N
grouped_data <- group_by(npk, block, N)

# Calculate the average yield for each group
average_yield_block = summarise(grouped_data, Average_Yield = mean(yield))
average_yield_block

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

# Create Two-Way ANOVA for block and N without interaction and response variable yield
two_way_anova <- aov(yield ~ block + N, data = npk)
summary(two_way_anova)

# Note: Both N and p have statistically significant effect, but Nitrogen has a higher 
# effect on yield.
# NOTE: P-value of N on yield is highly significant (1% level)
# NOTE: p-value for effect block on yield is less than 0.05 so statistically significant (5% level)

# Friedman is useful when the hypothesis/assumptions of normality and homogeneity 
# are not met, double check

# Perform test to check normality
# Analysis of residuals (normality test)
qqnorm((residuals)(two_way_anova))
qqline(residuals(two_way_anova), col = 'red')

# Note: Points deviate slightly towards the tail

# Distribution in histogram (since slight deviation at tail)
hist(residuals(two_way_anova), main = "Histogram of Residuals", xlab = "Residuals", breaks = 10)

# Shapiro Wilk test (Since slight deviation at tail)
shapiro.test(residuals(two_way_anova))

# p-value 0.6514 ( greater than 0.05) so does not significantly deviate from normality
# Likely normally distributed

# Tests are met therefore Friedman not neccesary (expand upon)
# Fairly normally distributed, passes p-values, all Hypotheses are accepted


# START OF 3.d

# Effect of each on yield
model_1 <- aov(yield ~ N + P + K + block, data = npk)
summary(model_1)

# NOTE: N, K and block have a significant effect on yield
# NOTE: P has no significant effect so can be omitted

# Interactions between N, K and block
model_2 <- aov(yield ~ N * K + N * block + K * block, data = npk)
summary(model_2)

# NOTE: P could be ommitted due to statistical insignificance
model_3 <- aov(yield ~ N + K + block, data = npk)
summary(model_3)

# Check to see if no interaction between variables
interaction.plot(npk$N, npk$block, npk$yield)
interaction.plot(npk$block, npk$N, npk$yield)

# No interaction overall, possible minor interaction between block an N. 

# Unnecessary? May delete in final version
# Critical F-value
# qf(1 - 0.05, df1 = 1, df2 = 10)
# qf(1 - 0.05, df1 = 5, df2 = 10)


# START OF 3.e

# Unclear if mandatory
#npk$N <- factor(npk$N)
#npk$K <- factor(npk$K)
#npk$block <- factor(npk$block)

# Calculate the mean yield of all groups separated by pressence of factors
seperated_groups <- split(npk$yield, list(npk$N, npk$K, npk$block))
mean_yield <- sapply(seperated_groups, mean)

combination <- paste(npk$N, npk$K, npk$block)
yield_summary <- aggregate(yield ~ combination, data = npk, FUN = mean)

# Order from high to low yield
yield_summary <- yield_summary[order(-yield_summary$yield), ]
yield_summary

# Apply TukeyHSD 
tukey_results <- TukeyHSD(model_3)
tukey_results
summary(tukey_results)

# NOTE: From the TukeyHSD results we see that K has a significant negative impact on yield.
# Therefore the best combination for model 3 (where we do not consider P) is N = 1 and K = 0
# Block factor introduces variability with block 3 having the overall biggest positive impact and block 1 a relatively lower yield


# START OF 3e

# Create a Mixed effects model with all factors.
mixed_effects_model <- lmer(yield ~ N + P + K + (1 | block), data = npk)
summary(mixed_effects_model)

# NOTE: Similarly to the additive model, we can conclude that N has a significant (5.617)
# positive effect on yield. K has a statistically significant negative effect on yield (-3.983)
# And P has a non significant negative effect on yield. (-1.183)
# Blocks have significant variability in data, with random effects variance of 13.16.
# mixed effects model further solidifies the stance that N has a significant impact on yield.

# Further notes on the mixed effects model: better representation, blocks introduce 
# random variance and not fixed systemic effects such as N, P and K
