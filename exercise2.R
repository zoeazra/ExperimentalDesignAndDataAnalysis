# Load necessary libraries
library(ggplot2)
library(dplyr)
library(lme4)
library(car)  # For ANOVA
library(lmtest)  # For model comparison
library(rstatix)
library(standardize)
library(arm)
library(lm.beta)
library(emmeans)
library(ggplot2)
library(gridExtra)

# Load the dataset
crops_data <- read.table("crops.txt", header=TRUE)
View(crops_data)

#(a)
#Null hypotheses:
#H1: There is no significant difference in the mean Crops yield across different Counties. (the means of observations grouped by country are the same)
#H2: There is no significant difference in the mean Crops yield between cases where the landlord and tenant are related versus not related. (the means of observations grouped by related are the same)
#H3: There is no interaction effect between County and Related on Crops yield (there is no interaction between county and related)
crops_data$County <- as.factor(crops_data$County)
crops_data$Related <- as.factor(crops_data$Related)


#plot(Crops ~ County + factor(Related), data=crops_data)
#plot(Crops ~ Related + factor(County), data=crops_data)


#fit anova model
anova_model <- lm(Crops ~ County * Related, data = crops_data)
anova(anova_model)

# Extract residuals from the linear model
residuals <- residuals(anova_model)
#check if anova assumptions are met
#normality of residuals
# Extract residuals
residuals_anova <- residuals(anova_model)

# Shapiro-Wilk test (p > 0.05 means residuals are normal)
shapiro.test(residuals_anova)
round(shapiro.test(residuals_anova)$p.value, 3)
#W = 0.94137 This is the test statistic, which measures how closely the data follow a normal distribution. The closer this value is to 1, the more normal the residuals are.
#p-value = 0.09904: This is the probability of observing the data given that the null hypothesis is true.
#Since the p-value (0.09904) is greater than 0.05, we fail to reject the null hypothesis. This means the residuals do not significantly differ from a normal distribution
#the normality assumption holds because the p-value is greater than 0.05.

# QQ plot to visually inspect normality
par(mfrow=c(1,2))
qqnorm(residuals_anova)
qqline(residuals_anova, col = "red")
#Since the QQ plot follows almost a straight line, it further supports the conclusion from the Shapiro-Wilk test that the residuals are normally distributed.

hist(residuals_anova, main = "Histogram of Residuals", xlab = "Residuals", col = "lightblue", border = "black")

#levene's test for homogeneity of variances
str(crops_data)
crops_data$County <- as.factor(crops_data$County)
crops_data$Related <- as.factor(crops_data$Related)
crops_data$Crops <- as.numeric(crops_data$Crops)
crops_data$Size <- as.factor(crops_data$Size)
str(crops_data)
res <- car::leveneTest(Crops ~ interaction(County, Related), data = crops_data)
print(res)
#	F value = 0.2994:This is the test statistic, representing the ratio of between-group variance to within-group variance. A higher F value suggests larger differences between group variances.
# p-value = 0.9083: This is the probability of observing the data assuming that the variances across groups are equal. A p-value greater than 0.05 means we fail to reject the null hypothesis.
#Since the p-value (0.9083) is greater than 0.05, we fail to reject the null hypothesis. This means that the variances across the groups are homogeneous, and the assumption of equal variances is met.


#interpret anova if assumptions hold
summary(anova_model)


ggplot(crops_data, aes(x = County, y = Crops, color = Related, group = Related)) +
  stat_summary(fun = mean, geom = "point", size = 3) +
  stat_summary(fun = mean, geom = "line") +
  labs(title = "Interaction Plot: County & Related",
       x = "County", 
       y = "Mean Crops") +
  theme_minimal()
#for county:The p-value (0.477) is greater than 0.05, meaning we fail to reject the null hypothesis for County. This suggests that there is no significant effect of the County on the Crops variable.
#for related: The p-value (0.527) is also greater than 0.05, meaning we fail to reject the null hypothesis for Related. This suggests that there is no significant effect of whether the landlord and tenant are related on the Crops variable.
#for both: The p-value (0.879) is much greater than 0.05, meaning we fail to reject the null hypothesis for the interaction effect of County and Related. This suggests that there is no significant interaction between County and Related on the Crops variable.
#There is no significant effect of County, Related, or the interaction between them on the Crops variable. The p-values for all factors and their interaction are greater than 0.05.
#estimate crops for county 3 when landlord and tenant are not related

#plot analysis
#The interaction plot shows no crossing but non-parallel lines, suggesting a weak interaction that is not statistically significant (p = 0.879). This indicates that County and Related mostly act independently, 
#with variability likely masking any minor interaction.
predicted_value <- with(crops_data, mean(Crops[County == "3" & Related == "no"], na.rm=TRUE))
cat("Estimated crops for County 3 (Landlord and Tenant NOT related):", predicted_value, "\n")

#another way using estimated marginal means
# Compute estimated marginal means for County 3, Related = "no"
emm_results <- emmeans::emmeans(anova_model, ~ County * Related)
emm_summary <- as.data.frame(emm_results)
county3_not_related <- emm_summary[emm_summary$County == "3" & emm_summary$Related == "no", ]
print(county3_not_related)


TukeyHSD(anova_model)
#No significant differences were found in any of the comparisons between County, Related, or their interaction terms.
#The Tukey HSD results support the findings from the ANOVA, where the factors and interactions did not significantly influence the Crops variable.
#The factors County and Related (and their interactions) do not appear to have a meaningful impact on the crops in this dataset, according to both the ANOVA and Tukey HSD tests.



#(b)
#ensure correct formating
crops_data$County <- as.factor(crops_data$County)
crops_data$Related <- as.factor(crops_data$Related)
crops_data$Size <- as.numeric(crops_data$Size)  #covariate, so keep numeric
crops_data$Crops <- as.numeric(crops_data$Crops)

#define models
model_full <- lm(Crops ~ County + Related + Size + County:Size + Related:Size, data = crops_data)
model_county_size <- lm(Crops ~ County + Related + Size + County:Size, data = crops_data)
model_related_size <- lm(Crops ~ County + Related + Size + Related:Size, data = crops_data)
model_simplest <- lm(Crops ~ County + Related + Size, data = crops_data)

#check assumptions for the full model
shapiro.test(residuals(model_full))  # Normality check
qqnorm(residuals(model_full)); qqline(residuals(model_full), col = "red") 
car::leveneTest(Crops ~ County * Related * Size, data = crops_data)   # Homogeneity of variance
#everything almost straight line same as before
#Shapiro-Wilk normality test:The p-value = 0.4924, which is greater than 0.05. This indicates that we fail to reject the null hypothesis, meaning the residuals do not significantly differ from a normal distribution, so the normality assumption is met.
#Levene's Test for Homogeneity of Variance: The p-value = 0.9083, which is also greater than 0.05. This suggests that the variances across groups are equal, and the homogeneity of variance assumption is met.
                     
#compare models using backwards selection
#If p-value > 0.05, we drop Related:Size (no significant effect).
anova(model_full, model_county_size)
#Since the p-value (0.2431) is greater than 0.05, there is no significant difference between Model 1 and Model 2. This suggests that the interaction term Related:Size is not necessary in the model, and you can proceed with Model 2 as the preferred model.
#If p-value > 0.05, we drop County:Size (no significant effect).
anova(model_county_size, model_related_size)
#The p-value for the comparison is 0.005747, which is less than 0.05. This suggests that Model 1 (including the interaction term County:Size) is significantly better than Model 2 (including Related:Size). Thus, Related:Size is not necessary, and County:Size should be kept in the model.
#If p-value > 0.05, we drop both interactions and keep only main effects.
anova(model_county_size, model_simplest)
#Since the p-value (0.0179) is less than 0.05, this suggests that Model 1 (with the interaction terms) is significantly better than Model 2. The inclusion of the interaction term (County:Size) provides a better fit for the data, and the model is significant.


#choose the best model
best_model <- model_county_size

#check assumptions still hold for the best model
shapiro.test(residuals(best_model))  # Normality check
qqnorm(residuals(best_model)); qqline(residuals(best_model), col = "red") 
car::leveneTest(Crops ~ interaction(County, Related), data = crops_data)  # Homogeneity of variance

summary(best_model)

#visualizations
interaction.plot(crops_data$County, crops_data$Size, crops_data$Crops)
plot(best_model, which = 2)
plot(best_model, which = 1)
pred <- predict(best_model)
ggplot(crops_data, aes(x = pred, y = Crops)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Predicted vs Actual Crops", x = "Predicted Crops", y = "Actual Crops") +
  theme_minimal()
ggplot(crops_data, aes(x = County, y = Crops, fill = Size)) +
  geom_boxplot() +
  labs(title = "Crop Distribution by County and Size") +
  theme_minimal()


#(c)

#computer standardized coefficients
standardized_model <- lm.beta::lm.beta(best_model)
summary(standardized_model)
#Size has a high standardized coefficient (0.6303), suggesting it’s one of the most influential predictors.
#The interaction term County2:Size has an even higher standardized coefficient (0.9668), indicating a strong effect when Size interacts with County2.
#County3:Size has a lower standardized coefficient (0.4120), which might suggest a weaker interaction effect
#The Related variable has a very small standardized coefficient (-0.0532), implying little influence on Crops compared to other variables.

#boxplot for county and related
ggplot(crops_data, aes(x = County, y = Crops, fill = Related)) +
  geom_boxplot() +
  labs(title = "Crop Yield by County and Related Status") +
  theme_minimal()
#This boxplot shows the distribution of crop yield for each county, separated by whether farms are “related” (yes/no).
#In County 1, the median crop yield is similar for both “related” and “not related” farms.
#In County 2, related farms appear to have a lower median yield and a wider spread in crop yield values.
#In County 3, related farms seem to have a higher variance in crop yields, with some outliers.
#This suggests that the impact of the “related” factor on crop yield is not consistent across all counties.
#County differences: Crop yield varies between counties, suggesting that location may influence production.
#Effect of “Related” Status: The effect of being related or not is inconsistent across counties—sometimes it’s beneficial (County 1), sometimes not (County 2), and sometimes unclear (County 3).

#scatter plot for size
ggplot(crops_data, aes(x = Size, y = Crops, color = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Effect of Farm Size on Crop Yield by County") +
  theme_minimal()
#The slopes of the lines differ across counties, indicating that the effect of farm size on crop yield varies by county.
#County 2 (green) has the steepest slope, suggesting that crop yield increases more sharply with farm size compared to the other counties.

#interaction plot county::size
interaction.plot(crops_data$County, crops_data$Size, crops_data$Crops, col = rainbow(length(unique(crops_data$County))))

#Overall, the first and third plots suggest an interaction between county and farm size, which supports including a County × Size interaction term in our model. The second plot shows that “Related” may not have a strong effect,

#anova to see which variable contributes the most
anova(best_model)
#The factor “County” is statistically significant with a p-value of 0.01558, which is less than 0.05. This indicates that there are differences in crop yields between counties.
#he factor “Related” has a p-value of 0.11405, which is greater than 0.05, indicating that “Related” does not have a significant effect on crop yields.
#The factor “Size” is highly significant, with a very small p-value (8.718e-11), suggesting that it has a substantial impact on crop yields.
#The interaction term “County:Size” is also statistically significant (p-value = 0.01192), indicating that the effect of Size on crop yields varies by County. 

#(d)

#create a new data frame with the specified farm characteristics
new_farm <- data.frame(County = factor(2, levels = levels(crops_data$County)), 
                       Size = 165,
                       Related = factor("yes", levels = levels(crops_data$Related)))

# predict the crop yield using the best model from (b)
predicted_crop_yield <- predict(best_model, newdata = new_farm, interval = "confidence")

#print predicted crop yield and error variance (standard error of the prediction)
predicted_crop_yield
#he predicted crop yield for a farm from County 2, with a size of 165 and a related landlord and tenant, is 6141.30.
#The confidence interval for this prediction is: Lower bound: 5428 and	Upper bound: 6854.61

#compute error variance
error_variance <- summary(best_model)$sigma^2
print(error_variance)
#= 501961

#extract the residual standard error
sigma <- summary(best_model)$sigma

#compute the covariance of the model coefficients
cov_matrix <- vcov(best_model)

#create the design matrix for the new farm data (same format as your training data)
new_farm_matrix <- model.matrix(~ County + Size + Related, data = new_farm)

new_farm_matrix <- new_farm_matrix[1, , drop = FALSE]  # select only the first row for the new data
# Check the structure of the covariance matrix and model coefficients
print(dim(cov_matrix))  # Should match the number of coefficients
print(colnames(new_farm_matrix))  # Check if the columns align with the model coefficients
#compute the prediction error variance
error_variance <- sigma^2 + t(new_farm_matrix) %*% cov_matrix %*% new_farm_matrix

# The square root of the error variance gives the prediction standard error (SE)
prediction_se <- sqrt(error_variance)

# Display the prediction standard error
prediction_se

