########################################
####### REGRESSION ANALYIS #############
########################################

## Clearing the memory data for a fresh start
rm(list = ls())

## Installing packages
#install.packages("ggplot2")
#install.packages("dplyr")
install.packages("qqplotr")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("RVAideMemoire")

## Importing libraries
#library("readxl")
library(readxl)
library(ggplot2)
library(dplyr)
library(qqplotr)
library(tidyverse)
library(corrplot)
library(RVAideMemoire)
library(stats)
library(car)

## Loading Dataset
migration_index_data = read_excel("migration4regression_data.xlsx",
                               sheet = "Reformatted_migration_data")

View(migration_index_data)
names(migration_index_data)
colnames(migration_index_data)
head(migration_index_data)
tail(migration_index_data)

# Accessing if there are null rows
migration_index_data[(is.na(migration_index_data$`Country Name`)),]

View(migration_index_data)

## Data Summary
str(migration_index_data)

# Renaming long column names
colnames(migration_index_data)[4:7] = c('GDP_per_capita', 'Annual_inflation', 'mortality_rate', 'tuberculosis_detect_rate')
View(migration_index_data)

migration_reduced = migration_index_data[, 4:8]
View(migration_reduced)
summary(migration_reduced)

## Accessing correlation among variables
data_corr = cor(migration_reduced[, c(5, 1,2,3,4)], method = 'pearson')
View(data_corr)

## Visualizing the correlation
corrplot(data_corr, type = 'upper')

# OBSERVATION:
# From the correlation plot, 'Migration_index' is strongly correlated with
# 'mortality_rate', 'GDP_per_capita' and 'tuberculosis_detect_rate' in that order

###########################################################
## REGRESSION ANALYSIS USING "FORWARD STEPWISE" STRATEGY ##
###########################################################

## 1. SIMPLE LINEAR REGRESSION
##############################
#    Using ORDINARY LEAST SQUARES REGRESSION, both of our Dependent and Independent
#    Variables being numerical and continuous

## 1 a. Using only 'mortality_rate' to regress 'Migration_index'

# => Step1: Dataset Loading: ... already loaded

# => Step2: Objective:
#       I want to examine the possible linear relation between 'mortality_rate'
#        and 'Migration_index'

# => Step3: Linear Regression Analysis:

model_1 <- lm(formula = Migration_Index ~ mortality_rate, data = migration_reduced)
summary.lm(model_1)

# FINDINGS:
# - Both Intercept and mortality_rate are significant and the SLR equation is
# - Migration_Index = 12.53547 - 0.66333 x mortality_rate
# - R2 = 0.79, i.e. mortality_rate explained 79% of the entire variation in Migration_Index

# PLOTTING
plot(formula=Migration_Index ~ mortality_rate, data = migration_reduced, col='blue',
     main='Regression: Migration Index Vs Mortality Rate',
     xlab='Mortality Rate', ylab='Migration Index')
abline(model_1, col='red')

# => Step4: Ensuring Fitted model meets SLR assumptions
# 1. Linearity: The scatter plot proves there is a linear relationship: Passed
# 2. Residual's Independence

plot(model_1, 1)

# The correlation of residuals vs fit is approximately 0, no pattern observed: Passed

# 3. Normality of Residuals

plot(model_1, 2)

# Near linear observations, residuals is normally distributed: Passed

# 4. Equal Variances of Residuals (Homoscedasticity)

plot(model_1, 3)

# Variance of residuals are constant and not related to the fitted line. No clear
# pattern among residuals, randomly scattered residuals: Passed

# Step5: Reporting

# All 4 assumptions were approved, therefore we can confirm that our fitted
# regression line is indeed:
# Migration_Index = 12.53547 - 0.66333 x mortality_rate
# Example: Given, mortality_rate of 23,
# Migration_Index = 12.53547 - 0.66333 x 23.0 = -2.72112


## 2. MULTIPLE LINEAR REGRESSION (MLR) ##
#########################################
#    Using ORDINARY LEAST SQUARES REGRESSION, both of our Dependent and Independent
#    Variables being numerical-continuous.

## 1 a. Using 'mortality_rate' and 'GDP_per_capita' to regress 'Migration_index'

# => Step1: Dataset Loading: ... already loaded

# => Step2: Objective:
#       I want to examine the possible linear relations between 'mortality_rate'
#       and 'GDP_per_capita' vs 'Migration_index'


# => Step3: Multiple Linear Regression Analysis:

model_2 <- lm(formula = Migration_Index ~ mortality_rate + GDP_per_capita,
              data = migration_reduced)
summary.lm(model_2)

# FINDINGS:
# - The Intercept, mortality_rate and GDP_per_capita are significant at 0.05 and
#   the MLR equation is
# - Migration_Index = 8.473 - 0.5024 x mortality_rate + 0.00005297 x GDP_per_capita
# - Adjusted R2 = 0.82, i.e. mortality_rate explained 82% of the entire variation
#   in Migration_Index with only about 3% increment over 79% in the case of SLR
#   with only 'mortality_rate'

# PLOTTING
pairs(migration_reduced[,c('Migration_Index','mortality_rate', 'GDP_per_capita')],
     lower.panel = NULL, pch=19, cex=0.2, col='blue')

# => Step4: Ensuring Fitted model meets SLR assumptions
# 1. Linearity: The pair plot proves there is a linear relationship: Passed
# 2. Residual's Independence

plot(model_2, 1)

# The correlation of residuals vs fit is approximately 0, no pattern observed: Passed

# 3. Normality of Residuals

plot(model_2, 2)

# Near linear observations, residuals is normally distributed: Passed

# 4. Equal Variances of Residuals (Homoscedasticity)

plot(model_2, 3)

# Variance of residuals are constant and not related to the fitted line. No clear
# pattern among residuals, randomly scattered residuals: Passed

# 5. No multicollinearity (Using Variance Inflation Factor (VIF) to test for multicolinearity)

vif(model_2)

# Since the VIF value for both variables are less than 5, we can conclude that there
# is no multicollinearity among them Independent Variables 'mortality_rate' and 'GDP_per_capita'



# Step6: Reporting

# All 5 assumptions were approved, therefore we can confirm that our fitted
# regression line is indeed:
# Migration_Index = 8.473 - 0.5024 x mortality_rate + 0.00005297 x GDP_per_capita
# Example: Given, mortality_rate as 8 and GDP_per_capita as 65000.4323,
# Migration_Index = 8.473 - 0.5024 x 8 + 0.00005297 x 65000.4323 = 7.896873


## 3. MULTIPLE LINEAR REGRESSION (MLR) ##
#########################################
#    Using ORDINARY LEAST SQUARES REGRESSION, both of our Dependent and Independent
#    Variables being numerical-continuous.

## 1 a. Using 'mortality_rate', 'GDP_per_capita' and 'tuberculosis_detect_rate'
## to regress 'Migration_index'

# => Step1: Dataset Loading: ... already loaded

# => Step2: Objective:
#       I want to examine the possible linear relations between 'mortality_rate'
#       'GDP_per_capita' and 'tuberculosis_detect_rate' vs 'Migration_index'


# => Step3: Multiple Linear Regression Analysis:

model_3 <- lm(formula = Migration_Index ~ mortality_rate + GDP_per_capita
              + tuberculosis_detect_rate, data = migration_reduced)
summary.lm(model_3)

# FINDINGS:
# - The Intercept, mortality_rate, GDP_per_capita and tuberculosis_detect_rate
# are significant at 0.05 and the MLR equation is
# - Migration_Index = 10.668 - 0.6147 x mortality_rate + 0.00006396 x GDP_per_capita
#                     - 0.08571 x tuberculosis_detect_rate
# - Adjusted R2 = 0.84, i.e. mortality_rate explained 84% of the entire variation
#   in Migration_Index with only about 2% increment over 79% in the case of SLR
#   with only 'mortality_rate' and 'GDP_per_capita'

# PLOTTING
pairs(migration_reduced[,c('Migration_Index','mortality_rate', 'GDP_per_capita',
                           'tuberculosis_detect_rate')],lower.panel = NULL,
      pch=19, cex=0.2, col='blue')

# => Step4: Ensuring Fitted model meets SLR assumptions
# 1. Linearity: The pair plot proves there is a linear relationship: Passed
# 2. Residual's Independence

plot(model_3, 1)

# The correlation of residuals vs fit is approximately 0, no pattern observed: Passed

# 3. Normality of Residuals

plot(model_3, 2)

# Near linear observations, residuals is normally distributed: Passed

# 4. Equal Variances of Residuals (Homoscedasticity)

plot(model_3, 3)

# Variance of residuals are constant and not related to the fitted line. No clear
# pattern among residuals, randomly scattered residuals: Passed

# 5. No multicollinearity (Using Variance Inflation Factor (VIF) to test for multicolinearity)

vif(model_3)

# Since the VIF value for all three variables are less than 5, we can conclude that there
# is no multicollinearity among the Independent Variables 'mortality_rate' and 'GDP_per_capita'



# Step6: Reporting

# All 5 assumptions were approved, therefore we can confirm that our fitted
# regression line is indeed:
# Migration_Index = 8.473 - 0.5024 x mortality_rate + 0.00005297 x GDP_per_capita
# Example: Given, mortality_rate as 8 and GDP_per_capita as 65000.4323,
# Migration_Index = 10.668 - 0.6147 x mortality_rate + 0.00006396 x GDP_per_capita
#                     - 0.08571 x tuberculosis_detect_rate


########################
## GENERAL CONCLUSION ##
#######################
# From the various models built, the minimum yet considerably good linear model
# to use, with 79% explainability of the dependent variable, without incurring
# additional error values introduced with the inclusion of additional variable
# to the model only always brings about marginal addition of meager 2% increased
# explainabilty of the dependent variable('Migration_Index')

# Hence, the fitted linear model has the equation:
# Migration_Index = 12.53547 - 0.66333 x mortality_rate
