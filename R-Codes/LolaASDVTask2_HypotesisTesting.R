install.packages("ggplot2")
install.packages("qqplotr")
install.packages("stats")
#install.packages("readxl")
install.packages("dplyr")

library(ggplot2)
library(qqplotr)
library(stats)
library(readxl)
library(dplyr)

## Loading Dataset
net_migration_data = read_excel("Net Migration per 1000 population.xlsx",
                                  sheet = "UNdata_Export_20221106_21490500")
head(net_migration_data)
View(net_migration_data)
summary(net_migration_data)

# Renaming column name
colnames(net_migration_data)[1] = "Country"
View(net_migration_data)

# Extracting only the "Country" and "Index" columns only
selected_migration_data = select(net_migration_data, "Country", "Index")
View(selected_migration_data)

# Filtering only Australia and Canada
australia_canada_migration_data = selected_migration_data[
  selected_migration_data$Country == "Australia" |
    selected_migration_data$Country == "Canada", ]

View(australia_canada_migration_data)
dim(australia_canada_migration_data)

# Converting the country into factor type, to satisfy categorical
# Independent vs numerical response requirement for Independent two-sample t-test

australia_canada_migration_data$Country = as.factor(
  australia_canada_migration_data$Country)

head(australia_canada_migration_data)
View(australia_canada_migration_data)

# Acessing the normality of the Numerical response variable "Index"

# Using Shappiro-Wilk Test (Statistical Test) to test for normality
set.seed(20)

# perform the test
shapiro.test(australia_canada_migration_data$Index)

# Observation:
# The p-value of the test is 0.06965 is greater than 0.05, therefore the data is
# normally distributed


# Since we have categorical Independent and Numerical Response variable which is
# normally distributed, we can perfrom our T-test

###########################
## 1. PERFORMING T-TESTS ##
###########################

## Test Objective:
# Null Hypothesis: The migration rate in Australia and Canada are the same. i.e.
#       i.e. the mean of Australia and mean of Canada Migration indices are the same
#     Ho: mu(Australia) = mu(Canada)
# Alternative Hypothesis: The migration rate of Australia and Canada are not the
#     same. i.e. the mean of Australia and mean of Canada Migration indices are not
#     the same
#     Ho1: mu(Australia) != mu(Canada)
# Setting the threshold value, p-value = 0.05

# "Country" had already been converted into factor data type (categrical)

# Inspecting data
head(australia_canada_migration_data)
View(australia_canada_migration_data)
summary(australia_canada_migration_data)
dim(australia_canada_migration_data)

hist(australia_canada_migration_data$Index)

# Comparing Australia vs Canada Migration
boxplot(Index ~ Country, data = australia_canada_migration_data,
        names=c("Australia", "Canada"),
        xlab = "Australia or Canada",  ylab="Migration Index",
        main="Migration Index for Australia and Canada")

##### Performing the Independent Two-Sample T-Test #####
t.test(Index ~ Country, australia_canada_migration_data)


# Conclusion:
# - The result of the test shows that we CAN ACCEPT THE NULL HYPOTHESIS, because
#   the p-value = 0.4654 obtained is greater than the threshold value of 0.05 that
#   we set.


#######################################
## 1. PERFORMING NON-PARAMETRIC TEST ##
#######################################

## Test Objective:
# Null Hypothesis: The migration rate in Australia and United Kingdom are the same. i.e.
#       i.e. the mean of Australia and mean of UK Migration indices are the same
#     Ho: mu(Australia) = mu(UK)
# Alternative Hypothesis: The migration rate of Australia and UK are not the
#     same. i.e. the mean of Australia and mean of UK Migration indices are not
#     the same
#     Ho1: mu(Australia) != mu(UK)
#
# Setting the threshold value, p-value = 0.05

## THE TEST

# Filtering only Australia and United Kingdom (UK)
aust_uk_migration_data = selected_migration_data[
  australia_canada_migration_data$Country == "Australia" |
    australia_canada_migration_data$Country == "United Kingdom", ]

View(australia_canada_migration_data)
dim(australia_canada_migration_data)

# Converting the country into factor type, to satisfy categorical
# Independent vs numerical response requirement for Independent two-sample t-test

australia_canada_migration_data$Country = as.factor(
  australia_canada_migration_data$Country)

head(australia_canada_migration_data)
View(australia_canada_migration_data)

# Acessing the normality of the Numerical response variable "Index"

# Using Shappiro-Wilk Test (Statistical Test) to test for normality
set.seed(20)

# perform the test
shapiro.test(australia_canada_migration_data$Index)

# Observation:
# The p-value of the test is 0.06965 is greater than 0.05, therefore the data is
# normally distributed


# "Country" had already been converted into factor data type (categrical)

# Inspecting data
head(australia_canada_migration_data)
View(australia_canada_migration_data)
summary(australia_canada_migration_data)
dim(australia_canada_migration_data)

hist(australia_canada_migration_data$Index)

# Comparing Australia vs Canada Migration
boxplot(Index ~ Country, data = australia_canada_migration_data,
        names=c("Australia", "Canada"),
        xlab = "Australia or Canada",  ylab="Migration Index",
        main="Migration Index for Australia and Canada")

##### Performing the Independent Two-Sample T-Test #####
t.test(Index ~ Country, australia_canada_migration_data)


# Conclusion:
# - The result of the test shows that we CAN ACCEPT THE NULL HYPOTHESIS, because
#   the p-value = 0.4654 obtained is greater than the threshold value of 0.05 that
#   we set.




#
