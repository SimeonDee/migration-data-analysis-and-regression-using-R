## Clearing the memory data for a fresh start
rm(list = ls())

## Installing Libraries (NOTE: remove the comments and comment back once installed)
#install.packages("ggplot2")
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("qqplotr")
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("RVAideMemoire")
#library("readxl")

## Loading libraries into current environment
library(readxl)
library(ggplot2)
library(dplyr)
library(qqplotr)
library(tidyverse)
library(corrplot)
library(RVAideMemoire)
library(stats)

## Loading dataset
migration_data = read_excel("Migration Data for ASDV.xlsx", sheet = "Raw Data")

## Exploring Dataset
View(migration_data)
names(migration_data)
colnames(migration_data)
row.names(migration_data)
head(migration_data)
tail(migration_data)

## Accessing if there are null rows
migration_data[(is.na(migration_data$`Country Code`)),]

# Due to the presence of ".." string in data, the numeric values are treated as strings
# Hence the need to convert them to numeric type below
migration_data[, 5:14] = lapply(migration_data[, 5:14], as.numeric)

# Dropping found null rows
migration_data = migration_data[!(is.na(migration_data$`Country Code`)),]


# Replacing "NA" denoting absence of value with mean of other years
# for the given country.

# Extracting years values from where means would be computed
year_values = migration_data[,5:14]
View(year_values)

# Replacing all ".." with NA, converting values into Numbers and replacing NA
# with mean of other years
clean_data = year_values
for (rw in 1:length(row.names(year_values))){
  curr_row = clean_data[rw,]
  loc_indices = which(curr_row %in% 'NA')
  mean_val <- mean(unlist(curr_row), na.rm=TRUE)

  migration_data[rw,5:14] = replace(curr_row, loc_indices, mean_val)
}

head(migration_data)
summary(migration_data)

# Saving a copy of the cleaned dataset
write.csv(migration_data, file = 'cleaned_migration_data.csv', row.names = FALSE)


# Selecting some factors to consider from the original dataset
selected_data = migration_data[
  migration_data$`Series Name` == 'GDP per capita (current US$)' |
    migration_data$`Series Name` == 'Inflation, consumer prices (annual %)' |
    migration_data$`Series Name` == 'Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)' |
    migration_data$`Series Name` == 'Tuberculosis case detection rate (%, all forms)', ]

View(selected_data)

# Extracting factors to consider across countries
gdp_data = selected_data[selected_data$`Series Name` == 'GDP per capita (current US$)', ]
inflation_data = selected_data[selected_data$`Series Name` == 'Inflation, consumer prices (annual %)', ]
mortality_data = selected_data[selected_data$`Series Name` == 'Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)', ]
tuberculosis_data = selected_data[selected_data$`Series Name` == 'Tuberculosis case detection rate (%, all forms)', ]

# Previewing the extracted data
View(gdp_data)
View(inflation_data)
View(mortality_data)
View(tuberculosis_data)

# Accessing if there are still null rows within the extracted datasets
migration_data[(is.na(migration_data$`Country Code`)),]
View(migration_data)

# Calculating Inflation Statistics for each country across the years
inflation_stats = NULL
country = inflation_data$`Country Name`
inflation_mean = c()
inflation_median = c()
inflation_std_dev = c()
inflation_variance = c()

for (row in 1:length(row.names(inflation_data))){
  mn <- mean(unlist(inflation_data[row, 5:14]), na.rm=TRUE)
  med <- median(unlist(inflation_data[row, 5:14]), na.rm=TRUE)
  std_dev <- sd(unlist(inflation_data[row, 5:14]))
  variance <- var(unlist(inflation_data[row, 5:14]))

  inflation_mean = append(inflation_mean, mn)
  inflation_median = append(inflation_median, med)
  inflation_std_dev = append(inflation_std_dev, std_dev)
  inflation_variance = append(inflation_variance, variance)
}

inflation_stats = data.frame(country, inflation_mean, inflation_median, inflation_std_dev,
                             inflation_variance)
View(inflation_stats)


# Calculating GDP per Capita Statistics for each country across the years
gdp_per_cap_stats = NULL
country = gdp_data$`Country Name`
gdp_mean = c()
gdp_median = c()
gdp_std_dev = c()
gdp_variance = c()

for (row in 1:length(row.names(gdp_data))){
  mn <- mean(unlist(gdp_data[row, 5:14]), na.rm=TRUE)
  med <- median(unlist(gdp_data[row, 5:14]), na.rm=TRUE)
  std_dev <- sd(unlist(gdp_data[row, 5:14]))
  variance <- var(unlist(gdp_data[row, 5:14]))

  gdp_mean = append(gdp_mean, mn)
  gdp_median = append(gdp_median, med)
  gdp_std_dev = append(gdp_std_dev, std_dev)
  gdp_variance = append(gdp_variance, variance)
}

gdp_per_cap_stats = data.frame(country, gdp_mean, gdp_median, gdp_std_dev,
                             gdp_variance)
View(gdp_per_cap_stats)


# Calculating Mortality from CVD, cancer, diabetes or CRD Statistics for each country
# across the years
mortality_stats = NULL
country = mortality_data$`Country Name`
mortality_mean = c()
mortality_median = c()
mortality_std_dev = c()
mortality_variance = c()

for (row in 1:length(row.names(mortality_data))){
  mn <- mean(unlist(mortality_data[row, 5:14]), na.rm=TRUE)
  med <- median(unlist(mortality_data[row, 5:14]), na.rm=TRUE)
  std_dev <- sd(unlist(mortality_data[row, 5:14]))
  variance <- var(unlist(mortality_data[row, 5:14]))

  mortality_mean = append(mortality_mean, mn)
  mortality_median = append(mortality_median, med)
  mortality_std_dev = append(mortality_std_dev, std_dev)
  mortality_variance = append(mortality_variance, variance)
}

mortality_stats = data.frame(country, mortality_mean, mortality_median, mortality_std_dev,
                             mortality_variance)
View(mortality_stats)


# Calculating "Tuberculosis case detection rate" Statistics for each country
# across the years
tuberculosis_stats = NULL
country = mortality_data$`Country Name`
tuberculosis_mean = c()
tuberculosis_median = c()
tuberculosis_std_dev = c()
tuberculosis_variance = c()

for (row in 1:length(row.names(tuberculosis_data))){
  mn <- mean(unlist(tuberculosis_data[row, 5:14]), na.rm=TRUE)
  med <- median(unlist(tuberculosis_data[row, 5:14]), na.rm=TRUE)
  std_dev <- sd(unlist(tuberculosis_data[row, 5:14]))
  variance <- var(unlist(tuberculosis_data[row, 5:14]))

  tuberculosis_mean = append(tuberculosis_mean, mn)
  tuberculosis_median = append(tuberculosis_median, med)
  tuberculosis_std_dev = append(tuberculosis_std_dev, std_dev)
  tuberculosis_variance = append(tuberculosis_variance, variance)
}

tuberculosis_stats = data.frame(country, tuberculosis_mean, tuberculosis_median,
                                tuberculosis_std_dev, tuberculosis_variance)
View(tuberculosis_stats)

# VISUALIZATIONS
#****************

# INFLATION
#sorted_inflation_mn = inflation_stats[order(-inflation_mean),]

# Barchart
barplot(inflation_stats$inflation_mean, names.arg = inflation_stats$country,
        col = rainbow(10), xlab = 'Coutries', ylab = 'Average Inflation')
title(main = 'Average Inflation')

# Piechart
percent <-round(100 * inflation_stats$inflation_mean /
                  sum(inflation_stats$inflation_mean), 1)
lbl_perc = paste(inflation_stats$country, '-', percent, '%')
pie(inflation_stats$inflation_mean, labels = lbl_perc, col = rainbow(10))
title(main = 'Average Inflation')

# GDP PER CAPITA
#sorted_inflation_mn = inflation_stats[order(-inflation_mean),]

# Barchart
barplot(gdp_per_cap_stats$gdp_mean, names.arg = gdp_per_cap_stats$country,
        col = rainbow(10), xlab = 'Coutries', ylab = 'Average GDP')
title(main = 'Average GDP Per Capita (USD)')
labels(gdp_per_cap_stats$inflation_mean)

# Piechart
percent <-round(100 * gdp_per_cap_stats$gdp_mean /
                  sum(gdp_per_cap_stats$gdp_mean), 1)
lbl_perc = paste(gdp_per_cap_stats$country, '-', percent, '%')
pie(gdp_per_cap_stats$gdp_mean, labels = lbl_perc, col = rainbow(10))
title(main = 'Average GDP Per Capita (USD)')



# MORTALITY FROM CVD, CANCER, DIABETES OR CRD
#sorted_inflation_mn = inflation_stats[order(-inflation_mean),]

# Barchart
barplot(mortality_stats$mortality_mean, names.arg = mortality_stats$country,
        col = rainbow(10), xlab = 'Coutries', ylab = 'Average Mortality')
title(main = 'Average Mortality from CVD, cancer, diabetes or CRD')

# Piechart
percent <-round(100 * mortality_stats$mortality_mean /
                  sum(mortality_stats$mortality_mean), 1)
lbl_perc = paste(mortality_stats$country, '-', percent, '%')
pie(mortality_stats$mortality_mean, labels = lbl_perc, col = rainbow(10))
title(main = "Average Mortality from CVD, cancer, diabetes or CRD")



# TUBERCULOSIS CASE DETECTION RATE
#sorted_inflation_mn = inflation_stats[order(-inflation_mean),]

# Barchart
barplot(tuberculosis_stats$tuberculosis_mean, names.arg = tuberculosis_stats$country,
        col = rainbow(10), xlab = 'Coutries', ylab = 'Average Tuberculosis rate')
title(main = 'Average Tuberculosis Detection Rate')

# Piechart
percent <-round(100 * tuberculosis_stats$tuberculosis_mean /
                  sum(tuberculosis_stats$tuberculosis_mean), 1)
lbl_perc = paste(tuberculosis_stats$country, '-', percent, '%')
pie(tuberculosis_stats$tuberculosis_mean, labels = lbl_perc, col = rainbow(10))
title(main = "Average Tuberculosis Detection Rate")


# DATA DISTRIBUTIONS AND SKEWNESS

# Inflation in US
us_inflation = unlist(inflation_data[1,5:14])
hist(us_inflation, breaks = 5, labels = FALSE, xlab = 'Inflation')
# title(main = "Inflation Distribution of USA (2012 - 2021)")
plot(density(us_inflation))




#### CORRELATION ANALYSIS FOR EACH FACTOR ####

# Pearson Correlations for the four (4) selected factors
gdp_per_cap_pearson_corr = round(cor(gdp_data[, 5:14], method = 'pearson'), digits = 2)
inflation_pearson_corr = round(cor(inflation_data[, 5:14], method = 'pearson'), digits = 2)
mortality_pearson_corr = round(cor(mortality_data[, 5:14], method = 'pearson'), digits = 2)
tuberculosis_pearson_corr = round(cor(tuberculosis_data[, 5:14], method = 'pearson'), digits = 2)

View(gdp_per_cap_pearson_corr)
View(inflation_pearson_corr)
View(mortality_pearson_corr)
View(tuberculosis_pearson_corr)


# Spearman Correlations for the four (4) selected factors
gdp_per_cap_spearman_corr = round(cor(gdp_data[, 5:14], method = 'spearman'), digits = 2)
inflation_spearman_corr = round(cor(inflation_data[, 5:14], method = 'spearman'), digits = 2)
mortality_spearman_corr = round(cor(mortality_data[, 5:14], method = 'spearman'), digits = 2)
tuberculosis_spearman_corr = round(cor(tuberculosis_data[, 5:14], method = 'spearman'), digits = 2)

View(gdp_per_cap_spearman_corr)
View(inflation_spearman_corr)
View(mortality_spearman_corr)
View(tuberculosis_spearman_corr)

# Visualizing the Pearson Correlations for the four (4) selected factors
corrplot(gdp_per_cap_pearson_corr, method = 'number', type = 'upper',
         title = 'Pearson Correlations of "GDP per Capita" for the ten(10) years')
corrplot(inflation_pearson_corr, method = 'number', type = 'upper',
         title = 'Pearson Correlations of "Inflation Rate" for the ten(10) years')
corrplot(mortality_pearson_corr, method = 'number', type = 'upper',
         title = 'Pearson Correlations of "Mortallity Rate" for the ten(10) years')
corrplot(tuberculosis_pearson_corr, method = 'number', type = 'upper',
         title = 'Pearson Correlations of "Tuberculosis Detection Rate" for the ten(10) years')

# Visualizing the Spearman Correlations for the four (4) selected factors
corrplot(gdp_per_cap_spearman_corr, method = 'number', type = 'upper',
         title = 'Spearman Correlations of "GDP per Capita" for the ten(10) years')
corrplot(inflation_spearman_corr, method = 'number', type = 'upper',
         title = 'Spearman Correlations of "Inflation Rate" for the ten(10) years')
corrplot(mortality_spearman_corr, method = 'number', type = 'upper',
         title = 'Spearman Correlations of "Mortality Rate" for the ten(10) years')
corrplot(tuberculosis_spearman_corr, method = 'number', type = 'upper',
         title = 'Spearman Correlations of "Tuberculosis Detection Rate" for the ten(10) years')






