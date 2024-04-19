#install.packages("readxl")
#install.packages("TTR")
#install.packages("forecast")

library(readxl)
library(TTR)
library(forecast)
library(dplyr)

## Loading Dataset
migration_index_data = read_excel("migration4regression_data.xlsx",
                                  sheet = "Reformatted_migration_data")
head(migration_index_data)

australia_migration_data = select(migration_index_data, 'Migration_Index')[1:10,1]
View(australia_migration_data)

# Converting data into timeseries
aust_migration_timeseries <- ts(australia_migration_data, start = 2012)

# visualizing the timeseries
plot.ts(aust_migration_timeseries)


## DECOMPOSING THE TIMESERIES INTO ITS CONSTITUENT PARTS ##
###########################################################

# From the plot, it is observed that the timeseries is non-seasonal, therefore
# it only contains a trend component and an irregular (random) component

# Also, it is observed that the timeseries is non-seasonal and the radom
# flunctuations in the data are roughly constant in size over time, hence, it
# can probably be described using an additive model.

## 1. Estimating the trend component of the timeseries:
# We use Simple Moving Average Smoothing Techniques to smoothen the timeseries
# with the SMA() function in order to estimate the trend component

# Finding appropriate smoothing order, n
# Setting n=1, smoothing with order of 1
aust_migration_timeseriesSMA1 = SMA(aust_migration_timeseries, n=1)
plot.ts(aust_migration_timeseriesSMA1)
# Observation: Not good enough

# Setting n=2, smoothing with order of 2
aust_migration_timeseriesSMA2 = SMA(aust_migration_timeseries, n=2)
plot.ts(aust_migration_timeseriesSMA2)

## Observation:
# - Smoothing with SMA of order 2 seems to give a clearer picture of
#   the trend
# - We can see that there was a decrease in migration index of Australia between
#   2012 and 2015 after which there was an increase between 2015 to 2017. Since
#   2017 till 2021 the migration index decreases rapidly.

## 2. Decomposing components using the decompose function
## Observation: Since the timeseries has no seasonal component, we cannot use the
# decompose() function to decompose the ts.


## FORCASTING ##
################

## 1. SHORT-TERM FORECASTS
#     (using HoltWinters() function)

# Since the migration index timeseries for Australia is non-seasonal additive model
# we can make short-term forecast using Simple Exponential Smooting.
# Smoothing allows us to estimate the level at current time point.

# Using HoltWinters() function for short-term forecasts
aust_migration_timeseriesForecasts = HoltWinters(aust_migration_timeseries,
                                                 beta = FALSE, gamma = FALSE, l.start = 10.423)
aust_migration_timeseriesForecasts

# Interpretation:
# - The output of the HoltWinters() function shows that the estimate if the alpha
#   parameter is about 0.95, this is very close to 1, hence, the forecasts are
#   based on only the recent observations

# One limitation of HoltWinters() is that it can only make forecasts for the same
# covered by the original timeseries.

## Accessing the forecasts
aust_migration_timeseriesForecasts$fitted

# Plotting the forecasts
plot(aust_migration_timeseriesForecasts)

# Forecast Evaluation
cat('Sum of Squared Error is ', aust_migration_timeseriesForecasts$SSE)


## 1. LONG-TERM FORECASTS
#     (using forecast() function)

# Forecasting 5 years migration index for australia into the future (2022 - 2026)
aust_migration_timeseriesForecasts2 <- forecast(aust_migration_timeseriesForecasts, h=5)
aust_migration_timeseriesForecasts2
plot(aust_migration_timeseriesForecasts2)


# Interpretation:
# - The blue line indicates the forecasts, purple-shaded area indicates 80% prediction
#   interval, while the gray-shaded area indicates 95% prediction interval.

# CHECKING IF THE PREDICTIVE MODEL CAN BE IMPROVED UPON
## 1. Is there correlation between forecast errors?

# plotting correlogram (using auto-correlation function, acf() function)
acf(aust_migration_timeseriesForecasts2$residuals, lag.max = 10, na.action = na.pass)

# Observations:
# - From the correlogram, it can be observed that all lags are within the significance
#   bounds. There is no significant evidence of a non-zero correlations at lags 1-10.

## 2. FUTHER TEST THAT THERE IS NO SIGNIFICANT EVIDENCE OF NON-ZERO CORRELATIONS AT LAGS 1-10
# Using Ljung-Box test, Box.test() function.

Box.test(aust_migration_timeseriesForecasts2$residuals, lag = 10, type = "Ljung-Box")

## Interpretation:
# - Since all lags are within the significance bounds, I believe there is no need
#   Ljung-Box test again. As evidence from the result of the test, X-squared and
#   p-value are NA (not available).

## 3. Are the forecast errors normally distributed with mean of zero and constant variance?

## Testing for constant variance
plot.ts(aust_migration_timeseriesForecasts2$residuals)

# Observation: The variance is not so constant.


## Testing for normal distribution of errors
plotForecastErrors <- function(forecast_errors){
  # making histogram of errors
  mybinsize <- IQR(forecast_errors) / 5
  mysd <- sd(forecast_errors)
  mymin <- min(forecast_errors) - mysd * 5
  mymax <- max(forecast_errors) + mysd * 3

  # generating mormally dist data with mean 0 and sd equals mysd
  mynorm <- rnorm(10000, mean = 0, sd = mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)

  if(mymin2 < mymin) { mymin <- mymin2 }
  if(mymax2 > mymax) { mymax <- mymax2 }

  # making a red histogram of of forecast_err, with norm dist data overlaid
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecast_errors, col='red', freq=FALSE, breaks=mybins)

  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plotting the normal curve as blue on top of hist of forecast errors
  points(myhist$mids, myhist$density, type="1", col='blue', lwd=2)
}

aust_migration_timeseriesForecasts2$residuals <-
  aust_migration_timeseriesForecasts2$residuals[
    !is.na(aust_migration_timeseriesForecasts2$residuals)]

# calling the function
plotForecastErrors(aust_migration_timeseriesForecasts2$residuals)


## ARIMA MODELS ##
##################

## 1. Making timeseries data stationary
aust_t_seriesDiff = diff(aust_migration_timeseries,differences = 1)
plot.ts(aust_t_seriesDiff)

aust_t_seriesDiff2 = diff(aust_migration_timeseries,differences = 2)
plot.ts(aust_t_seriesDiff2)


aust_t_seriesDiff3 = diff(aust_migration_timeseries,differences = 3)
plot.ts(aust_t_seriesDiff3)

# Now time-series is stationary in mean and var, as the levels of the series are roughl constant over time
# so we can define ARIMA(p,3,q)

## 2. Selecting appropriate ARIMA model.
# plotting auto-correlation (correlogram) for lag 1-10
acf(aust_t_seriesDiff3, lag.max = 10)
acf(aust_t_seriesDiff3, lag.max = 10, plot = FALSE)


# plotting partial auto-correlation (correlogram) for lag 1-10
pacf(aust_t_seriesDiff3, lag.max = 10)
pacf(aust_t_seriesDiff3, lag.max = 10, plot = FALSE)

# automatically finding best model
auto.arima(australia_migration_data)

# ARIMA(0,1,0)

## FORECASTING USING ARIMA ##
#############################
# building forecast model based on ARIMA
australia_t_seriesARIMA = arima(aust_migration_timeseries, order = c(0,1,0))
australia_t_seriesARIMA

# making forecasts
australia_t_seriesARIMAForecasts = forecast(australia_t_seriesARIMA, h=5)
australia_t_seriesARIMAForecasts

# Plot forecasts
plot(australia_t_seriesARIMAForecasts)

## Verifying whether the model can be improved upon ##
######################################################
## 1. Is there correlation between forecast errors?

# plotting correlogram (using auto-correlation function, acf() function)
acf(australia_t_seriesARIMAForecasts$residuals, lag.max = 10)

# Observations:
# - From the correlogram, it can be observed that all lags are within the significance
#   bounds. There is no significant evidence of a non-zero correlations at lags 1-10.

## 2. FUTHER TEST THAT THERE IS NO SIGNIFICANT EVIDENCE OF NON-ZERO CORRELATIONS AT LAGS 1-10
# Using Ljung-Box test, Box.test() function.

Box.test(australia_t_seriesARIMAForecasts$residuals, lag = 10, type = "Ljung-Box")

## Interpretation:
# - Since all lags are within the significance bounds, I believe there is no need
#   Ljung-Box test again. As evidence from the result of the test, X-squared and
#   p-value are NA (not available).


## Testing for constant variance
plot.ts(australia_t_seriesARIMAForecasts$residuals)

# Observation: The variance is not so constant.

australia_t_seriesARIMAForecasts$residuals <-
  australia_t_seriesARIMAForecasts$residuals[
    !is.na(australia_t_seriesARIMAForecasts$residuals)]

# calling the function
plotForecastErrors(australia_t_seriesARIMAForecasts$residuals)

