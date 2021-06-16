########################################################################
############################ Time Series ###############################
########################################################################

# Exploring the Time Series Data
TimeSeriesData=AirPassengers
TimeSeriesData= JohnsonJohnson
class(TimeSeriesData)
TimeSeriesData

# Create a data frame to store results
DecomposedTS <- decompose(x=TimeSeriesData, type = c("additive"))
plot(DecomposedTS)

# Loading the library to create time series model
library(forecast)

# Creating the ARIMA time series model
TSfit =auto.arima(TimeSeriesData)
TSfit

# Predicting the passenger flow for next 24 months
TSforecast=predict(TSfit,2)
TSforecast

# Creating upper and lower confidence intervals to Just to plot the time series
Ulimit <- TSforecast$pred + 2*TSforecast$se
Llimit <- TSforecast$pred - 2*TSforecast$se

# Visualizing the forecast
ts.plot(TimeSeriesData, TSforecast$pred, Ulimit, Llimit, col=c(1,2,4,4), lty = c(1,1,2,2))

####################################

# Reading data from csv and converting to time series data
SuperStoreSales=read.csv('/Users/farukh/Desktop/Data Science using R/SuperStoreSales.csv')
SuperStoreSales

# converting a column into time series data
# frequency=7 means daily data
# frequency=4 means quaterly data
# frequency=12 means monthly data
SalesTimeSeries= ts(data=SuperStoreSales$Quantity, start=c(2014,1), end= c(2017,12), frequency=12)
SalesTimeSeries

# Assigning to the variable for Time Series analysis
TimeSeriesData=SalesTimeSeries
