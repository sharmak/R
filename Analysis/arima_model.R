# Most of the stuff is copied from https://www.otexts.org/fpp/8/
# Arima model and exponental smoothing are two techniques
# used in time series forecasting
# ARIMA - Uses auto-correlations
# Exponential smoothing - Uses trend and seasonality
# Stationary time series is the series in which value at a time doesn't depend
# upon the time at which it is observed 
# e.g. seasonal series, trend series are not stationary
# White noise is stationary. 
# Differencing: take the diff consective terms to convert non stationary
# series to stationary e.g. take dow zones it appears a non-stationary series
# but if we take the difference of consecutive values the series changes to 
# stationary. This is one example of converting non-stationary series to 
# stationary.
library(ggplot2)
Quandl.auth("XQQrEp2ntQZ4Syg2pMbZ")
dow <- Quandl("BCB/UDJIAD1", start_date='2014-01-01')
dow.z <- zoo(dow$Value, dow$Date)
autoplot(dow.z)
dow.ret.1.day <- dow.z -lag(dow.z, -1)
autoplot(dow.ret.1.day)

# One way to check stationarity is to plot acf on the series
# for stationary series, acf drops to zero quickly while for non-stationary
# series it drop slowly to zero
acf(dow$Value)
acf(coredata(dow.ret.1.day))
