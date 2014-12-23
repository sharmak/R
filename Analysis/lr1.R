# Electricity consumption was recorded for a small town on 12 randomly 
# chosen days. 
# The following maximum temperatures (degrees Celsius) and 
# consumption (megawatt-hours) were recorded for each day. 
#
# Day   1	2	3	4	5	6	7	8	9	10	11	12
# Mwh 	16.3	16.8	15.5	18.2	15.2	17.5	19.8	19.0	17.5	16.0	19.6	18.0
# temp 	29.3	21.7	23.7	10.4	29.7	11.9	9.0	23.4	17.8	30.0	8.6	11.8
# 
# Plot the data and find the regression model for Mwh with temperature as 
# an explanatory variable. Why is there a negative relationship?
# Produce a residual plot. 
# Is the model adequate? Are there any outliers or influential observations?
# Use the model to predict the electricity consumption that you would expect 
# for a day with maximum temperature 10∘ and a day with maximum 
# temperature 35∘. Do you believe these predictions?
# Give prediction intervals for your forecasts. 
# The following R code will get you started: 
library(fpp)
library(ggplot2)
ggplot(data=econsumption) + aes(x=Mwh, y=temp) +  geom_smooth(method="lm") + geom_point(shape=1)
fit <- lm(Mwh~temp, data=econsumption)
summary(fit)
fit$residuals
econsumption$residuals <- fit$residuals
ggplot(data=econsumption) + aes(x=temp, y=residuals) + geom_point(shape=1)
predict(fit, data.frame(temp=c(35)), interval="confidence")
predict(fit, data.frame(temp=c(10)), interval="confidence")


# The following table gives the winning times (in seconds) for 
# the men’s 400 meters final in each Olympic Games from 1896 to 
# 2012 (data set `olympic`).
# 1896  54.2	1928	47.8	1964	45.1	1992	43.50
# 1900	49.4	1932	46.2	1968	43.8	1996	43.49
# 1904	49.2	1936	46.5	1972	44.66
# 1908	50.0	1948	46.2	1976	44.27
# 1912	48.2	1952	45.9	1980	44.60
# 1920	49.6	1956	46.7	1984	44.27
# 1924	47.6	1960	44.9	1988	43.87
# 
# Update the data set `olympic` to include the winning times from 
# the last few Olympics.
# Plot the winning time against the year. 
# Describe the main features of the scatterplot.
# Fit a regression line to the data. Obviously the winning times 
# have been decreasing, but at what *average* rate per year?
# Plot the residuals against the year. What does this indicate about 
# the suitability of the fitted line?
# Predict the winning time for the men’s 400 meters final in 
# the 2000, 2004, 2008 and 2012 Olympics.
# Give a prediction interval for each of your forecasts. What assumptions have you made in these calculations?
# Find out the actual winning times for these Olympics (see www.databaseolympics.com). How good were your forecasts and prediction intervals? 
ggplot(data=olympic) + aes(x=Year, y=time) + geom_smooth(method="lm") + geom_point(shape=1)
fit <- lm(time~Year, data=olympic)
summary(fit)
olympic$resid <- fit$residuals
ggplot(data=olympic) + aes(x=Year, y=resid) + geom_point(shape=1)
predict(fit, data.frame(Year=(2000)), interval="confidence")  