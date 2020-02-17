rm(list = ls())




#READ THE DATA AND CODE IT WITH THE TIMESERIES FORMAT
shampoo<- read.csv('C:/Users/HP/Desktop/NorthEastern University/Intermediate Analytics/M5/0.Project - Time Series in R/shampoo.csv')
shampoo<- shampoo$Sales
View(shampoo)
shampoo_timeseries<- ts(shampoo, frequency=12)
shampoo_timeseries
summary(shampoo_timeseries)



#PLOTTING THE TIME SERIES
plot.ts(shampoo_timeseries,ylab="Sales", xlab="Year", main="Shampoo Sales")

#DECOMPOSING THE TIME SERIES decomposing Time Series 
#install.packages('TTR')
library(TTR)

#Decomposing and seasonally adjusting
shampoo_timeseriesComponents <- decompose(shampoo_timeseries) #Decomposing Seasonal Data 
shampoo_timeseriesComponents$seasonal #get the estimated values of the seasonal components
plot(shampoo_timeseriesComponents)
shampoo_timeseries_seasonallyadjusted <- shampoo_timeseries - shampoo_timeseriesComponents$seasonal
plot(shampoo_timeseries_seasonallyadjusted, ylab="Sales", xlab="Year", main="Shampoo Sales Seasonally adjusted")




# Since we have the data that can be described with Additive Model, going upward and seasonailty, we will use the Holt Winters Exponenetial smoothing
#HOLT WINTERS EXPONENTIAL SMOOTHING
library('forecast')
log_shampoo_timesereis <-log(shampoo_timeseries)
shampoo_timeseries_forecast <- HoltWinters(log_shampoo_timesereis)
shampoo_timeseries_forecast
shampoo_timeseries_forecast$SSE #sum of squared estimate of errors 
plot(shampoo_timeseries_forecast) #plotting the Sales prediction 
                                  #The plot is not very successful but it was decent enough 
shampoo_timeseries_forecast2<- forecast(shampoo_timeseries_forecast,h=36)
                                  #Forecasting for the next 3 years
plot(shampoo_timeseries_forecast2)
acf(na.omit(shampoo_timeseries_forecast2$residuals, lag.max=20)) #Making Correlogram 
Box.test(shampoo_timeseries_forecast2$residuals, lag=20, type="Ljung-Box") #Use the Ljung-Box test
plot.ts(na.omit(shampoo_timeseries_forecast2$residuals))            #Make the Time Plot 
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(na.omit(shampoo_timeseries_forecast2$residuals))  #Make a histogram





#ARIMA MODELS 
#Turning the data into Stationary data with Differencing a Time Series
shampoo_diff1 <- diff(shampoo_timeseries, differences = 1) #Not very stationary
plot.ts(shampoo_diff1)
shampoo_diff2 <- diff(shampoo_timeseries, differences = 2) #More Stationary 
plot.ts(shampoo_diff2)

#Selecting a Candidate ARIMA Model
acf(shampoo_diff2, lag.max=20)             # plot a correlogram
acf(shampoo_diff2, lag.max=20, plot=FALSE) # get the autocorrelation values
#The  autocorrelation flutuates which means the dataset pass the Agumented Dickey Fuller test, the dataset input is stationary
pacf(shampoo_diff2, lag.max=20)             # plot a partial correlogram
pacf(shampoo_diff2, lag.max=20, plot=FALSE) # get the  partial autocorrelation values
#The Partial autocorrelation at lag 1 is neg, more than -0.8 and the lag 2 is neg, more than -0.3 and tail off to 0 after lag 3  

#Forecasting using an ARIMA Model 
#d: 2 or 3 because the lags go to within the range and the lag 3 and lag 4 are equal to each other  
#p: 2 The number of lags before going back to the range 
#q: 2 because you are using moving average 2
#Normal ARIMA
shampoo_timeseries_arima <- arima(shampoo_timeseries, order= c(2,2,2)) #fit an ARIMA (2,2,2) model
shampoo_timeseries_arima
shampoo_timeseries_forecast3 <- forecast(shampoo_timeseries_arima, h=12)
shampoo_timeseries_forecast3
plot(shampoo_timeseries_forecast3)
#Auto ARIMA
shampoo_timeseries_arima <- auto.arima(shampoo_timeseries, seasonal=FALSE) #they decide the model for you
shampoo_timeseries_arima
shampoo_timeseries_forecast3 <- forecast(shampoo_timeseries_arima, h=12)
shampoo_timeseries_forecast3
plot(shampoo_timeseries_forecast3)




