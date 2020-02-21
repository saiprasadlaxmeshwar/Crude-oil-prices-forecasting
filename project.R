library(fpp)
library(fpp2)
library(lubridate)
library(ggplot2)
library(forecast)
library(dplyr)
library(colortools)


data<-Crude_Oil_Prices_Daily

head(data)
class(data)

# Coerce monthly_milk to `Date` class ----
class(data$Date)
data$Date <- as.Date(data$Date, format = "%m/%d/%Y")
# Check it worked
class(data$Date) 

# Experiment with `format()`
format(data$Date, format = "%Y-%B-%u")
class(format(data$Date, format = "%Y-%B-%u"))  # class is no longer `Date`


# Coerce bad date to good date ----
data$Date <- as.Date(data$Date, format = "%d/%b/%Y-%u")
head(data$Date)
class(data$Date)

attach(data)
library(ggplot2)

ggplot(data, aes(x = Date, y = `Closing Value`)) + 
  geom_line() + 
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") + 
  theme_classic()



# Extract month and year and store in new column
data$year <- format(data$Date, format = "%Y")
data$month_num <- format(data$Date, format = "%m")


ggseasonplot(monthly_oil_model, year.labels=TRUE
             ,year.labels.left=TRUE) + 
ggtitle("Seasonal plot")


train <- window(x = monthly_milk_ts, start=c(1962), end=c(1970))
test <- window(x = monthly_milk_ts, start=c(1970))


# Using ts objects to decompose trends ----

# Transform to `ts` class
monthly_oil_ts <- ts(data$`Closing Value`, start = 1986, end = 2018, freq = 250)  # Specify start and end year, measurement frequency (monthly = 12)

# Decompose using `stl()`
monthly_oil_stl <- stl(monthly_oil_ts, s.window = "period")

# Generate plots
plot(monthly_oil_stl)  # top=original data, second=estimated seasonal, third=estimated smooth trend, bottom=estimated irregular element i.e. unaccounted for variation
monthplot(monthly_oil_ts, choice = "seasonal")  # variation in milk production for each month
seasonplot(monthly_oil_ts)


# Forecasting ----

# Split data into testing and training
monthly_oil_model <- window(x = monthly_oil_ts, start=c(1986), end=c(2006))
monthly_oil_test <- window(x = monthly_oil_ts, start=c(2007))

# Simple model 

fit1<-meanf(monthly_oil_model,h=250)  
fit2<-naive(monthly_oil_model,h=250)                
fit3<-snaive(monthly_oil_model,h=250) 
fit4<-rwf(monthly_oil_model,h=250)                
print("Accuracy for Average method")
accuracy(fit1, monthly_oil_test)
print("Accuracy for Naive method")
accuracy(fit2, monthly_oil_test)
print("Accuracy for Seasonal Naive method")
accuracy(fit3, monthly_oil_test)
print("Accuracy for Drift method")
accuracy(fit4, monthly_oil_test)


autoplot(monthly_oil_model)+
  autolayer(meanf(monthly_oil_model,h=250),
            series="Mean",PI=FALSE)+
  autolayer(naive(monthly_oil_model,h=250),
            series="Naive",PI=FALSE)+
  autolayer(snaive(monthly_oil_model,h=250),
            series="Seasonal Naive",PI=FALSE)+
  autolayer(rwf(monthly_oil_model, drift=TRUE, h=250),
            series="Drift", PI=FALSE) +
  ggtitle("Forecasts")+
  xlab("Year")+ylab("Closing value")+
  guides(colour=guide_legend(title="Forecast"))


##### STL Decomposition

tail(monthly_oil_model)


fit<-stl(monthly_oil_model, s.window='periodic')

plot(monthly_oil_model, col="gray", main="Crude_Oil_Prices_Daily",
     ylab="value", xlab="")
lines(fit$time.series[,2],col="red",ylab="T rend")

plot(fit)

plot(monthly_oil_model, col="gray", main="Crude_Oil_Prices_Daily",
     ylab="value", xlab="")
lines(seasadj(fit),col="red",ylab="Seasonally adjusted") #Seasonally adjusted

### Moving Average

plot(monthly_oil_model, col="gray", main="Crude_Oil_Prices_Daily",
     ylab="value", xlab="")
lines(ma(monthly_oil_model,3),col="blue")

plot(monthly_oil_model, col="gray", main="Crude_Oil_Prices_Daily",
     ylab="value", xlab="")




fit4<-ses(monthly_oil_model,h=40)
fit4$model
lines(fitted(fit3), col="red")
fit1 <-ses(monthly_oil_model, alpha=0.9645, initial="simple", h=250)
plot(fit1)


fit5<-stl(monthly_oil_model,t.window=250,s.window="periodic",robust=TRUE)
plot(fit5)

fit5%>%seasadj()%>%naive()%>%
  autoplot()+ylab("Closing Value")+ggtitle("Crude oil forecast")
naive(seasadj(fit5))


###Holt 
fc<-holt(monthly_oil_model,h=250)
fc$model

fc<-holt(monthly_oil_test,h=400,alpha = 0.9663, beta = 0.0001)

autoplot(monthly_oil_model)+
  autolayer(fc,series="Holt's method",PI=FALSE)+
  ggtitle("Forecasts from Holt's method")+xlab("Year")+
  ylab("Closing value")+
  guides(colour=guide_legend(title="Forecast"))

data2<-Crude_Oil_Prices_Daily
data2 = ts(data2$`Closing Value`, 
           freq=255)


print("Holt Winter's accuracy")
accuracy(fc)

monthly_oil_ts_1 <- ts(data$`Closing Value`, start = 1986, end = 2018, freq = 10)

fit6<-hw(monthly_oil_ts_1,seasonal="additive")
fit7<-hw(monthly_oil_ts_1,seasonal="multiplicative")

autoplot(monthly_oil_model) +
  autolayer(fit6,series="HW additive forecasts",PI=FALSE)+
  autolayer(fit7,series="HW multiplicative forecasts",PI=FALSE)+
  xlab("Year")+
  ylab("Closing value")+
  ggtitle("Crude oil prices")+
  guides(colour=guide_legend(title="Forecasts"))

auto.arima(monthly_oil_model, seasonal=FALSE)

c=forecast(monthly_oil_model,h=100)
b=Arima(monthly_oil_model,order = c(1,1,1))


forecast(b,h=10)
plot(monthly_oil_model)+plot(c,col="red")


accuracy(b,monthly_oil_test )
fit = auto.arima(data,seasonal = FALSE)
fit


b=Arima(monthly_oil_test,order = c(1,1,1))
accuracy(b)

fc = forecast(data, h=250)
plot(fc)

print("Simple exponential smoothing model accuracy")
accuracy(fit4)
