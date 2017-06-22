# This file contains all the code
# Before proceeding with the code please set the working directory appropriately
########################################
# Author : Srinivasan                  #
# Date: 6/19/2017                      #
# Email: srinivasan.ayyangar@gmail.com #
# Cell: (469)-740-6664                 #
########################################

#**********************************************
# Part One: Finding The Summary of all the data
#***********************************************
#Data 1:
crude_oil = read.csv(file="Crude Oil.csv",head=TRUE,sep=",")
data_frame <- setNames(crude_oil, c("Date","Cushing, WTI","Brent, Europe"))
head(data_frame)
summary(data_frame,na.rm=True)


#Data 2:
conv_gas = read.csv(file="Conventional_Gasoline.csv",head=TRUE,sep=",")
data_frame <- setNames(conv_gas, c("Date","NY_Regular","US_Gulf_Regular"))
head(data_frame)
summary(data_frame,na.rm=True)

#Data 3:
Reg_Gas = read.csv(file="RBOB Regular Gasoline.csv",head=TRUE,sep=",")
data_frame <- setNames(Reg_Gas, c("Date","LA_Reformulated_ROBR"))
head(data_frame)
summary(data_frame,na.rm=True)

#Data 4:
Heat_Oil = read.csv(file="Heating Oil.csv",head=TRUE,sep=",")
data_frame <- setNames(Heat_Oil, c("Date","NY_Heating_Oil_Spot"))
head(data_frame)
summary(data_frame,na.rm=True)

#Data 5:
Lsd = read.csv(file="Low_Sulphur_Diesel.csv",head=TRUE,sep=",")
data_frame <- setNames(Lsd, c("Date","NY_Harbour","US_Gulf_Coast","LA_CARB"))
head(data_frame)
summary(data_frame,na.rm=True)

#Data 6:
kerosene= read.csv(file="Kerosene-Type Jet Fuel.csv",head=TRUE,sep=",")
data_frame <- setNames(kerosene, c("Date","US_Gulf_Coast"))
head(data_frame)
summary(data_frame,na.rm=True)

#Data 7:
Propane = read.csv(file="Propane.csv",head=TRUE,sep=",")
data_frame <- setNames(Propane, c("Date","TX Propane Spot Price"))
head(data_frame)
summary(data_frame,na.rm=True)

#***********************************************
#***********************************************

#****** START PREDICTIVE MODELLING******#


Training = read.csv(file="Train.csv",head=TRUE,sep=",")
Training <- setNames(Training, c("Date","WTI","BRT"))
head(Training)
summary(Training$WTI) 
summary(Training$BRT)

#Removing the BRT column as we will not use it fore prediction
Training[3] <- NULL
head(Training)

#  Convert to date if not already
Training$Date <- as.Date(Training$Date)
head(Training)

library(lubridate)

#  Get months
Training$Months <- month(as.POSIXlt(Training$Date, format="%m"))

#  Get years
Training$Year <- year(as.POSIXlt(Training$Date, format="%Y"))

# Transforming the values to Numeric
transform(Training,Months = as.numeric(Months))
transform(Training,Year = as.numeric(Year))

# calculating the time to be used in Loess Filter
Training$time <- Training$Year + Training$Months/12

# Plotting the graphs
plot(Training$WTI,type='l',xlab='Time',ylab='WTI monthly price')

# Getting the necessary libraries
library(mFilter)
require(mFilter)

# Using the HP filter to smooth the fluctuations and trying
# to make the series stationary

wti_hp=hpfilter(Training$WTI, freq=100,type="lambda",drift=F)
trend=ts(wti_hp$trend)
cycle=ts(wti_hp$cycle)
plot(ts.union(trend,cycle),type="l",xlab="Time",ylab="", main='Decomposition of WTI monthly price as trend and cycle')

# HP filter did not provide the necessary structure, hence rejecting it

# Trying with Loess Filter 
wti_low <- ts(loess(Training$WTI~Training$time,span=0.4)$fitted)
wti_hi <- ts(Training$WTI - loess(Training$WTI~Training$time,span=0.07)$fitted)
wti_cycles <- Training$WTI - wti_low - wti_hi
plot(ts.union(Training$price, wti_low,wti_hi,wti_cycles),
     main="Decomposition of WTI monthly price as trend + noise + cycles")

# Loess Filter did not provide the expected result, rejecting it

# Trying LOG and Log Difference
# Taking the Log Plot:
plot(log(Training$WTI),type='l',ylab='log(wti)')
# setting difference to 1
plot(diff(log(Training$WTI),differences = 1),type='l',ylab='difference of log(wti)')

# Model Selection

diff_logprice=diff(log(Training$WTI),differences = 1)
f=spectrum(diff_logprice,spans=c(2,2), main="Smoothed periodogram")

# Training The model
Training.arima101 <- arima(log(Training$WTI),order=c(2,1,2),seasonal=list(order=c(1,0,1),period=12))

Training.pred1 <- predict(Training.arima101, n.ahead = 12)

plot (Training$WTI,type="l")
lines(Training.pred1$pred, col="blue")
lines(Training.pred1$pred+2*Training.pred1$se, col="red")
lines(Training.pred1$pred-2*Training.pred1$se, col="red")

library(forecast)
require(forecast)

ax <- forecast(Training.arima101)
ax$mean
# this provides us the Log mean of the data points from which we can obtain the actual values

