setwd("C:\\Users\\sony\\Documents\\group project time series")

# Clearing the environment
rm(list = ls())

# Loading the required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(magrittr)
library(stats)
library(raster)
library(forecast)
library(tseries)
require(graphics)


#Loading the dataset
global <- read.csv("Global Superstore.csv")

summary(global)

str(global)
##################################################################################################################
#                                     Data Preparation & Cleaning
##################################################################################################################
# Checking for unique columns
length(unique(global$Order.ID)) # 25035 rows
length(unique(global$Row.ID)) # 51290 rows

# Removing the duplicate entries

global <- global[!duplicated(global[c(2,15)]),]

#Checking for NA values
sapply(global, function(x) sum(is.na(x))) # 41266 NAs in Postal Code column
sapply(global, function(x) length(which(is.na(x))))
sapply(global, function(x) length(which(x ==" ")))

#Removing the postal code column as it doesn't have any significance and filled with 80% NA values
global <- global[,-12]


# Converting to default date format
global$Order.Date <- as.Date(global$Order.Date,"%d-%m-%Y")
global$Ship.Date <- as.Date(global$Ship.Date,"%d-%m-%Y")

# Deriving a new metric
global$Market_Seg <- paste(global$Market, global$Segment, sep = "_")
global$Market_Seg <- as.factor(global$Market_Seg)

# Extracting months from the date column as we need to perform the analysis month-wise
global$order_month_year <- as.factor(format(global$Order.Date,"%m-%Y"))
summary(global$order_month_year)

#univariate analysis
ggplot(global,aes(order_month_year,fill = order_month_year))+geom_histogram(stat = "count")

# Aggregating the values for 21 market segments by its ordered month and year
segmented <- global %>% mutate(month = as.numeric(format(Order.Date, "%m")), year = as.numeric(format(Order.Date, "%Y"))) %>%
  group_by(Market_Seg, year, month) %>%
  summarise(monthly_sales = sum(Sales),
            monthly_qty = sum(Quantity), 
            monthly_profit = sum(Profit))


#1008-6(can_con)-14(can_corp)-23(can_home office) so 1008-6-14-23 = 965

# Aggregating the total values of sales,profit,quantity and Coefficient of variation for 21 market segments 
aggregated_values <- global %>%  group_by(Market_Seg) %>% summarise(.,tot_sales=sum(Sales),tot_profit = sum(Profit),tot_qty = sum(Quantity),
                                                                    CV = cv(Profit))

# Plotting them to visualise the most profitable segments and with lowest CoV value
# Plotting the Coefficent of Variation against Market-seg
ggplot(aggregated_values,aes(Market_Seg,CV,fill=Market_Seg))+geom_bar(position = "dodge",stat = "identity")

# Plotting the total profit against Market-seg
ggplot(aggregated_values,aes(Market_Seg,tot_profit,fill=Market_Seg))+geom_bar(position = "dodge",stat = "identity")

# APAC-Consumer & EU - Consumer are the two most profitable segments as the total profit 221356.209 & 188651.078 for those two segments
#is Higher and the CoV is also 421.4 and 471.6

# Deriving a new column for month and year together from the segmented
segmented$month_year <- paste(segmented$month,segmented$year,sep = "-")

# Filtering the two most profitable segments from the aggregated values
EU_Consumer <- filter(segmented,Market_Seg == "EU_Consumer")
APAC_Consumer <- filter(segmented,Market_Seg == "APAC_Consumer")

# Replacing the factor format of order year and month with numbers 

EU_Consumer$month_year <- seq(1:48)
APAC_Consumer$month_year <- seq(1:48)

#Creating a genralized function for smoothing .

smoothing_fun <- function(x){
  # Smoothing the series - Moving Average Smoothing
  w <- 1
  smoothedseries <- stats::filter(x, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)
  # Smoothing left end of the time series
  diff <- smoothedseries[w+2] - smoothedseries[w+1]
  for (i in seq(w,1,-1)) {
    smoothedseries[i] <- smoothedseries[i+1] - diff
  }
  
  # Smoothing right end of the time series
  n <- length(x)
  diff <- smoothedseries[n-w] - smoothedseries[n-w-1]
  for (i in seq(n-w+1, n)) {
    smoothedseries[i] <- smoothedseries[i-1] + diff
  }
  return(smoothedseries)
}
####################################################################################################################
#                                        Modelling EU Consumer sales data
####################################################################################################################
#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

# Creating the total time series
EU_Con_sales_ts <- ts(EU_Consumer$monthly_sales)

# Dividing the train and test data
EU_Con_sales_ts_indata <- EU_Consumer[1:42,]
EU_Con_sales_ts_outdata <- EU_Consumer[43:48,]

#Plotting the time series for indata
EU_sales_timeser <- ts(EU_Con_sales_ts_indata$monthly_sales)
plot((EU_sales_timeser))

# Slightly increasing trend can be noticed and seasonality

# Smoothing the series - Moving Average Smoothing
#Smoothing the EU_Consumer_sales
smoothed_EU_sales <- smoothing_fun(EU_sales_timeser)

# Plotting the smoothed time series of EU_Consumer_sales
EU_sales_timevals_in <- EU_Con_sales_ts_indata$month_year
lines(smoothed_EU_sales, col="red", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

EU_sales_smootheddf <- as.data.frame(cbind(EU_sales_timevals_in, as.vector(smoothed_EU_sales)))
colnames(EU_sales_smootheddf) <- c('Month', 'Sales')


#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_sales <- lm(Sales ~ sin(0.5*Month) + cos(0.5*Month) + poly(Month,3) ,  data=EU_sales_smootheddf)
global_pred_EU_sales <- predict(lmfit_EU_sales, Month=EU_sales_timevals_in)
summary(global_pred_EU_sales)
lines(EU_sales_timevals_in, global_pred_EU_sales, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_sales <- EU_sales_timeser - global_pred_EU_sales
plot(local_pred_EU_sales, col='red', type = "l")

# ACF & PACF plot of local_pred
acf(local_pred_EU_sales)
pacf(local_pred_EU_sales)
acf(local_pred_EU_sales, type="partial")
#Modelling the local pred with auto arima
armafit_EU_sales <- auto.arima(local_pred_EU_sales)
tsdiag(armafit_EU_sales)
armafit_EU_sales

#From the tsdiag plot , ACF Of Residuals all maximum under the confidence interval therefore it is WHITE NOISE.

#We'll check if the residual series is white noise

resi_EU_sales <- local_pred_EU_sales-fitted(armafit_EU_sales)

adf.test(resi_EU_sales,alternative = "stationary")

# Augmented Dickey-Fuller Test
# 
# data:  resi_EU_sales
# Dickey-Fuller = -3.8315, Lag order = 3, p-value = 0.02677
# alternative hypothesis: stationary
 kpss.test(resi_EU_sales)
 
# KPSS Test for Level Stationarity
# 
# data:  resi_EU_sales
# KPSS Level = 0.10032, Truncation lag parameter = 1, p-value = 0.1

#From the above test , the residual is stationary .

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out_EU_sales <- EU_Con_sales_ts_outdata$month_year

global_pred_out_EU_sales <- predict(lmfit_EU_sales,data.frame(Month =timevals_out_EU_sales))

global_pred_EU_sales_combined <- global_pred_EU_sales+fitted(armafit_EU_sales)

fcast_EU_sales <- global_pred_out_EU_sales

#A the local part of the the time series is not completly white noise and it is having some lags,
# So we used MA(1) from auto arima to model this . 

arma_pred=predict(armafit_EU_sales,n.ahead = 6)$pred
fcast_total=fcast_EU_sales+arma_pred

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_EU_sales <- accuracy(fcast_total,EU_Con_sales_ts_outdata$monthly_sales)[5]
MAPE_class_dec_EU_sales
#[1] 23.13734

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_EU_sales <- c(ts(global_pred_EU_sales_combined),ts(global_pred_out_EU_sales))
plot(EU_Con_sales_ts, col = "black")
lines(class_dec_pred_EU_sales, col = "red")

#So, that was classical decomposition and its prediction
#########################################End of Classical decomposition of EU Conssumr Sales#################################################################
#now let's do an ARIMA fit

autoarima_EU_sales <- auto.arima(EU_sales_timeser)
autoarima_EU_sales
tsdiag(autoarima_EU_sales)
# Series: EU_sales_timeser 
# ARIMA(2,1,0) 
# 
# Coefficients:
#   ar1      ar2
# -0.5797  -0.4906
# s.e.   0.1346   0.1310
# 
# sigma^2 estimated as 168503216:  log likelihood=-445.83
# AIC=897.66   AICc=898.31   BIC=902.8
plot(autoarima_EU_sales$x, col="black")
lines(fitted(autoarima_EU_sales), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_EU_sales <- EU_sales_timeser - fitted(autoarima_EU_sales)

adf.test(resi_auto_arima_EU_sales,alternative = "stationary")
kpss.test(resi_auto_arima_EU_sales)
# Showing it is staionary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_sales <- predict(autoarima_EU_sales, n.ahead = 6)

MAPE_auto_arima_EU_sales <- accuracy(fcast_auto_arima_EU_sales$pred,EU_Con_sales_ts_outdata$monthly_sales)[5]
MAPE_auto_arima_EU_sales
#[1] 28.87421
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_EU_sales <- c(fitted(autoarima_EU_sales),ts(fcast_auto_arima_EU_sales$pred))
plot(EU_Con_sales_ts, col = "black")
lines(auto_arima_pred_EU_sales, col = "red")

#So from the above two models we conclude that , Classical Decomposition model is better and having good prediction on validation data set . 

####################################################################################################################
#                                        Modelling EU Consumer quantity data
####################################################################################################################
#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

EU_Con_Quan_ts_tot <- ts(EU_Consumer$monthly_qty)
# Dividing the train and test data
EU_Con_Quan_ts_indata <- EU_Consumer[1:42,]
EU_Con_Quan_ts_outdata <- EU_Consumer[43:48,]
#Plotting the TS of indata
EU_Con_Quan_ts <- ts(EU_Con_Quan_ts_indata$monthly_qty)
plot(EU_Con_Quan_ts)


#Smoothing the EU_Consumer_quantity

smoothed_EU_quantity <- smoothing_fun(EU_Con_Quan_ts)

# Plotting the smoothed series
EU_quantity_timevals_in <- EU_Con_Quan_ts_indata$month_year
lines(smoothed_EU_quantity, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe
#smoothedseries

EU_quantity_smootheddf <- as.data.frame(cbind(EU_quantity_timevals_in, as.vector(smoothed_EU_quantity)))
colnames(EU_quantity_smootheddf) <- c('Month', 'Quantity')


#modelling the global part of the time series using the additive model for both trend and seasonality

lmfit <- lm(Quantity ~ sin(0.5*Month)+poly(Month,4)+cos(0.5*Month), data=EU_quantity_smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(EU_quantity_timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- EU_Con_Quan_ts-global_pred
plot(local_pred, col='red', type = "l")
# Acf plot of local pred
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)
armafit
# Series: local_pred 
# ARIMA(2,0,0) with zero mean 
# 
# Coefficients:
#   ar1      ar2
# -0.5900  -0.5761
# s.e.   0.1223   0.1183
# 
# sigma^2 estimated as 8143:  log likelihood=-248.15
# AIC=502.31   AICc=502.94   BIC=507.52
tsdiag(armafit)

#####
##########we can see the left part is white noise ACF of residual plot################################

arma_fitted=armafit$fitted

global_pred_combined=global_pred+arma_fitted

#We'll check if the residual series is white noise
##we can see the left part is white noise ACF of residual plot###############################
resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#So we have concluded that , the residual time series is staionary and WHITE NOISE . 

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

timevals_out <- EU_Con_Quan_ts_outdata$month_year

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE
###predict values for auto arima and add local pred values

arma_pred=predict(armafit,n.ahead = 6)$pred
fcast_total=fcast+arma_pred


# Calculating the MAPE for classical decompostion
MAPE_class_dec <- accuracy(fcast_total,EU_Con_Quan_ts_outdata$monthly_qty)[5]
MAPE_class_dec
#[1] 27.36632
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred_combined),ts(fcast_total))
plot(EU_Con_Quan_ts_tot, col = "black")
lines(class_dec_pred, col = "red")

######################End of EU Consumer quantity data classical decomposition###########################################
#                                         ARIMA FIT Model
##################################################################################################################################

#now let's do an ARIMA fit

autoarima_EU_quantity<- auto.arima(EU_Con_Quan_ts)
autoarima_EU_quantity
tsdiag(autoarima_EU_quantity)
plot(autoarima_EU_quantity$x, col="black")
lines(fitted(autoarima_EU_quantity), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_EU_quantity <- EU_Con_Quan_ts - fitted(autoarima_EU_quantity)

adf.test(resi_auto_arima_EU_quantity,alternative = "stationary")
kpss.test(resi_auto_arima_EU_quantity)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_quantity <- predict(autoarima_EU_quantity, n.ahead = 6)

MAPE_auto_arima_EU_quantity <- accuracy(fcast_auto_arima_EU_quantity$pred,EU_Con_Quan_ts_outdata$monthly_sales)[5]
MAPE_auto_arima_EU_quantity
#[1] 99.03888
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_EU_quantity <- c(fitted(autoarima_EU_quantity),ts(fcast_auto_arima_EU_quantity$pred))
plot(EU_Con_Quan_ts_tot, col = "black")
lines(auto_arima_pred_EU_quantity, col = "red")

#So from the above two models we conclude that , Classical Decomposition model is better and having good prediction on validation data set . 

####################################################################################################################
#                                        Modelling APAC Consumer quantity data
####################################################################################################################
#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

APAC_Con_quan_ts <- ts(APAC_Consumer$monthly_qty)
#Dividing the train and test data
APAC_Con_quan_ts_indata <- APAC_Consumer[1:42,]
APAC_Con_quan_ts_outdata <- APAC_Consumer[43:48,]
#Plotting the inputdata TS
APAC_quan_timeser <- ts(APAC_Con_quan_ts_indata$monthly_qty)
plot((APAC_quan_timeser))
# Slightly increasing trend can be noticed and seasonality

# Smoothing the series - Moving Average Smoothing
# Smoothing the APAC Consumer Quantity

smoothed_APAC_quan <- smoothing_fun(APAC_quan_timeser)

# Plotting the smoothed time series of EU_Consumer_sales
APAC_quan_timevals_in <- APAC_Con_quan_ts_indata$month_year
lines(smoothed_APAC_quan, col="red", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

APAC_quan_smootheddf <- as.data.frame(cbind(APAC_quan_timevals_in, as.vector(smoothed_APAC_quan)))
colnames(APAC_quan_smootheddf) <- c('Month', 'Quantity')

#using Additive model for the global part of the time series. 

lmfit_APAC_quan <- lm(Quantity ~ sin(0.5*Month)+poly(Month,1)+cos(0.5*Month), data=APAC_quan_smootheddf)
global_pred_APAC_quan <- predict(lmfit_APAC_quan, Month=APAC_quan_timevals_in)
summary(global_pred_APAC_quan)
summary(lmfit_APAC_quan)
lines(APAC_quan_timevals_in, global_pred_APAC_quan, col='blue', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_quan <- APAC_quan_timeser - global_pred_APAC_quan
plot(local_pred_APAC_quan, col='red', type = "l")
#Plotting ACF
acf(local_pred_APAC_quan)
acf(local_pred_APAC_quan, type="partial")
armafit_APAC_quan <- auto.arima(local_pred_APAC_quan)

tsdiag(armafit_APAC_quan)
armafit_APAC_quan
# Series: local_pred_APAC_quan 
# ARIMA(0,0,0) with zero mean 
# 
# sigma^2 estimated as 13460:  log likelihood=-259.25
# AIC=520.5   AICc=520.6   BIC=522.24

#We'll check if the residual series is white noise

resi_APAC_quan <- local_pred_APAC_quan-fitted(armafit_APAC_quan)

adf.test(resi_APAC_quan,alternative = "stationary")
kpss.test(resi_APAC_quan)
# Telling it is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months


timevals_out_APAC_quan <- APAC_Con_quan_ts_outdata$month_year

global_pred_out_APAC_quan <- predict(lmfit_APAC_quan,data.frame(Month =timevals_out_APAC_quan))

fcast_APAC_quan <- global_pred_out_APAC_quan

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec_APAC_quan <- accuracy(fcast_APAC_quan,APAC_Con_quan_ts_outdata$monthly_qty)[5]
MAPE_class_dec_APAC_quan
#[1] 22.92209
#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred_APAC_quan <- c(ts(global_pred_APAC_quan),ts(global_pred_out_APAC_quan))
plot(APAC_Con_quan_ts, col = "black")
lines(class_dec_pred_APAC_quan, col = "red")

#So, that was classical decomposition,

########End of classical decomposition of APAC Consumer Quantity data####################################################
#                                        ARIMA MODEL
###########################################################################################################################################

#now let's do an ARIMA fit

autoarima_APAC_quan <- auto.arima(APAC_quan_timeser)
autoarima_APAC_quan
tsdiag(autoarima_APAC_quan)
plot(autoarima_APAC_quan$x, col="black")
lines(fitted(autoarima_APAC_quan), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima_APAC_quan <- APAC_quan_timeser - fitted(autoarima_APAC_quan)

adf.test(resi_auto_arima_APAC_quan,alternative = "stationary")
kpss.test(resi_auto_arima_APAC_quan)

#Also, let's evaluate the model using MAPE
fcast_auto_arima_APAC_quan <- predict(autoarima_APAC_quan, n.ahead = 6)

MAPE_auto_arima_APAC_quan <- accuracy(fcast_auto_arima_APAC_quan$pred,APAC_Con_quan_ts_outdata$monthly_qty)[5]
MAPE_auto_arima_APAC_quan
#[1] 26.22708
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_APAC_quan <- c(fitted(autoarima_APAC_quan),ts(fcast_auto_arima_APAC_quan$pred))
plot(APAC_Con_quan_ts, col = "black")
lines(auto_arima_pred_APAC_quan, col = "red")


#So from the above two models we conclude that , Classical Decomposition model is better and having good prediction on validation data set . 

####################################################################################################################
#                                        Modelling APAC Consumer sales data
####################################################################################################################
#Let's create the model using the first 42 rows.
#Then we can test the model on the remaining 6 rows later

APAC_Con_sales_ts_tot <- ts(APAC_Consumer$monthly_sales)
#Dividing into train and test data
APAC_Con_sales_ts_indata <- APAC_Consumer[1:42,]
APAC_Con_sales_ts_outdata <- APAC_Consumer[43:48,]
#Plotting the indata TS
APAC_Con_sales_ts <- ts(APAC_Con_sales_ts_indata$monthly_sales)
plot(APAC_Con_sales_ts)

# Smoothing the APAC Consumer Quantity

smoothed_APAC_sales <- smoothing_fun(APAC_Con_sales_ts )

#Plot the smoothed time series

timevals_in <- APAC_Con_sales_ts_indata$month_year
lines(smoothed_APAC_sales, col="blue", lwd=2)


#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

APAC_smootheddf <- as.data.frame(cbind(timevals_in, as.vector(smoothed_APAC_sales)))
colnames(APAC_smootheddf) <- c('Month', 'Sales')

#Now, let's fit a Additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.5*Month)+poly(Month,1), data=APAC_smootheddf)
global_pred <- predict(lmfit, Month=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)
summary(lmfit)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- APAC_Con_sales_ts-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months


timevals_out <- APAC_Con_sales_ts_outdata$month_year

global_pred_out <- predict(lmfit,data.frame(Month =timevals_out))

fcast <- global_pred_out

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,APAC_Con_sales_ts_outdata$monthly_sales)[5]
MAPE_class_dec
#[1] 21.13183

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(global_pred),ts(global_pred_out))
plot(APAC_Con_sales_ts_tot, col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, 
#####################################End of classical decomposition of APAC Consumer Sales############################
#now let's do an ARIMA fit
autoarima <- auto.arima(APAC_Con_sales_ts)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- APAC_Con_sales_ts - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 6)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred,APAC_Con_sales_ts_outdata$monthly_sales)[5]
MAPE_auto_arima
#[1] 27.66925
#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(APAC_Con_sales_ts_tot, col = "black")
lines(auto_arima_pred, col = "red")

#So from the above two models we conclude that , Classical Decomposition model is better and having good prediction on validation data set . 


######################################################################################################################
#                                     Prediction For the Next 6 Months
###########################################################################################################################

EU_Con_sales_total <- ts(EU_Consumer$monthly_sales)
plot(EU_Con_sales_total)

#Smoothing the total EU sales time series
smoothed_EU_sales_total <- smoothing_fun(EU_Con_sales_total)

# Plotting the smoothed time series of EU_Consumer_sales
EU_Con_sales_timevals_total <- EU_Consumer$month_year
lines(smoothed_EU_sales_total,col="blue",lwd=2)


EU_sales_smootheddf_total <- as.data.frame(cbind(EU_Con_sales_timevals_total, as.vector(smoothed_EU_sales_total)))
colnames(EU_sales_smootheddf_total) <- c('Month', 'Sales')


#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_sales_total <- lm(Sales ~ sin(0.5*Month) + cos(0.5*Month) + poly(Month,3) ,  data=EU_sales_smootheddf_total)
global_pred_EU_sales_total <- predict(lmfit_EU_sales_total, Month=EU_Con_sales_timevals_total)
summary(global_pred_EU_sales_total)
lines(EU_Con_sales_timevals_total, global_pred_EU_sales_total, col='green', lwd=2)

future_months <- as.data.frame(c(49:54))
colnames(future_months)<- c("Month")

future_EU_sales <- predict(lmfit_EU_sales_total,newdata=future_months)
future_EU_sales



#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_sales_total <- EU_Con_sales_total-global_pred_EU_sales_total
plot(local_pred_EU_sales_total, col='red', type = "l")
acf(local_pred_EU_sales_total)
acf(local_pred_EU_sales_total, type="partial")
armafit <- auto.arima(local_pred_EU_sales_total)
armafit
## as it is arima(0,0,0)
## we can say that its not havinfg any autoregressive behaviour left in local part.

#As it is white noise and stationary ,so the local time series model dosen't have autoregressiveness  . 

resi <- local_pred_EU_sales_total-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

# Hence the Forcasted 6 months values are 
future_EU_sales
future_EU_sales_df <- cbind(future_months,future_EU_sales)
EU_Consumer_past <- EU_Consumer[, c(7,4)]
colnames(future_EU_sales_df) <- c("month_year", "monthly_sales")
EU_Consumer_tot1 <- rbind(EU_Consumer_past,future_EU_sales_df)
plot(EU_Consumer_tot1, col = "black", type = "l")
abline(v=48, col = "red")

# Forecasting the values for EU Consumer Quantity
EU_Con_quantity_total <- ts(EU_Consumer$monthly_qty)
plot(EU_Con_quantity_total)

#Smoothing the total EU Quantity time series
smoothed_EU_quantity_total <- smoothing_fun(EU_Con_quantity_total)

# Plotting the smoothed time series of EU_Consumer_sales
EU_Con_Quantity_timevals_total <- EU_Consumer$month_year
lines(smoothed_EU_quantity_total,col="blue",lwd=2)


EU_quantity_smootheddf_total <- as.data.frame(cbind(EU_Con_Quantity_timevals_total, as.vector(smoothed_EU_quantity_total)))
colnames(EU_quantity_smootheddf_total) <- c('Month', 'Quantity')


#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_EU_quantity_total <- lm(Quantity ~ sin(0.5*Month) + cos(0.5*Month) + poly(Month,4) ,  data=EU_quantity_smootheddf_total)
global_pred_EU_quantity_total <- predict(lmfit_EU_quantity_total, Month=EU_Con_Quantity_timevals_total)
summary(global_pred_EU_quantity_total)
lines(EU_Con_Quantity_timevals_total, global_pred_EU_quantity_total, col='green', lwd=2)

future_months <- as.data.frame(c(49:54))
colnames(future_months)<- c("Month")

future_EU_quantity <- predict(lmfit_EU_quantity_total,newdata=future_months)
future_EU_quantity



#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_EU_quantity_total <- EU_Con_quantity_total-global_pred_EU_quantity_total
plot(local_pred_EU_quantity_total, col='red', type = "l")
acf(local_pred_EU_quantity_total)
acf(local_pred_EU_quantity_total, type="partial")
armafit <- auto.arima(local_pred_EU_quantity_total)

tsdiag(armafit)
##  from acf of residuals after autoarima modelling we can say it is white noise 
armafit
## ar(2)
# it menas its has some auto regressiveness left which can be modelled
. 

resi <- local_pred_EU_quantity_total-fitted(armafit)

adf.test(resi,alternative = "stationary")

kpss.test(resi)
## forecasting for local part from ar(2)
arma_pred = predict(armafit,n.ahead = 6)
# Hence the Forcasted 6 months values are combination local and global pred 
local_qnty_pred=arma_pred$pred
#local_qnty_pred=as.data.frame(local_qnty_pred)
future_EU_quantity=future_EU_quantity+local_qnty_pred
future_EU_quantity
future_EU_quantity_df <- cbind(future_months,as.data.frame(future_EU_quantity))
EU_Consumer_past <- EU_Consumer[, c(7,5)]
colnames(future_EU_quantity_df) <- c("month_year", "monthly_qty")
EU_Consumer_tot1 <- rbind(EU_Consumer_past,future_EU_quantity_df)
#auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(EU_Consumer_tot1$monthly_qty, col = "black", type = "l")
abline(v=48, col = "red")




# Forecasting for APAC Consumer Sales

APAC_Con_sales_total <- ts(APAC_Consumer$monthly_sales)
plot(APAC_Con_sales_total)

#Smoothing the total APAC sales time series
smoothed_APAC_sales_total <- smoothing_fun(APAC_Con_sales_total)

# Plotting the smoothed time series of APAC_Consumer_sales
APAC_Con_sales_timevals_total <- APAC_Consumer$month_year
lines(smoothed_APAC_sales_total,col="blue",lwd=2)


APAC_sales_smootheddf_total <- as.data.frame(cbind(APAC_Con_sales_timevals_total, as.vector(smoothed_APAC_sales_total)))
colnames(APAC_sales_smootheddf_total) <- c('Month', 'Sales')


#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APAC_sales_total <- lm(Sales ~ sin(0.5*Month)+poly(Month,1),  data=APAC_sales_smootheddf_total)
global_pred_APAC_sales_total <- predict(lmfit_APAC_sales_total, Month=APAC_Con_sales_timevals_total)
summary(global_pred_APAC_sales_total)
lines(APAC_Con_sales_timevals_total, global_pred_APAC_sales_total, col='green', lwd=2)

future_months <- as.data.frame(c(49:54))
colnames(future_months)<- c("Month")

future_APAC_sales <- predict(lmfit_APAC_sales_total,newdata=future_months)
future_APAC_sales



#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_sales_total <- APAC_Con_sales_total-global_pred_APAC_sales_total
plot(local_pred_APAC_sales_total, col='red', type = "l")
acf(local_pred_APAC_sales_total)
acf(local_pred_APAC_sales_total, type="partial")
armafit <- auto.arima(local_pred_APAC_sales_total)

tsdiag(armafit)
armafit
## means local part is white noise from the acf of residuals!
#As it is white noise and stationary ,so the local time series model dosen't have autoregressiveness  . 

resi <- local_pred_APAC_sales_total-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)

# Hence the Forcasted 6 months values are 
future_APAC_sales
future_APAC_sales_df <- cbind(future_months,future_APAC_sales)
APAC_Consumer_past <- APAC_Consumer[, c(7,4)]
colnames(future_APAC_sales_df) <- c("month_year", "monthly_sales")
APAC_Consumer_tot1 <- rbind(APAC_Consumer_past,future_APAC_sales_df)
plot(APAC_Consumer_tot1, col = "black", type = "l")
abline(v=48, col = "red")



# Forecasting for APAC Consumer Quantity

APAC_Con_quantity_total <- ts(APAC_Consumer$monthly_qty)
plot(APAC_Con_quantity_total)

#Smoothing the total APAC Quantity time series
smoothed_APAC_quantity_total <- smoothing_fun(APAC_Con_quantity_total)

# Plotting the smoothed time series of APAC_Consumer_sales
APAC_Con_Quantity_timevals_total <- APAC_Consumer$month_year
lines(smoothed_APAC_quantity_total,col="blue",lwd=2)


APAC_quantity_smootheddf_total <- as.data.frame(cbind(APAC_Con_Quantity_timevals_total, as.vector(smoothed_APAC_quantity_total)))
colnames(APAC_quantity_smootheddf_total) <- c('Month', 'Quantity')


#Now, let's fit a additive model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit_APAC_quantity_total <- lm(Quantity ~ sin(0.5*Month)+poly(Month,1)+cos(0.5*Month) ,  data=APAC_quantity_smootheddf_total)
global_pred_APAC_quantity_total <- predict(lmfit_APAC_quantity_total, Month=APAC_Con_Quantity_timevals_total)
summary(global_pred_APAC_quantity_total)
lines(APAC_Con_Quantity_timevals_total, global_pred_APAC_quantity_total, col='green', lwd=2)

future_months <- as.data.frame(c(49:54))
colnames(future_months)<- c("Month")

future_APAC_quantity <- predict(lmfit_APAC_quantity_total,newdata=future_months)
future_APAC_quantity



#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred_APAC_quantity_total <- APAC_Con_quantity_total-global_pred_APAC_quantity_total
plot(local_pred_APAC_quantity_total, col='red', type = "l")
acf(local_pred_APAC_quantity_total)
acf(local_pred_APAC_quantity_total, type="partial")
armafit <- auto.arima(local_pred_APAC_quantity_total)

tsdiag(armafit)
## from acf of residuals in plot it seems as white noise
armafit
## arima(0,0,0) it confirms that its a noise.

#As it is white noise and stationary ,so the local time series model dosen't have autoregressiveness  . 

resi <- local_pred_APAC_quantity_total-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)
arma_pred = predict(armafit,n.ahead = 6)
# Hence the Forcasted 6 months values are 
future_APAC_quantity
future_APAC_quantity_df <- cbind(future_months,future_APAC_quantity)
APAC_Consumer_past <- APAC_Consumer[, c(7,5)]
colnames(future_APAC_quantity_df) <- c("month_year", "monthly_qty")
APAC_Consumer_tot1 <- rbind(APAC_Consumer_past,future_APAC_quantity_df)
#auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(APAC_Consumer_tot1$monthly_qty, col = "black", type = "l")
abline(v=48, col = "red")


# Conclusion:
#   1. Based on data provided we helped "Global Mart" in identifying 2 most profitable
# market segments as APAC Consumer and EU Consumer.
# 2. 4 key forecasts on test data are as follows:
#   a. APAC Consumer Sales is likely to rise in next 6 months with small
# fluctuations.
# b. APAC Consumer Quantity is likely to rise steeply in coming 6 months.
# c. EU Consumer Sales may show the slow rise in coming months.
# d. EU Consumer Quantity is likely to drop during initial 1 or 2 months & then
# rise rapidly in next 3 months, eventually reaching a plateau.