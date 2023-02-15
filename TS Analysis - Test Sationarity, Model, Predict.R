## read & format data
sales <- read.csv("USA FIRM SALES DATA.csv", header = TRUE)

salesbu1 <- ts(sales$BU1, start = c(2015, 2), end = c(2017,12), frequency = 12)
salesbu2 <- ts(sales$BU2, start = c(2015, 2), end = c(2017,12), frequency = 12)
salesbu3 <- ts(sales$BU3, start = c(2015, 2), end = c(2017,12), frequency = 12)


## 1.1 stationarity check

## a. plot
plot(salesbu1)
plot(salesbu2)
plot(salesbu3)

# all plots show a trend which contradict stationarity

## b. Autocorrelation
acf(salesbu1)
acf(salesbu2)
acf(salesbu3)

# for each BU we observe a slow decay in autocorrelations which contradict 
# stationarity

## c. Dickey-Fuller tests:

library(urca)
df1 <- ur.df(salesbu1)
summary(df1)

# test statistic = 0.9383 which is superior to 5% value (-1.95) so we fail to 
# reject the null hypothesis of non-stationarity for BU1 sales

df2 <- ur.df(salesbu2)
summary(df2)

# test statistic = 3.0436 which is superior to 5% value (-1.95) so we fail to 
# reject the null hypothesis of non-stationarity for BU2 sales

df3 <- ur.df(salesbu3)
summary(df3)

# test statistic = 3.5216 which is superior to 5% value (-1.95) so we fail to 
# reject the null hypothesis of non-stationarity for BU3 sales

## 1.2 Differenciating:

library(forecast)
ndiffs(salesbu1)
ndiffs(salesbu2)
ndiffs(salesbu3)

# order of integration = 1 for all time series is recommended 


## 2.1 Model Identification:

diff1 <- diff(salesbu1, differences = 1)
acf(diff1)
pacf(diff1)

diff2 <- diff(salesbu2, differences = 1)
acf(diff2)
pacf(diff2)

diff3 <- diff(salesbu3, differences = 1)
acf(diff3)
pacf(diff3)

# PACF suggest that the Auto Regressive part order will be 1 for all 3 BUs
# ACF suggest that the Auto Regressive part order will be 0 for all 3 BUs

## 2.2 Parameters Identification:

salesbu1Mod <- auto.arima(salesbu1, d=1, max.p = 2, max.q = 2, trace=TRUE, ic="aic")
salesbu2Mod <- auto.arima(salesbu2, d=1, max.p = 2, max.q = 2, trace=TRUE, ic="aic")
salesbu3Mod <- auto.arima(salesbu3, d=1, max.p = 2, max.q = 2, trace=TRUE, ic="aic")

# Best model for all 3 time series is confirmed to be (p,q,d): ARIMA(0,1,1) 
# as discussed previously. Models will have the following coefficients:

coef(salesbu1Mod)
coef(salesbu2Mod)
coef(salesbu3Mod)


## 3. Diagnostic checking:

# We expect to have residuals with properties of a white noise. Let's check:
resi1 <- residuals(salesbu1Mod)
resi2 <- residuals(salesbu2Mod)
resi3 <- residuals(salesbu3Mod)

Box.test(resi1)
# p-value = 0.7099 which is > 5% so we fail to reject that residuals are white noise

Box.test(resi2)
# p-value = 0.2333 which is > 5% so we fail to reject that residuals are white noise

Box.test(resi3)
# p-value = 0.09373 which is > 5% so we fail to reject that residuals are white noise

# This validates that our 3 models are adequate representations of the data generating process 


## 4. Forecasting

predict(salesbu1Mod, n.ahead = 3)
#           Jan      Feb      Mar
# 2018 137.9893 137.9893 137.9893

predict(salesbu2Mod, n.ahead = 3)
#           Jan      Feb      Mar
# 2018 131.6501 131.6501 131.6501

predict(salesbu3Mod, n.ahead = 3)
#           Jan      Feb      Mar
# 2018 127.6788 127.6788 127.6788
