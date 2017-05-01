library(dplyr)
library(lubridate)
library(zoo)
library(raster)
library(forecast)
library(xts)
################Checkpoint 1############

sales <- read.csv("Global Superstore.csv",stringsAsFactors = F)


sum(is.na(sales)) #41296
sum(is.na(sales$Postal.Code)) #41296


summary(sales$Sales)
str(sales)
sales$Market <- as.factor(sales$Market)
sales$Segment <- as.factor(sales$Segment)

# Making subsets of complete dataset using the 
# 7 factor levels of "Market" and 3 factor levels of "Segment"

subsets <- split(sales, with(sales, interaction(sales$Market,sales$Segment)), drop = TRUE)

#creation of a list for aggregating transaction lvel data
transaction_data <- list()
subset_index <- vector(mode="list", length=length(subsets))

subset_index <- as.data.frame(matrix(0, ncol = 2, nrow = length(subsets)))
col.names = c("Market", "Segment")
colnames(subset_index)<- col.names

transaction_data_cv <- as.data.frame(matrix(0, ncol = 3, nrow = length(subsets)))
col.names = c("Market", "Segment", "cv")
colnames(transaction_data_cv)<- col.names

# Aggregation of transaction level data and
# calculation of coeffifient of varioation for each of the segment
for (count in 1:length(subsets))
{
  subset_index[count,]<- c(as.character(subsets[[count]]$Market[1]),as.character(subsets[[count]]$Segment[1]) )
  df <- subsets[[count]]
  df$Order.Date <- as.Date(df$Order.Date,"%d-%m-%Y")
  df$year <- (strftime(df$Order.Date,"%Y"))
  df$month <- (strftime(df$Order.Date,"%m"))
  df$Order.Date <- paste(month.abb[as.numeric(df$month)],df$year, sep=" ")
  df$Order.Date <- as.yearmon(df$Order.Date )
  df$Order.Date <- as.Date(df$Order.Date )
  #aggregated transaction level data
  transaction_data[[count]] <- aggregate(cbind(Sales, Profit,Quantity)~Order.Date, data=df, sum, na.rm=TRUE)
  #calculation of coefficient of variation
  transaction_data_cv[count,]<- c(as.character(subsets[[count]]$Market[1]),as.character(subsets[[count]]$Segment[1]), as.numeric(cv(transaction_data[[count]]$Profit)))
}
transaction_data_cv$cv <- as.numeric(transaction_data_cv$cv)

# From the data frame transaction_data_cv, 
#following are the top 5 segments based on co-efficient of Variation(least cv)
# EU,Consumer    #5
# APAC,Consumer  #2
# LATAM,Consumer #6
# APAC,Corporate #9
# EU,Corporate   #12

Segment1_EU_Con <- transaction_data[[5]]
Segment2_APAC_Con <- transaction_data[[2]]
Segment3_LATAM_Con <- transaction_data[[6]]
Segment4_APAC_Cor <- transaction_data[[9]]
Segment5_EU_cor <- transaction_data[[12]]

View(transaction_data[[5]])
str(transaction_data[[5]])

#separating out last 6 months data for out of sample testing
validation_Segment1_EU_Con    <- Segment1_EU_Con[43:48,]
validation_Segment1_EU_Con$Month <- c(43:48)
validation_Segment2_APAC_Con  <- Segment2_APAC_Con[43:48,]
validation_Segment2_APAC_Con$Month <- c(43:48)
validation_Segment3_LATAM_Con <- Segment3_LATAM_Con[43:48,]
validation_Segment3_LATAM_Con$Month <- c(43:48)
validation_Segment4_APAC_Cor  <- Segment4_APAC_Cor[43:48,]
validation_Segment4_APAC_Cor$Month <- c(43:48)
validation_Segment5_EU_cor    <- Segment5_EU_cor[43:48,]
validation_Segment5_EU_cor$Month <- c(43:48)

Segment1_EU_Con <- Segment1_EU_Con[1:42,]
Segment1_EU_Con$Month <- c(1:42)
Segment2_APAC_Con <- Segment2_APAC_Con[1:42,]
Segment2_APAC_Con$Month <- c(1:42)
Segment3_LATAM_Con <- Segment3_LATAM_Con[1:42,]
Segment3_LATAM_Con$Month <- c(1:42)
Segment4_APAC_Cor <- Segment4_APAC_Cor[1:42,]
Segment4_APAC_Cor$Month <- c(1:42)
Segment5_EU_cor <- Segment5_EU_cor[1:42,]
Segment5_EU_cor$Month <- c(1:42)
########End of Checkpoint 1####################

#########Checkpoint 2#####################

# Creation of time series objects
##### Segment1_EU_Con 
Segment1_EU_Con.ts <- ts(Segment1_EU_Con$Profit)
plot(Segment1_EU_Con.ts)
width <- 3
smoothedseries <- stats::filter(Segment1_EU_Con.ts, filter=rep(1/(2*width+1),(2*width+1)),
                         method="convolution", sides=2)

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}
n <- length(Segment1_EU_Con.ts)
timevals <- Segment1_EU_Con$Month
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

lines(smoothedseries, col="green", lwd=2)
plot(smoothedseries)

smootheddf <- as.data.frame(cbind(1:42, as.vector(smoothedseries)))
colnames(smootheddf) <- c('month', 'profit')

lmfit1 <- lm(Segment1_EU_Con$Profit ~  sin(0.5*Segment1_EU_Con$Month) * poly(Segment1_EU_Con$Month,2) 
              + cos(0.5*Segment1_EU_Con$Month) * poly(Segment1_EU_Con$Month,2)
              + Segment1_EU_Con$Month * poly(Segment1_EU_Con$Month,2), data=Segment1_EU_Con)

timevals <- c(1:42)
trend1 <- predict(lmfit1, data.frame(x=timevals))
lines(timevals, trend1, col='red', lwd=2)

### Checking whether the residue left is white noise or not
### Clearly from the ACF and PACF plots we can observe the residue is indeed just white noise

resi <- Segment1_EU_Con.ts - trend1
plot(resi, col='red')
acf(resi)
acf(resi, type="partial")

armafit <- auto.arima(resi)

tsdiag(armafit)
armafit

### Series: resi 
### ARIMA(0,0,0) with zero mean
### sigma^2 estimated as 3270253:  log likelihood=-374.6
### AIC=751.21   AICc=751.31   BIC=752.94

autoarima1 <- auto.arima(Segment1_EU_Con.ts)
autoarima1
tsdiag(autoarima1)
plot(autoarima1$x, col="black")
lines(fitted(autoarima1), col="red")

### sigma^2 estimated as 5944159:  log likelihood=-377.15
### AIC=760.29   AICc=760.94   BIC=765.43

### Conlusion:By comparing log likelihood, AIC,AICc,BIC, The regression model better describes the series than auto arima



##################Validation#################

#### Forecasting profit for next 6 months using the model developed using classical decomposition method

forecast1_lm <- forecast(object = trend1,h=6)
forecast1_lm
accuracy(forecast1_lm)

###                    ME     RMSE      MAE       MPE     MAPE     MASE      ACF1
### Training set 1.218586 262.3696 183.0547 -3.751623 10.11497 0.464809 0.4633147

### Forecasting profict for next 6 months using the model developed using autoarima

forecast1 <- forecast(object = autoarima1,h=6)
forecast1
accuracy(forecast1)

###                    ME     RMSE      MAE       MPE     MAPE      MASE        ACF1
### Training set 245.5028 2349.378 1970.612 -34.22787 80.31336 0.8435899 -0.03680115

##############################################

##### Segment2_APAC_Con
Segment2_APAC_Con.ts <- ts(Segment2_APAC_Con$Profit)
plot(Segment2_APAC_Con.ts)
width <- 3
smoothedseries <- stats::filter(Segment2_APAC_Con.ts, filter=rep(1/(2*width+1),(2*width+1)),
                                method="convolution", sides=2)

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}
n <- length(Segment2_APAC_Con.ts)
timevals <- Segment2_APAC_Con$Month
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

lines(smoothedseries, col="green", lwd=2)
plot(smoothedseries)

smootheddf <- as.data.frame(cbind(1:42, as.vector(smoothedseries)))

colnames(smootheddf) <- c('month', 'profit')
lmfit2 <- lm(Segment2_APAC_Con$Profit ~  sin(0.5*Segment2_APAC_Con$Month) * poly(Segment2_APAC_Con$Month,2) 
             + cos(0.5*Segment2_APAC_Con$Month) * poly(Segment2_APAC_Con$Month,2)
             + Segment2_APAC_Con$Month * poly(Segment2_APAC_Con$Month,3)  , data=Segment2_APAC_Con)

timevals <- c(1:42)
trend2 <- predict(lmfit2, data.frame(x=timevals))
lines(timevals, trend2, col='red', lwd=2)

### Checking whether the residue left is white noise or not
### Clearly from the ACF and PACF plots we can observe the residue is indeed just white noise

resi <- Segment2_APAC_Con.ts - trend2
plot(resi, col='red')
acf(resi)
acf(resi, type="partial")

armafit <- auto.arima(resi)

tsdiag(armafit)
armafit

### sigma^2 estimated as 2666661:  log likelihood=-368.46
### AIC=748.92   AICc=751.32   BIC=759.35

autoarima2 <- auto.arima(Segment2_APAC_Con.ts)
autoarima2
tsdiag(autoarima2)
plot(autoarima2$x, col="black")
lines(fitted(autoarima2), col="red")

### sigma^2 estimated as 6614195:  log likelihood=-380.39
### AIC=764.78   AICc=765.1   BIC=768.21

### Conlusion:By comparing log likelihood, AIC,AICc,BIC, The regression model better describes the series than auto arima


##################Validation#################

#### Forecasting profit for next 6 months using the model developed using classical decomposition method

forecast2_lm <- forecast(object = trend2,h=6)
forecast2_lm
accuracy(forecast2_lm)

###                  ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
####Training set -3.24295 274.8822 227.4339 0.7290383 5.753848 0.5751444 0.6946061

### Forecasting profict for next 6 months using the model developed using autoarima

forecast2 <- forecast(object = autoarima2,h=6)
forecast2
accuracy(forecast2)

###                  ME     RMSE      MAE       MPE     MAPE     MASE       ACF1
### Training set 566.9791 2509.827 1944.388 -103.4098 143.6428 0.689497 -0.1769714

##############################################

##### Segment3_LATAM_Con
Segment3_LATAM_Con.ts <- ts(Segment3_LATAM_Con$Profit)
plot(Segment3_LATAM_Con.ts)
width <- 1
smoothedseries <- stats::filter(Segment3_LATAM_Con.ts, filter=rep(1/(2*width+1),(2*width+1)),
                                method="convolution", sides=2)

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}
n <- length(Segment3_LATAM_Con.ts)
timevals <- Segment3_LATAM_Con$Month
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="green", lwd=2)

plot(smoothedseries)

smootheddf <- as.data.frame(cbind(1:42, as.vector(smoothedseries)))

colnames(smootheddf) <- c('month', 'profit')
lmfit3 <- lm(Segment3_LATAM_Con$Profit ~ sin(1*Segment3_LATAM_Con$Month) * poly(Segment3_LATAM_Con$Month,2) 
            + cos(1*Segment3_LATAM_Con$Month) * poly(Segment3_LATAM_Con$Month,2)  
            + Segment3_LATAM_Con$Month * poly(Segment3_LATAM_Con$Month,1)   , data=Segment3_LATAM_Con)
timevals <- c(1:42)
trend3 <- predict(lmfit3, data.frame(x=timevals))
lines(timevals, trend3, col='red', lwd=2)

### Checking whether the residue left is white noise or not
### Clearly from the ACF and PACF plots we can observe the residue is indeed just white noise
resi <- Segment3_LATAM_Con.ts - trend3
plot(resi, col='red')
acf(resi)
acf(resi, type="partial")

armafit <- auto.arima(resi)

tsdiag(armafit)
armafit

### sigma^2 estimated as 1120210:  log likelihood=-352.1
### AIC=706.21   AICc=706.31   BIC=707.95

autoarima3 <- auto.arima(Segment3_LATAM_Con.ts)
autoarima3
tsdiag(autoarima3)
plot(autoarima3$x, col="black")
lines(fitted(autoarima3), col="red")

### sigma^2 estimated as 2325148:  log likelihood=-366.44
### AIC=738.89   AICc=739.52   BIC=744.1

### Conlusion:By comparing log likelihood, AIC,AICc,BIC, The regression model better describes the series than auto arima


##################Validation#################

#### Forecasting profit for next 6 months using the model developed using classical decomposition method

forecast3_lm <- forecast(object = trend3,h=6)
forecast3_lm
accuracy(forecast3_lm)

###                  ME     RMSE      MAE       MPE     MAPE      MASE     ACF1
### Training set 73.54604 950.3151 764.6373 -9.320183 40.10392 0.9777157 0.551457

### Forecasting profict for next 6 months using the model developed using autoarima

forecast3 <- forecast(object = autoarima3,h=6)
forecast3
accuracy(forecast3)

###                   ME     RMSE      MAE       MPE     MAPE      MASE       ACF1
### Training set -0.2833304 1488.095 1198.039 -52.15378 112.0082 0.8035563 0.02682706

##############################################


##### Segment4_APAC_Cor
Segment4_APAC_Cor.ts <- ts(Segment4_APAC_Cor$Profit)
plot(Segment4_APAC_Cor.ts)
width <- 2
smoothedseries <- stats::filter(Segment4_APAC_Cor.ts, filter=rep(1/(2*width+1),(2*width+1)),
                                method="convolution", sides=2)

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}
n <- length(Segment4_APAC_Cor.ts)
timevals <- Segment4_APAC_Cor$Month
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}

lines(smoothedseries, col="green", lwd=2)

plot(smoothedseries)

smootheddf <- as.data.frame(cbind(1:42, as.vector(smoothedseries)))

colnames(smootheddf) <- c('month', 'profit')
lmfit4 <- lm(Segment4_APAC_Cor$Profit ~ sin(0.5*Segment4_APAC_Cor$Month) * poly(Segment4_APAC_Cor$Month,2) 
            + cos(0.5*Segment4_APAC_Cor$Month) * poly(Segment4_APAC_Cor$Month,2), data=Segment4_APAC_Cor)
timevals <- c(1:42)
trend4 <- predict(lmfit4, data.frame(x=timevals))
lines(timevals, trend4, col='red', lwd=2)

### Checking whether the residue left is white noise or not
### Clearly from the ACF and PACF plots we can observe the residue is indeed just white noise
resi <- Segment4_APAC_Cor.ts - trend4
plot(resi, col='red')
acf(resi)
acf(resi, type="partial")

armafit <- auto.arima(resi)

tsdiag(armafit)
armafit

### sigma^2 estimated as 2364314:  log likelihood=-367.43
### AIC=738.87   AICc=739.17   BIC=742.34

autoarima4 <- auto.arima(Segment4_APAC_Cor.ts)
autoarima4
tsdiag(autoarima4)
plot(autoarima4$x, col="black")
lines(fitted(autoarima4), col="red")

### sigma^2 estimated as 3588074:  log likelihood=-376.05
### AIC=756.09   AICc=756.4   BIC=759.57

### Conlusion:By comparing log likelihood, AIC,AICc,BIC, The regression model better describes the series than auto arima


##################Validation#################

#### Forecasting profit for next 6 months using the model developed using classical decomposition method

forecast4_lm <- forecast(object = trend4,h=6)
forecast4_lm
accuracy(forecast4_lm)

###                   ME     RMSE      MAE      MPE     MAPE      MASE      ACF1
### Training set 28.98941 483.5275 380.7558 -1.31941 16.38744 0.9767926 0.7316337

### Forecasting profict for next 6 months using the model developed using autoarima

forecast4 <- forecast(object = autoarima4,h=6)
forecast4
accuracy(forecast4)

###                     ME     RMSE     MAE       MPE     MAPE      MASE        ACF1
### Training set -6.171518e-13 1871.535 1512.47 -625.2072 751.8533 0.6732498 -0.07473241

##############################################


##### Segment5_EU_cor
Segment5_EU_cor.ts <- ts(Segment5_EU_cor$Profit)
plot(Segment5_EU_cor.ts)
width <- 1
smoothedseries <- stats::filter(Segment5_EU_cor.ts, filter=rep(1/(2*width+1),(2*width+1)),
                                method="convolution", sides=2)

diff <- smoothedseries[width+2] - smoothedseries[width+1]
for (i in seq(width,1,-1)) {
  smoothedseries[i] <- smoothedseries[i+1] - diff
}
n <- length(Segment5_EU_cor.ts)
timevals <- Segment5_EU_cor$Month
diff <- smoothedseries[n-width] - smoothedseries[n-width-1]
for (i in seq(n-width+1, n)) {
  smoothedseries[i] <- smoothedseries[i-1] + diff
}
lines(smoothedseries, col="green", lwd=2)

plot(smoothedseries)

smootheddf <- as.data.frame(cbind(1:42, as.vector(smoothedseries)))

colnames(smootheddf) <- c('month', 'profit')
lmfit5 <- lm(Segment5_EU_cor$Profit ~ sin(0.5*Segment5_EU_cor$Month) * poly(Segment5_EU_cor$Month,2) 
            + cos(0.5*Segment5_EU_cor$Month) * poly(Segment5_EU_cor$Month,2) +sin(1*Segment5_EU_cor$Month) * poly(Segment5_EU_cor$Month,2) 
            + cos(1*Segment5_EU_cor$Month) * poly(Segment5_EU_cor$Month,2)
            + Segment5_EU_cor$Month * poly(Segment5_EU_cor$Month,2) , data=Segment5_EU_cor)
timevals <- c(1:42)
trend5 <- predict(lmfit5, data.frame(x=timevals))
lines(timevals, trend5, col='red', lwd=2)

### Checking whether the residue left is white noise or not
### Clearly from the ACF and PACF plots we can observe the residue is indeed just white noise
resi <- Segment5_EU_cor.ts - trend5
plot(resi, col='red')
acf(resi)
acf(resi, type="partial")

armafit <- auto.arima(resi)

tsdiag(armafit)
armafit
### sigma^2 estimated as 404252:  log likelihood=-330.09
### AIC=670.18   AICc=671.84   BIC=678.86

autoarima5 <- auto.arima(Segment5_EU_cor.ts)
autoarima5
tsdiag(autoarima5)
plot(autoarima5$x, col="black")
lines(fitted(autoarima5), col="red")

### sigma^2 estimated as 2468735:  log likelihood=-368.19
### AIC=740.39   AICc=740.69   BIC=743.86 

### Conlusion:By comparing log likelihood, AIC,AICc,BIC, The regression model better describes the series than auto arima


##################Validation#################

#### Forecasting profit for next 6 months using the model developed using classical decomposition method

forecast5_lm <- forecast(object = trend5,h=6)
forecast5_lm
accuracy(forecast5_lm)

###                    ME     RMSE      MAE       MPE     MAPE      MASE      ACF1
###  Training set 4.420893 626.7406 511.5626 -5.683914 27.48642 0.9987379 0.5496986

### Forecasting profict for next 6 months using the model developed using autoarima

forecast5 <- forecast(object = autoarima5,h=6)
forecast5
accuracy(forecast5)

###                    ME     RMSE     MAE       MPE     MAPE      MASE        ACF1
###Training set 2.153721e-09 1552.403 1174.36 -62.01764 118.1985 0.6737038 -0.09698577

##############################################

