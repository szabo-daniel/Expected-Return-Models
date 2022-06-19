#Daniel Szabo 

rm(list = ls())

library(data.table)
library(ggplot2)
library(forecast)
library(vars)
library(dyn)
library(reshape2)

monthly <- read.csv("PredictorData2021 - Monthly.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
monthly <- as.data.table(monthly)
monthly$Index <- as.numeric(gsub(",","", monthly$Index))
monthly$yyyymm <- as.Date(paste0(as.character(monthly$yyyymm), "01"), format = "%Y%m%d")

annual <- read.csv("PredictorData2021 - Annual.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
annual <- as.data.table(annual)
annual$Index <- as.numeric(gsub(",","",annual$Index))

#Calculate/Transform annual variables to conform with Goyal-Welch data (from christophj.github.io)

annual <- annual[, IndexDiv := Index + D12]
annual <- annual[, dp := log(D12) - log(Index)] #Dividend Price Ratio
annual <- annual[, ep := log(E12) - log(Index)] #Earnings Price Ratio

vec_dy <- c(NA, annual[2:nrow(annual), log(D12)] - annual[1:(nrow(annual)-1), log(Index)])
annual <- annual[, dy := vec_dy]
annual <- annual[, logret := c(NA,diff(log(Index)))]

vec_logretdiv <- c(NA, annual[2:nrow(annual), log(IndexDiv)] - annual[1:(nrow(annual)-1), log(Index)])
vec_logretdiv <- c(NA, log(annual[2:nrow(annual), IndexDiv]/annual[1:(nrow(annual)-1), Index]))

annual <- annual[, logretdiv := vec_logretdiv] 
annual <- annual[, logRfree := log(Rfree + 1)] #Log risk-free rate
annual <- annual[, rp_div   := logretdiv - logRfree] #Log Equity Premium

#Time Series
ts_annual <- ts(annual, start=annual[1, yyyy], end=annual[nrow(annual), yyyy])
plot(ts_annual[, c("rp_div", "dp", "dy")])

###############################################################################
#Subset data into training and test - use later
train <- subset(ts_annual, start = 2, end = 120)
test <- subset(ts_annual, start = 121, end = 151)

#Models on annual data (basic for now just to familiarize myself with the syntax & graphs)
an_rp_div <- ts(ts_annual[-1,"rp_div"])

#Naive Model
fc_naive <- naive(an_rp_div, h = 5)
summary(fc_naive)
checkresiduals(fc_naive) #p-val: 4.227e-08 
autoplot(forecast(fc_naive))

#Simple Exponential Smoothing
fc_ses <- ses(an_rp_div, h = 5)
summary(fc_ses)
checkresiduals(fc_ses) #p-val: 0.0142
autoplot(forecast(fc_ses))

#Holt Model
fc_holt <- holt(an_rp_div, h = 5)
summary(fc_holt)
checkresiduals(fc_holt) #p-val: 0.0032
autoplot(forecast(fc_holt))

#ARIMA Model
fc_arima <- auto.arima(an_rp_div)
summary(fc_arima)
checkresiduals(fc_arima) #p-val: 0.2817
autoplot(forecast(fc_arima, h = 10))

#TBATS Model
fc_tbats <- tbats(an_rp_div)
summary(fc_tbats)
checkresiduals(fc_tbats) #p-val: 0.0108
autoplot(forecast(fc_tbats, h = 5))





