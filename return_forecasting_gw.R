#Daniel Szabo 

rm(list = ls())

library(data.table)
library(ggplot2)
library(forecast)
library(vars)

monthly <- read.csv("PredictorData2021 - Monthly.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
monthly <- as.data.table(monthly)
monthly$Index <- as.numeric(gsub(",","", monthly$Index))
monthly$yyyymm <- as.Date(paste0(as.character(monthly$yyyymm), "01"), format = "%Y%m%d")

#quarterly <- read.csv("PredictorData2021 - Quarterly.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
#quarterly <- as.data.table(quarterly)
#quarterly$Index <- as.numeric(gsub(",","", quarterly$Index))
#Convert date here

annual <- read.csv("PredictorData2021 - Annual.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
annual <- as.data.table(annual)
annual$Index <- as.numeric(gsub(",","",annual$Index))

#Calculate/Transform Annual variables to conform with Goyal-Welch data (from christophj.github.io)

annual <- annual[, IndexDiv := Index + D12]
annual <- annual[, dp := log(D12) - log(Index)] #Dividend Price Ratio
annual <- annual[, ep := log(E12) - log(Index)] 

vec_dy <- c(NA, annual[2:nrow(annual), log(D12)] - annual[1:(nrow(annual)-1), log(Index)])
annual <- annual[, dy := vec_dy]
annual <- annual[, logret := c(NA,diff(log(Index)))]

vec_logretdiv <- c(NA, annual[2:nrow(annual), log(IndexDiv)] - annual[1:(nrow(annual)-1), log(Index)])
vec_logretdiv <- c(NA, log(annual[2:nrow(annual), IndexDiv]/annual[1:(nrow(annual)-1), Index]))

annual <- annual[, logretdiv := vec_logretdiv]
annual <- annual[, logRfree := log(Rfree + 1)]
annual <- annual[, rp_div   := logretdiv - logRfree]

#Time Series
ts_annual <- ts(annual, start=annual[1, yyyy], end=annual[nrow(annual), yyyy])
plot(ts_annual[, c("rp_div", "dp", "dy")])

################################################################################
#Subset data into training and test - use later
train <- subset(ts_annual, end = 120)
test <- subset(ts_annual, start = 121, end = 151)

#Models on annual data (basic for now just to familiarize myself with the syntax & graphs)
annual_index <- ts_annual[,"IndexDiv"]

#Naive Model
fc_naive <- naive(annual_index, h = 5)
summary(fc_naive)
checkresiduals(fc_naive)

#Simple Exponential Smoothing
fc_ses <- ses(annual_index, h = 5)
summary(fc_ses)
checkresiduals(fc_ses)

#Holt Model
fc_holt <- holt(annual_index, h = 5)
summary(fc_holt)
checkresiduals(fc_holt)

#ARIMA Model
fc_arima <- auto.arima(annual_index)
summary(fc_arima)
checkresiduals(fc_arima)

#TBATS Model
fc_tbats <- tbats(annual_index)
summary(fc_tbats)
checkresiduals(fc_tbats)

#Basic exploratory plots of raw index returns
ggplot(monthly, aes(x = yyyymm, y = Index)) + geom_line()
ggplot(annual, aes(x = yyyy, y = Index)) + geom_line()



