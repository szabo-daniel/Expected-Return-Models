#Daniel Szabo

rm(list = ls())

library(data.table)
library(ggplot2)
library(forecast)
library(dyn)
library(reshape2)
library(readxl)
library(zoo)
library(corrplot)
library(ForecastComb)

#Import data (quarterly GW data updated thru 2021)
qdata <- fread("PredictorData2021 - Quarterly.csv", na.strings = "NaN")
qdata$Index <- as.numeric(gsub(",","", qdata$Index))


colnames(qdata)[colnames(qdata) == "yyyyq"] <- "yearq"
colnames(qdata)[colnames(qdata) == "b/m"] <- "bm"

qdata$yearq <- as.yearqtr(format(qdata$yearq), "%Y%q")

#Compute/Transform required annual variables
#Dividends
#1. Dividend Price Ratio (difference between log of dividends and log of prices)
qdata$dp <- log(qdata$D12) - log(qdata$Index) 

#2. Dividend Yield (difference between log of dividends and log of lagged prices)
qdata$dy[1] <- NA
qdata$dy[2:nrow(qdata)] <- log(qdata$D12[2:nrow(qdata)]) - log(qdata$Index[1:(nrow(qdata) - 1)])

#Earnings 
#1. Earnings Price Ratio (difference between log of earnings and log of prices)
qdata$ep <- log(qdata$E12) - log(qdata$Index)

#2. Dividend Payout Ratio (difference between log of dividends and log of earnings)
qdata$de <- log(qdata$D12) - log(qdata$E12)

#Long Term Yield
#1. Term Spread (difference between long-term yield of gov't bonds and T-Bill)
qdata$tms <- qdata$lty - qdata$tbl

#Corporate Bond Returns 
#1. Default Yield Spread (difference between BAA and AAA-rated bonds)
qdata$dfy <- qdata$BAA - qdata$AAA

#2. Default Return Spread (difference between long-term corporate bond and long-term gov't bond returns)
qdata$dfr <- qdata$corpr - qdata$ltr

#Equity Premium (dependent variable) 
qdata$logRfree <- log(qdata$Rfree + 1) #log transformation of risk-free rate

qdata$Index_Div <- qdata$Index + qdata$D12 #annual returns + dividend 

#Log transformation of index returns + dividend
qdata$logReturnsDiv[1] <- NA
qdata$logReturnsDiv[2:nrow(qdata)] <- log(qdata$Index_Div[2:nrow(qdata)] / qdata$Index[1:nrow(qdata) - 1])

#Equity premium
qdata$eqprem <- qdata$logReturnsDiv - qdata$logRfree

#Remove all unused variables from qdata
qdata_rem <- qdata[, c("Index", "D12", "E12", "AAA", "BAA", "cay", "Rfree", "corpr", "csp", 
                   "CRSP_SPvw", "CRSP_SPvwx", "D3", "E3", "logRfree", "Index_Div",
                   "logReturnsDiv") := NULL]

#Convert to time series format - corresponding to time frame where all variables are available
ts_data <- ts(qdata, start = 1947, end = 2021, frequency = 4) 
plot(ts_data[,"eqprem"], main = "Log Equity Premium")
ts_data_df <- data.frame(ts_data)

#Subset qdata into 1947 - 2021 time frame
qdata <- subset(qdata, yearq >= 1947)
#################################################################################
#Correlation plot for data exploration
corrplot.mixed(cor(qdata_rem[,2:ncol(qdata)], use="pairwise.complete.obs"))

#Training Set 
startIS <- 1
endIS <- which(ts_data_df$yearq == 1976.00)
train <- subset(ts_data, start = startIS, end = endIS)
est_periods <- nrow(ts_data) - endIS

train_eqprem <- train[,"eqprem"]
all_eqprem <- ts_data[,"eqprem"]

#Test Set
startOS <- which(ts_data_df$yearq == 1976.00) + 1
endOS <- nrow(ts_data)
test <- subset(ts_data, start = startOS, end = endOS)

test_eqprem <- test[,"eqprem"]

#Historical mean model
fc_hist_mean <- meanf(train_eqprem, h = est_periods) 
#create forecast
hist_mean_errors <- all_eqprem - fc_hist_mean$mean #calculate error from "test" data
RMSE_hist <- sqrt(mean(hist_mean_errors^2)) #Compute RMSE, can use as error metric

#Preliminary kitchen sink model
ks_data <- qdata[,-1] # Remove year column for regression
ks_model <- lm(eqprem ~ ., data = ks_data)
summary(ks_model)
ks_pred_eqprem <- predict(ks_model, h = est_periods,  new_data = ks_data) #predict OS period
ks_errors <- all_eqprem - ks_pred_eqprem[1:297] #compute errors
RMSE_ks <- sqrt(mean(ks_errors^2)) #Compute root mean squared error, can use as error metric

###################################################################################
r2 <- function(actual,predict){
  cor(actual,predict)^2}

#Forecast Combination
train_pred <- train[,c("bm", "tbl", "lty", "ntis", "ltr", "svar", "ik", "dp", "dy", "ep", "de", "tms", "dfy", "dfr")]
train_obs <- train_eqprem
test_obs <- test_eqprem
test_pred <- test[,c("bm", "tbl", "lty", "ntis", "ltr", "svar", "ik", "dp", "dy", "ep", "de", "tms", "dfy", "dfr")]

combo_forecast <- foreccomb(train_obs, train_pred, test_obs, test_pred, criterion = "RMSE")

#Auto-combine 
best_auto_combination <- auto_combine(combo_auto_forecast, criterion = "RMSE")
plot(best_auto_combination)
OS_R2_BAC <- r2(combo_forecast$Actual_Test, best_auto_combination$Forecasts_Test)

#Rolling-combine
rolling_combination <- rolling_combine(combo_forecast, comb_method = "comb_OLS")
plot(rolling_combination)
OS_R2_RC <- r2(combo_forecast$Actual_Test, rolling_combination$Forecasts_Test)
