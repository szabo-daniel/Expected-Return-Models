#Daniel Szabo

library(caret)
library(data.table)
library(dplyr)
library(ggplot2)
library(forecast)
library(dyn)
library(reshape2)
library(readxl)
library(zoo)
library(corrplot)
library(ForecastComb)
library(dynlm)
library(xts)
library(stargazer)

#Import data (quarterly GW data updated thru 2021)
qdata <- fread("PredictorData2021 - Quarterly.csv", na.strings = "NaN")
qdata$Index <- as.numeric(gsub(",","", qdata$Index))

#Convert variable names to more consistent/readable form
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

#Transform qdata into a time series object for future use
ts_data <- ts(qdata)
ts_data_dt <- data.table(ts_data)
   
##################################################################################
R2 <- function(hist_errors, model_errors){
  1 - mean(hist_errors^2)/mean(model_errors^2)
}

dRMSE <- function(hist_errors, model_errors){
  sqrt(mean(model_errors^2)) - sqrt(mean(hist_errors^2))
}

#Subset data to be from 1947 onward (to have values for all predictor variables)
reg_data <- subset(qdata, yearq >= 1947)

#Transform data to only include predictor variables, period, and dependent variable eqprem (log equity premium)
reg_data <- reg_data[,c("yearq", "eqprem", "bm", "tbl", "lty", "ntis", "infl", "ltr", "svar", "ik", "dp", "dy",
                        "ep", "de", "tms", "dfy", "dfr")]

#Hold back predictor variable data by 1 quarter to prevent look-ahead bias
reg_data <- reg_data %>% mutate(bm = lag(bm),
                                tbl = lag(tbl),
                                lty = lag(lty),
                                ntis = lag(ntis),
                                infl = lag(infl),
                                ltr = lag(ltr),
                                svar = lag(svar),
                                ik = lag(ik),
                                dp = lag(dp),
                                dy = lag(dy),
                                ep = lag(ep),
                                de = lag(de),
                                tms = lag(tms),
                                dfy = lag(dfy),
                                dfr = lag(dfr))

reg_data <- reg_data[-1] #Remove last row to eliminate NAs

#Create training and test sets
#Constant parameters across all sets
endIS <- which(reg_data$yearq == "2018 Q4") #end in-sample period 
startOS <- endIS + 1 #start out-of-sample period 1 ahead
endOS <- nrow(reg_data) #end out-of-sample at the end of the data
est_periods <- endOS - endIS #periods to be estimated (length of OS period)

#Training set 1 (1947 - 2018)
startIS_1 <- which(reg_data$yearq == "1947 Q2")
train_1 <- reg_data[startIS_1:endIS]

#Training set 2 (1990 - 2018)
startIS_2 <- which(reg_data$yearq == "1990 Q1")
train_2 <- reg_data[startIS_2:endIS]

#Training set 3 (2000 - 2018)
startIS_3 <- which(reg_data$yearq == "2000 Q1")
train_3 <- reg_data[startIS_3:endIS]

#Test Set
test <- reg_data[startOS:endOS]

#Prevailing Mean Model (benchmark)
#This model predicts the equity premium by taking the mean of previous equity premiums

#1. Initialize data to be used, to be used only for this model
all_reg_1 <- reg_data #1947-2021
all_reg_2 <- reg_data[which(reg_data$yearq == "1990 Q1"):nrow(reg_data)] #1990-2021
all_reg_3 <- reg_data[which(reg_data$yearq == "2000 Q1"):nrow(reg_data)] #2000-2021

#Initialize vectors for predicted equity premium values to go into
pm_pred_1 <- double(nrow(all_reg_1)) 
pm_pred_2 <- double(nrow(all_reg_2))
pm_pred_3 <- double(nrow(all_reg_3))

for (i in seq(1, nrow(all_reg_1))) {
  pm_pred_1[i] <- mean(all_reg_1$eqprem[1:i-1]) #Insert equity premium prediction based on mean of previous equity premiums
}
pm_pred_test_1 <- tail(pm_pred_1, nrow(test)) #Last 12 variables represent our "test" data to compare with other models
pm_1_R2 <- r2(test$eqprem, pm_pred_test_1) #Calculate R2 metric for model
pm_1_errors <- test$eqprem - pm_pred_test_1

for (i in seq(1, nrow(all_reg_2))){
  pm_pred_2[i] <- mean(all_reg_2$eqprem[1:i-1])
}
pm_pred_test_2 <- tail(pm_pred_2, nrow(test))
pm_2_R2 <- r2(test$eqprem, pm_pred_test_2)
pm_2_errors <- test$eqprem - pm_pred_test_2

for (i in seq(1, nrow(all_reg_3))){
  pm_pred_3[i] <- mean(all_reg_3$eqprem[1:i-1])
}
pm_pred_test_3 <- tail(pm_pred_3, nrow(test))
pm_3_R2 <- r2(test$eqprem, pm_pred_test_3)
pm_3_errors <- test$eqprem - pm_pred_test_3

#Regression 1: 1947 - 2021 
reg47_data <- train_1[,-c("yearq")] #Omit yearq from regression
ks_reg47 <- lm(eqprem ~ ., data = reg47_data) #Use all predictor variables in regression
summary(ks_reg47)

ks_reg47_pred_eqprem <- predict(ks_reg47, h = est_periods, newdata = test[,-c("yearq")]) #Forecast equity premium values across OS period
ks_reg47_errors <- test$eqprem - ks_reg47_pred_eqprem #Calculate difference between actual and predicted equity premium values
RMSE_ks_reg47 <- sqrt(mean(ks_reg47_errors^2)) #calculate RMSE using error values
dRMSE_ks_reg47 <- dRMSE(pm_1_errors, ks_reg47_errors)

#ks_reg47_R2 <- r2(test$eqprem, ks_reg47_pred_eqprem) #Calculate OS R2 value for regression
ks_reg47_R2 <- R2(pm_1_errors, ks_reg47_errors)

#Regression 2: 1990 - 2021
reg90_data <- train_2[,-c("yearq")]
ks_reg90 <- lm(eqprem ~ ., data = reg90_data)
summary(ks_reg90)

ks_reg90_pred_eqprem <- predict(ks_reg90, h = est_periods, newdata = test[,-c("yearq")])
ks_reg90_errors <- test$eqprem - ks_reg90_pred_eqprem
RMSE_ks_reg90 <- sqrt(mean(ks_reg90_errors^2))
dRMSE_ks_reg90 <- dRMSE(pm_2_errors, ks_reg90_errors)

#ks_reg90_R2 <- r2(test$eqprem, ks_reg90_pred_eqprem)
ks_reg90_R2 <- R2(pm_2_errors, ks_reg90_errors)

#Regression 3: 2000 - 2021
reg00_data <- train_3[,-c("yearq")]
ks_reg00 <- lm(eqprem ~ ., data = reg00_data)
summary(ks_reg00)

ks_reg00_pred_eqprem <- predict(ks_reg00, h = est_periods, newdata = test[,-c("yearq")])
ks_reg00_errors <- test$eqprem - ks_reg00_pred_eqprem
RMSE_ks_reg00 <- sqrt(mean(ks_reg00_errors^2))
dRMSE_ks_reg00 <- dRMSE(pm_3_errors, ks_reg00_errors)

ks_reg00_R2 <- R2(pm_3_errors, ks_reg00_errors)
##################################################################################
#Combination Forecasts
#Combination Forecast 1: 1947 - 2018 training
train_1_ts <- ts(train_1, start = c(1947, 2), end = c(2018, 4), frequency = 4) #Subset training data to be from 1947:2-2018:4
test_ts <- ts(test, start = c(2019, 1), end = c(2021, 4), frequency = 4) #Create test test to be from 2019:1-2021:4

#Create variables to use in "ForecastComb" package
#Predictor variable data in training data
train_pred_1 <- train_1_ts[,c("bm", "tbl", "lty", "ntis", "ltr", "infl", "svar", "ik", "dp", "dy", "ep", "de", "tms", "dfy", "dfr")]
#Dependent variable data in training data
train_obs_1 <- train_1_ts[,"eqprem"]
#Predictor variable data in test data
test_pred_1 <- test_ts[,c("bm", "tbl", "lty", "ntis", "infl", "ltr", "svar", "ik", "dp", "dy", "ep", "de", "tms", "dfy", "dfr")]
#Dependent variable data in test data
test_obs_1 <- test_ts[,"eqprem"]

#Combine above variables into "forecomb" object - feeds into different combination forecasts
combo_forecast_1 <- foreccomb(train_obs_1, train_pred_1, test_obs_1, test_pred_1, criterion = "RMSE")

#Create best auto combination model (best fit method) that minimizes RMSE
best_auto_combination_1 <- auto_combine(combo_forecast_1, criterion = "RMSE")

#Compute errors
BAC_1_errors <- test$eqprem - best_auto_combination_1$Forecasts_Test

#Compute out-of-sample R2
#BAC_R2_1 <- r2(test$eqprem, best_auto_combination_1$Forecasts_Test)
BAC_1_R2 <- R2(pm_1_errors, BAC_1_errors)

#Compute RMSE
RMSE_BAC_1 <- best_auto_combination_1$Accuracy_Test[2]

#Compute change in RMSE relative to mean model
dRMSE_BAC_1 <- dRMSE(pm_1_errors, BAC_1_errors)

#Combination Forecast 2: 1990 - 2018 training
train_2_ts <- ts(train_2, start = c(1990, 1), end = c(2018, 4), frequency = 4)
test_ts <- ts(test, start = c(2019, 1), end = c(2021, 4), frequency = 4)

train_pred_2 <- train_2_ts[,c("bm", "tbl", "lty", "ntis", "ltr", "infl", "svar", "ik", "dp", "dy", "ep", "de", "tms", "dfy", "dfr")]
train_obs_2 <- train_2_ts[,"eqprem"]
test_pred_2 <- test_ts[,c("bm", "tbl", "lty", "ntis", "infl", "ltr", "svar", "ik", "dp", "dy", "ep", "de", "tms", "dfy", "dfr")]
test_obs_2 <- test_ts[,"eqprem"]

combo_forecast_2 <- foreccomb(train_obs_2, train_pred_2, test_obs_2, test_pred_2, criterion = "RMSE")

best_auto_combination_2 <- auto_combine(combo_forecast_2, criterion = "RMSE")
#BAC_R2_2 <- r2(test$eqprem, best_auto_combination_2$Forecasts_Test)
BAC_2_errors <- test$eqprem - best_auto_combination_2$Forecasts_Test

BAC_2_R2 <- R2(pm_2_errors, BAC_2_errors)

RMSE_BAC_2 <- best_auto_combination_2$Accuracy_Test[2]

dRMSE_BAC_2 <- dRMSE(pm_2_errors, BAC_2_errors)

#Combination Forecast 3: 2000 - 2018 training
train_3_ts <- ts(train_3, start = c(2000, 1), end = c(2018, 4), frequency = 4)
test_ts <- ts(test, start = c(2019, 1), end = c(2021, 4), frequency = 4)

train_pred_3 <- train_3_ts[,c("bm", "tbl", "lty", "ntis", "ltr", "infl", "svar", "ik", "dp", "dy", "ep", "de", "tms", "dfy", "dfr")]
train_obs_3 <- train_3_ts[,"eqprem"]
test_pred_3 <- test_ts[,c("bm", "tbl", "lty", "ntis", "infl", "ltr", "svar", "ik", "dp", "dy", "ep", "de", "tms", "dfy", "dfr")]
test_obs_3 <- test_ts[,"eqprem"]

combo_forecast_3 <- foreccomb(train_obs_3, train_pred_3, test_obs_3, test_pred_3, criterion = "RMSE")

best_auto_combination_3 <- auto_combine(combo_forecast_3, criterion = "RMSE")

BAC_3_errors <- test$eqprem - best_auto_combination_3$Forecasts_Test
#BAC_R2_3 <- r2(test$eqprem, best_auto_combination_3$Forecasts_Test)
BAC_3_R2 <- R2(pm_3_errors, BAC_3_errors)

RMSE_BAC_3 <- best_auto_combination_3$Accuracy_Test[2]

dRMSE_BAC_3 <- dRMSE(pm_3_errors, BAC_3_errors)

#Bates-Granger Forecasts
BG_combo_1 <- comb_BG(combo_forecast_1)
BG_combo_2 <- comb_BG(combo_forecast_2)
BG_combo_3 <- comb_BG(combo_forecast_3)

BG_1_errors <- test$eqprem - BG_combo_1$Forecasts_Test
BG_2_errors <- test$eqprem - BG_combo_2$Forecasts_Test
BG_3_errors <- test$eqprem - BG_combo_3$Forecasts_Test

BG_1_R2 <- R2(pm_1_errors, BG_1_errors)
BG_2_R2 <- R2(pm_2_errors, BG_2_errors)
BG_3_R2 <- R2(pm_3_errors, BG_3_errors)

BG_1_RMSE <- BG_combo_1$Accuracy_Test[2]
BG_2_RMSE <- BG_combo_2$Accuracy_Test[2]
BG_3_RMSE <- BG_combo_3$Accuracy_Test[2]

dRMSE_BG_1 <- dRMSE(pm_1_errors, BG_1_errors)
dRMSE_BG_2 <- dRMSE(pm_2_errors, BG_2_errors)
dRMSE_BG_3 <- dRMSE(pm_3_errors, BG_3_errors)
##################################################################################
#Paper trading
#Create portfolio data, consisting of the index returns and risk-free return
assets <- subset(qdata, qdata$yearq >= 2019)
assets <- assets[,c("CRSP_SPvw", "Rfree")]

#Model returns
#If equity premium is positive, invest in market index. If negative, invest in risk-free asset
pm_1_returns <- ifelse(pm_pred_test_1 > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_pm_1 <- mean(pm_1_returns) #calculate mena of returns over time period

pm_2_returns <- ifelse(pm_pred_test_2 > 0, assets$CRSP_SPv, assets$Rfree)
mean_returns_pm_2 <- mean(pm_2_returns)

pm_3_returns <- ifelse(pm_pred_test_3 > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_pm_3 <- mean(pm_3_returns)

ks_1_model_returns <- ifelse(ks_reg47_pred_eqprem > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_ks_1 <- mean(ks_1_model_returns)

ks_2_model_returns <- ifelse(ks_reg90_pred_eqprem > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_ks_2 <- mean(ks_2_model_returns)

ks_3_model_returns <- ifelse(ks_reg00_pred_eqprem > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_ks_3 <-mean(ks_3_model_returns)

BAC_returns_1 <- ifelse(best_auto_combination_1$Forecasts_Test > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_BAC_1 <- mean(BAC_returns_1)

BAC_returns_2 <- ifelse(best_auto_combination_2$Forecasts_Test > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_BAC_2 <- mean(BAC_returns_2)

BAC_returns_3 <- ifelse(best_auto_combination_3$Forecasts_Test > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_BAC_3 <- mean(BAC_returns_3)

BG_returns_1 <- ifelse(BG_combo_1$Forecasts_Test > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_BG_1 <- mean(BG_returns_1)

BG_returns_2 <- ifelse(BG_combo_2$Forecasts_Test > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_BG_2 <- mean(BG_returns_2)

BG_returns_3 <- ifelse(BG_combo_2$Forecasts_Test > 0, assets$CRSP_SPvw, assets$Rfree)
mean_returns_BG_3 <- mean(BG_returns_3)

#Sharpe Ratios
pm_1_sharpe <- mean_returns_pm_1 / sd(pm_1_returns) #mean of returns divided by their standard deviation
pm_2_sharpe <- mean_returns_pm_2 / sd(pm_2_returns)
pm_3_sharpe <- mean_returns_pm_3 / sd(pm_3_returns)

ks_1_sharpe <- mean_returns_ks_1 / sd(ks_1_model_returns)
ks_2_sharpe <- mean_returns_ks_2 / sd(ks_2_model_returns)
ks_3_sharpe <- mean_returns_ks_3 / sd(ks_3_model_returns)

BAC_1_sharpe <- mean_returns_BAC_1 / sd(BAC_returns_1)
BAC_2_sharpe <- mean_returns_BAC_2 / sd(BAC_returns_2)
BAC_3_sharpe <- mean_returns_BAC_3 / sd(BAC_returns_3)

BG_1_sharpe <- mean_returns_BG_1 / sd(BG_returns_1)
BG_2_sharpe <- mean_returns_BG_2 / sd(BG_returns_2)
BG_3_sharpe <- mean_returns_BG_3 / sd(BG_returns_3)

#Output results (exploratory)
rbind(ks_1_sharpe, ks_2_sharpe, ks_3_sharpe, 
      BAC_1_sharpe, BAC_2_sharpe, BAC_3_sharpe,
      BG_1_sharpe, BG_2_sharpe, BG_3_sharpe)

rbind(ks_reg47_R2, ks_reg90_R2, ks_reg00_R2, 
      BAC_1_R2, BAC_2_R2, BAC_3_R2,
      BG_1_R2, BG_2_R2, BG_3_R2)

rbind(RMSE_ks_reg47, RMSE_ks_reg90, RMSE_ks_reg00,
      RMSE_BAC_1, RMSE_BAC_2, RMSE_BAC_3,
      BG_1_RMSE, BG_2_RMSE, BG_3_RMSE)

rbind(dRMSE_ks_reg47, dRMSE_ks_reg90, dRMSE_ks_reg00,
      dRMSE_BAC_1, dRMSE_BAC_2, dRMSE_BAC_3,
      dRMSE_BG_1, dRMSE_BG_2, dRMSE_BG_3)
################################################################################
#Output equity premium and market return data in a CSV file
market_return <- tail(qdata$CRSP_SPvw, nrow(test))
rfree <- tail(qdata$Rfree, nrow(test))

return_data <- cbind(market_return, rfree)
colnames(return_data) <- c("Market Return", "Risk-free Rate")
return_data <- data.frame(return_data)

eqprem_predictions <- cbind(test$yearq,
                            test$eqprem,
                            return_data$Market.Return,
                            return_data$Risk.free.Rate,
                            ks_reg47_pred_eqprem,
                            ks_reg90_pred_eqprem,
                            ks_reg00_pred_eqprem,
                            best_auto_combination_1$Forecasts_Test,
                            best_auto_combination_2$Forecasts_Test,
                            best_auto_combination_3$Forecasts_Test,
                            BG_combo_1$Forecasts_Test,
                            BG_combo_2$Forecasts_Test,
                            BG_combo_3$Forecasts_Test)
eqprem_predictions <- data.frame(eqprem_predictions)

write.csv(eqprem_predictions, "paper_trading.csv", row.names = F)
################################################################################
#Stargazer Outputs
#Create variables to store result outputs
models <- c("Kitchen Sink", "Best Fit Combination", "Bates/Granger Combination")

model1_OS_R2 <- c(ks_reg47_R2, BAC_1_R2, BG_1_R2)
model1_RMSE <- c(RMSE_ks_reg47, RMSE_BAC_1, BG_1_RMSE)
model1_dRMSE <- c(dRMSE_ks_reg47, dRMSE_BAC_1, dRMSE_BG_1)
model1_sharpe <- c(ks_1_sharpe, BAC_1_sharpe, BG_1_sharpe)

model2_OS_R2 <- c(ks_reg90_R2, BAC_2_R2, BG_2_R2)
model2_RMSE <- c(RMSE_ks_reg90, RMSE_BAC_2, BG_2_RMSE)
model2_dRMSE <- c(dRMSE_ks_reg90, dRMSE_BAC_2, dRMSE_BG_2)
model2_sharpe <- c(ks_2_sharpe, BAC_2_sharpe, BG_2_sharpe)

model3_OS_R2 <- c(ks_reg00_R2, BAC_3_R2, BG_3_R2)
model3_RMSE <- c(RMSE_ks_reg00, RMSE_BAC_3, BG_3_RMSE)
model3_dRMSE <- c(dRMSE_ks_reg00, dRMSE_BAC_3, dRMSE_BG_3)
model3_sharpe <- c(ks_3_sharpe, BAC_3_sharpe, BG_3_sharpe)

#Table 1: in return_forecasting_gw.R

#Table 2: First period: 1947 - 2018 training
table2 <- data.frame(Model = models, OS.R2 = model1_OS_R2, RMSE = model1_RMSE, dRMSE = model1_dRMSE, Sharpe = model1_sharpe)
stargazer(table2, summary = F, title = "Table 2: Model Results, Training Period 1947 - 2018",
          align = T, digits = 4, no.space = T, flip = F, type = "text", rownames = F, out = "table2.txt")

#Table 3: Second period: 1990 - 2018 training 
table3 <- data.frame(Model = models, OS.R2 = model2_OS_R2, RMSE = model2_RMSE, dRMSE = model2_dRMSE, Sharpe = model2_sharpe)
stargazer(table3, summary = F, title = "Table 3: Model Results, Training Period 1990 - 2018",
          align = T, digits = 4, no.space = T, flip = F, type = "text", rownames = F, out = "table3.txt")

#Table 4: Third period: 2000 - 2018 training
table4 <- data.frame(Model = models, OS.R2 = model3_OS_R2, RMSE = model3_RMSE, dRMSE = model3_dRMSE, Sharpe = model3_sharpe)
stargazer(table4, summary = F, title = "Table 4: Model Results, Training Period 2000 - 2018",
          align = T, digits = 4, no.space = T, flip = F, type = "text", rownames = F, out = "table4.txt")
