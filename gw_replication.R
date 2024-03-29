#Daniel Szabo

rm(list = ls())

library(data.table)
library(ggplot2)
library(forecast)
library(dyn)
library(reshape2)
library(readxl)
library(rms)

#Import original Goyal-Welch data (annual)

an_data <- read_xls("PredictorData.xls", na = "NaN", sheet = 3)
colnames(an_data)[colnames(an_data) == "yyyy"] <- "year"
colnames(an_data)[colnames(an_data) == "b/m"] <- "bm"

#Compute/Transform required annual variables
#Dividends
#1. Dividend Price Ratio (difference between log of dividends and log of prices)
an_data$dp <- log(an_data$D12) - log(an_data$Index) 

#2. Dividend Yield (difference between log of dividends and log of lagged prices)
an_data$dy[1] <- NA
an_data$dy[2:nrow(an_data)] <- log(an_data$D12[2:nrow(an_data)]) - log(an_data$Index[1:(nrow(an_data) - 1)])

#Earnings 
#1. Earnings Price Ratio (difference between log of earnings and log of prices)
an_data$ep <- log(an_data$E12) - log(an_data$Index)

#2. Dividend Payout Ratio (difference between log of dividends and log of earnings)
an_data$de <- log(an_data$D12) - log(an_data$E12)

#Long Term Yield
#1. Term Spread (difference between long-term yield of gov't bonds and T-Bill)
an_data$tms <- an_data$lty - an_data$tbl

#Corporate Bond Returns 
#1. Default Yield Spread (difference between BAA and AAA-rated bonds)
an_data$dfy <- an_data$BAA - an_data$AAA

#2. Default Return Spread (difference between long-term corporate bond and long-term gov't bond returns)
an_data$dfr <- an_data$corpr - an_data$ltr

#Equity Premium (dependent variable) 
an_data$logRfree <- log(an_data$Rfree + 1) #log transformation of risk-free rate

an_data$Index_Div <- an_data$Index + an_data$D12 #annual returns + dividend 

#Log transformation of index returns + dividend
an_data$logReturnsDiv[1] <- NA
an_data$logReturnsDiv[2:nrow(an_data)] <- log(an_data$Index_Div[2:nrow(an_data)] / an_data$Index[1:nrow(an_data) - 1])

#Equity premium
an_data$eqprem <- an_data$logReturnsDiv - an_data$logRfree

#Kitchen Sink Regression (all vars) #testing for now, doesn't provide matching results here
ks_all <- summary(lm(eqprem ~ dp + dy + ep + de + svar + bm + ntis + tbl + lty 
                    + dfr + infl + csp + CRSP_SPvw + CRSP_SPvwx, data = an_data))
ks_all 

#Convert to time series format
ts_data <- ts(an_data, start = 1871, end = 2005, frequency = 1) 
plot(ts_data[,"eqprem"], main = "Equity Premium")

################################################################################
#Get R2 (IS and OS) and change in RMSE 
#for reference (using formulas provided by GW:
#OS r2 = 1 - MSE(A) / MSE(N) || where A: Historical mean model and N: OLS model
#dRMSE = sqrt(MSE(N)) - sqrt(MSE(A))
#Data starts from 1872, goes until 2005
#Running this with a single variable for now, may turn this into a function later

start = which(an_data$year == 1872)
end = which(an_data$year == 2005)
OS_periods = 20

#Historical mean model (A) 
#hist_mean_model <- meanf(ts_data[2:nrow(ts_data),"eqprem"])
#IS_errors_hist <- hist_mean_model$residuals
#summary(hist_mean_model)

hist_mean_model <- meanf(an_data$eqprem[2:nrow(an_data)])
IS_errors_hist <- hist_mean_model$residuals

#OLS model (N)
OLS_model <- lm(eqprem ~ dp, data = ts_data)
OLS_model_sum <- summary(OLS_model)
IS_errors_OLS <- OLS_model$residuals

#control <- dyn$lm(eval(parse(text="eqprem")) ~ lag(eval(parse(text="dp")), -1), data=window(ts_data, start = 1872, end = 2005))
#summary(control)

#OS Errors (using for loop for now, may change later)

#Historical Mean Model
#window(ts_data, 1900, 1900)[, "eqprem"]
#an_data$eqprem[30]

#mean(window(ts_data, 1872, 1900)[, "eqprem"], na.rm=TRUE)
#mean(an_data$eqprem[start:30], na.rm = T)

OS_errors_hist <- 0
OS_errors_OLS <- 0

ts_data_df <- data.frame(ts_data)
for(i in seq(start, end)) {
  
  #year <- ts_data[i, "year"]
  #Historical model
  OS_errors_hist[i] <- an_data$eqprem[i] - mean(an_data$eqprem[start:i])
  
  #OLS model - using dp as test variable for now (still a WIP)
  OS_OLS <- lm(eqprem ~ lag(dp), data = ts_data_df)
  OLS_df <- data.frame(ts_data_df$dp[i:i])
  colnames(OLS_df) <- "dp"
  pred_OLS <- predict(OS_OLS, newdata=OLS_df)
  OS_errors_OLS[i] <- pred_OLS - an_data$eqprem[i]
}

#Calculate Error metrics 
MSE_A <- mean(OS_errors_hist^2) 
MSE_N <- mean(OS_errors_OLS^2, na.rm = T) 

#Stats (temp values for now)
IS_R2 <- round(OLS_model_sum$r.squared * 100, 2)
OS_R2 <- round((1 - MSE_A / MSE_N) * 100, 2)
dRMSE <- round(sqrt(MSE_N) / sqrt(MSE_A), 2)

#Put into table (similar to table 1 in GW) - R2 values in % form
Variable <- "dp"
stats_table <- cbind(Variable, IS_R2, OS_R2, dRMSE)
stats_table

################################################################################
#Testing different methods
eqprem <- ts_data[,"eqprem"]
start = which(an_data$year == 1872) # 2
end = which(an_data$year == 2005) # 131
OS_periods = 20

#Create training set
train <- subset(eqprem, start = start, end = end - OS_periods)
 
#Historical mean model & MSE
fc_hist_mean <- meanf(train, h = 20)
hist_mean_errors <- eqprem - fc_hist_mean$mean
MSE_hist <- mean(hist_mean_errors^2)
plot(hist_mean_errors)

#Linear univariate model & MSE
fc_lm <- lm(eqprem ~ dp, data = ts_data_df)
lm_df <- data.frame(ts_data_df$dp)
colnames(lm_df) <- "dp"
pred_fc_lm <- predict(fc_lm, newdata=lm_df)
fc_lm_errors <- pred_fc_lm - eqprem
MSE_lm <- mean(fc_lm_errors^2, na.rm = T)
plot(fc_lm_errors)

### Stats (univariate)
Variable <- "dp"
test_IS_R2 <- round(OLS_model_sum$r.squared * 100, 2)
test_OS_R2 <- round((1 - MSE_hist / MSE_lm) * 100, 2)
test_dRMSE <- round(sqrt(MSE_lm) / sqrt(MSE_hist), 2)

### Kitchen Sink Model testing
train_ks <- ts_data_df[2:115, c("eqprem", "dfy", "infl", "svar", "de", "lty", "tms", "tbl", "dfr",
                                "dp", "dy", "ltr", "ep", "bm", "ik", "ntis", "eqis", "CRSP_SPvw", "CRSP_SPvwx",
                                "csp")]
train_ks <- na.omit(train_ks)
ks_model <- lm(eqprem ~ ., data = train_ks)
fc_ks <- predict(ks_model, h = 20, newdata = ts_data_df)
fc_ks_df <- as.data.frame(fc_ks)
ks_errors <- eqprem[97:135] - fc_ks_df$`Point Forecast` #will need to fix this
MSE_ks <- mean(ks_errors^2, na.rm = T)

test_ks_OS_R2 <- round((1 - MSE_A / MSE_ks) * 100, 2)

################################################################################
#Key:

#eqprem: Equity premium (dependent variable)
#dfy: Default yield spread
#infl: Inflation
#svar: Stock variance
#de: Dividend payout ratio
#lty: Long term yield
#tms: Term spread
#tbl: Treasury-bill rate
#dfr: Default return spread
#dp: Dividend price ratio
#dy: Dividend yield
#ltr: Long term return
#ep: Earning price ratio
#bm: Book to market ratio
#ik: Investment capital ratio
#ntis: Net equity issuing
#eqis: Percent equity issuing
#ks: Kitchen sink