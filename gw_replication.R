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
                    + dfr + infl, data = an_data))
ks_all 

#Convert to time series format
ts_data <- ts(an_data, start = 1871, end = 2005, frequency = 1) 
plot(ts_data[,"eqprem"], main = "Equity Premium")

################################################################################
#Get R2 (IS and OS) and change in RMSE 
#for reference (using formulas provided by GW:
#OS r2 = 1 - MSE(A) / MSE(N) || where A: Historical mean model and N: OLS model
#dRMSE = sqrt(MSE(N)) - sqrt(MSE(A))
#Data starts from 1872, goes until 2005, with 20 years being used for OS analysis
start = which(an_data$year == 1872)
end = which(an_data$year == 2005)
OS_periods = 20

#Historical mean model (A) ~ two versions, I'll pick one
hist_mean_model <- meanf(ts_data[2:nrow(ts_data),"eqprem"])
IS_errors_hist <- hist_mean_model$residuals
summary(hist_mean_model)

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
#for(i in seq(start + OS_periods, end - 1))
#Test to see if it iterates correctly
for(i in seq(2,135)) {
  test <- an_data$eqprem[i]
  print(test)
  
}
print(test)
#OLS Model

#Calculate Error metrics (temp values for now)
MSE_A <- mean(OS_errors_hist^2)
MSE_N <- mean(OS_errors_OLS^2)

#Stats (temp values for now)
IS_R2 <- OLS_model_sum$r.squared
OS_R2 <- 1 - MSE_A / MSE_N
dRMSE <- sqrt(MSE_N) / sqrt(MSE_A)

#Put into table (similar to table 1 in GW) - will need to include var list
stats_table <- cbind(IS_R2, OS_R2, dRMSE)

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
#all: Kitchen sink