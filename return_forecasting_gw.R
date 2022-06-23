#Daniel Szabo 

rm(list = ls())

library(data.table)
library(ggplot2)
library(forecast)
library(vars)
library(dyn)
library(reshape2)
library(tseries)

#Import Data
#monthly <- read.csv("PredictorData2021 - Monthly.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
#monthly <- as.data.table(monthly)
#monthly$Index <- as.numeric(gsub(",","", monthly$Index))
#monthly$yyyymm <- as.Date(paste0(as.character(monthly$yyyymm), "01"), format = "%Y%m%d")

annual <- read.csv("PredictorData2021 - Annual.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
annual <- as.data.table(annual)
annual$Index <- as.numeric(gsub(",","",annual$Index))

#Original 2005 data
annual <- read_xls("PredictorData.xls", na = "NaN", sheet = 3)
annual <- as.data.table(annual)
################################################################################
#Calculate/Transform annual variables to conform with Goyal-Welch data (based on christophj.github.io)

{
#Continuously compounded index returns + 12 month moving sum of dividends
annual <- annual[, IndexDiv := Index + D12] 

#Dividend Price Ratio
annual <- annual[, dp := log(D12) - log(Index)]

#Dividend earnings ratio
annual <- annual[, de := log(D12) - log(E12)] 

#Default return spread 
annual <- annual[, dfr := corpr - ltr] 

#Default yield spread
annual <- annual[, dfy := BAA - AAA] 

#Earnings Price Ratio
annual <- annual[, ep := log(E12) - log(Index)] 

#Term spread 
annual <- annual[, tms := lty - tbl] 

#Dividend Yield
vec_dy <- c(NA, annual[2:nrow(annual), log(D12)] - annual[1:(nrow(annual)-1), log(Index)])
annual <- annual[, dy := vec_dy] 

annual <- annual[, logret := c(NA,diff(log(Index)))]
vec_logretdiv <- c(NA, annual[2:nrow(annual), log(IndexDiv)] - annual[1:(nrow(annual)-1), log(Index)])
vec_logretdiv <- c(NA, log(annual[2:nrow(annual), IndexDiv]/annual[1:(nrow(annual)-1), Index]))
annual <- annual[, logretdiv := vec_logretdiv] 

#Log risk-free rate
annual <- annual[, logRfree := log(Rfree + 1)] 

#Log Equity Premium
annual <- annual[, rp_div := logretdiv - logRfree]

#Time Series
#ts_annual <- ts(annual, start=annual[1, yyyy], end=annual[nrow(annual), yyyy])
ts_annual <- ts(annual, start = 1871, end = 2005, frequency = 1)
}

plot(ts_annual[, c("rp_div", "dp", "dy")])
colnames(ts_annual)[colnames(ts_annual) == "b/m"] <- "b.m"
################################################################################
#Statistics function (christophj.github.io)
#ts_df: time series data frame, indep: independent variable, dep: dependent variable (rp_div in this analysis),
#start: start date, end: end date, est_periods_OOS: OOS periods used

get_statistics <- function(ts_df, indep, dep, h=1, start=1872, end=2005, est_periods_OOS = 20) {
  
  #In-Sample Analysis
  
  #1. Historical mean model
  avg   <- mean(window(ts_df, start, end)[, dep], na.rm=TRUE)
  IS_error_N <- (window(ts_df, start, end)[, dep] - avg)
  
  #2. OLS model (Ordinary Least Squares regression)
  reg <- dyn$lm(eval(parse(text=dep)) ~ lag(eval(parse(text=indep)), -1), data=window(ts_df, start, end))
  IS_error_A <- reg$residuals
  
  #OOS Analysis 
  
  OOS_error_N <- numeric(end - start - est_periods_OOS) #Historical mean error
  OOS_error_A <- numeric(end - start - est_periods_OOS) #OLS error
  
  #Only use information that is available up to the time at which the forecast is made
  j <- 0
  for (i in (start + est_periods_OOS):(end-1)) {
    j <- j + 1
    #Get the actual ERP that you want to predict
    actual_ERP <- as.numeric(window(ts_df, i+1, i+1)[, dep])
    
    #1. Historical mean model
    OOS_error_N[j] <- actual_ERP - mean(window(ts_df, start, i)[, dep], na.rm=TRUE)
    
    #2. OLS model
    reg_OOS <- dyn$lm(eval(parse(text=dep)) ~ lag(eval(parse(text=indep)), -1), 
                      data=window(ts_df, start, i))
    #Compute_error
    df <- data.frame(x=as.numeric(window(ts_df, i, i)[, indep]))
    names(df) <- indep
    pred_ERP   <- predict.lm(reg_OOS, newdata=df)
    OOS_error_A[j] <-  pred_ERP - actual_ERP
    
  }
  ##
  #Compute statistics 
  MSE_N <- mean(OOS_error_N^2)
  MSE_A <- mean(OOS_error_A^2)
  T <- length(!is.na(ts_df[, dep]))
  OOS_R2  <- 1 - MSE_A/MSE_N
  #Is the -1 enough (maybe -2 needed because of lag)?
  OOS_oR2 <- OOS_R2 - (1-OOS_R2)*(reg$df.residual)/(T - 1) 
  dRMSE <- sqrt(MSE_N) - sqrt(MSE_A)
  ##
  
  #### CREATE PLOT
  IS  <- cumsum(IS_error_N[2:length(IS_error_N)]^2)-cumsum(IS_error_A^2)
  OOS <- cumsum(OOS_error_N^2)-cumsum(OOS_error_A^2)
  df  <- data.frame(x=seq.int(from=start + 1 + est_periods_OOS, to=end), 
                    IS=IS[(1 + est_periods_OOS):length(IS)], 
                    OOS=OOS) #Because you lose one observation due to the lag
  #Shift IS errors vertically, so that the IS line begins 
  # at zero on the date of first OOS prediction. (see Goyal/Welch (2008, p. 1465))
  
  #Oil Shock overlay
  df$IS <- df$IS - df$IS[1] 
  df  <- melt(df, id.var="x") 
  plotGG <- ggplot(df) + 
    geom_line(aes(x=x, y=value,color=variable)) + 
    geom_rect(data=data.frame(),#Needed by ggplot2, otherwise not transparent
              aes(xmin=1973, xmax=1975,ymin=-0.2,ymax=0.2), 
              fill='red',
              alpha=0.1) + 
    scale_y_continuous('Cumulative SSE Difference', limits=c(-0.2, 0.2)) + 
    scale_x_continuous('Year')
  ##
  
  return(list(IS_error_N = IS_error_N,
              IS_error_A = reg$residuals,
              OOS_error_N = OOS_error_N,
              OOS_error_A = OOS_error_A,
              IS_R2 = summary(reg)$r.squared, 
              IS_aR2 = summary(reg)$adj.r.squared, 
              OOS_R2  = OOS_R2,
              OOS_oR2 = OOS_oR2,
              dRMSE = dRMSE,
              plotGG = plotGG))
  
}
###############################################################################
#Get stats of each variable (IS, OOS errors for both models, IS, OOS R2, change in RMSE)

dp_stat <- get_statistics(ts_annual, "dp", "rp_div", start=1872)
dy_stat <- get_statistics(ts_annual, "dy", "rp_div", start=1872)
ep_stat <- get_statistics(ts_annual, "ep", "rp_div", start=1872)
de_stat <- get_statistics(ts_annual, "de", "rp_div", start=1872)
dfy_stat <- get_statistics(ts_annual, "dfy", "rp_div", start = 1919)
dfr_stat <- get_statistics(ts_annual, "dfr", "rp_div", start=1926)
eqis_stat <- get_statistics(ts_annual, "eqis", "rp_div", start=1927)
infl_stat <- get_statistics(ts_annual, "infl", "rp_div", start=1919)
ik_stat <- get_statistics(ts_annual, "ik", "rp_div", start=1947)
lty_stat <- get_statistics(ts_annual, "lty", "rp_div", start=1919)
ltr_stat <- get_statistics(ts_annual, "ltr", "rp_div", start=1926)
svar_stat <- get_statistics(ts_annual, "svar", "rp_div", start = 1885)
bm_stat <- get_statistics(ts_annual, "b.m", "rp_div", start = 1921)
tbl_stat <- get_statistics(ts_annual, "tbl", "rp_div", start=1920)
tms_stat <- get_statistics(ts_annual, "tms", "rp_div", start = 1920)
ntis_stat <- get_statistics(ts_annual, "ntis", "rp_div", start=1927)

#Plots - Cumulative SSE difference between IS/OOS of each variable
ntis_stat$plotGG + ggtitle("Net Equity Expansion")
dp_stat$plotGG + ggtitle("Dividend-Price Ratio (dp)")
dy_stat$plotGG + ggtitle("Dividend Yield (dy)")
ep_stat$plotGG + ggtitle("Earnings Price Ratio (ep)")
de_stat$plotGG + ggtitle("Dividend Payout Ratio (dp)")
dfy_stat$plotGG + ggtitle("Default Yield Spread (dfy)")
dfr_stat$plotGG + ggtitle("Default return spread (dfr)")
eqis_stat$plotGG + ggtitle("Percent Equity Issuing")
infl_stat$plotGG + ggtitle("Inflation")
ik_stat$plotGG + ggtitle("Investment-Capital Ratio")
lty_stat$plotGG + ggtitle("Long-Term Yield")
ltr_stat$plotGG + ggtitle("Long-Term Return")
svar_stat$plotGG + ggtitle("Stock variance") #still needs work
bm_stat$plotGG + ggtitle("Book to Market ratio")
tbl_stat$plotGG + ggtitle("Treasury Bill Rate")
tms_stat$plotGG + ggtitle("Term spread")

###############################################################################
#Stats Table (IS, OOS R2 and change in RMSE for each variable - similar to GW tables)
{
Variable <- c("dfy", "infl", "svar", "d/e", "lty", "tms", "tbl", "dfr", "d/p", 
              "d/y", "ltr", "e/p", "b/m", "i/k", "ntis", "eqis")

IS_R2 <- c(dfy_stat$IS_aR2, infl_stat$IS_aR2, svar_stat$IS_aR2,
               de_stat$IS_aR2, lty_stat$IS_aR2, tms_stat$IS_aR2,
               tbl_stat$IS_aR2, dfr_stat$IS_aR2, dp_stat$IS_aR2,
               dy_stat$IS_aR2, ltr_stat$IS_aR2, ep_stat$IS_aR2,
               bm_stat$IS_aR2, ik_stat$IS_aR2, ntis_stat$IS_aR2,
               eqis_stat$IS_aR2)
IS_R2 <- round(IS_R2*100, 2)

OS_R2 <- c(dfy_stat$OOS_oR2, infl_stat$OOS_oR2, svar_stat$OOS_oR2,
           de_stat$OOS_oR2, lty_stat$OOS_oR2, tms_stat$OOS_oR2,
           tbl_stat$OOS_oR2, dfr_stat$OOS_oR2, dp_stat$OOS_oR2,
           dy_stat$OOS_oR2, ltr_stat$OOS_oR2, ep_stat$OOS_oR2,
           bm_stat$OOS_oR2, ik_stat$OOS_oR2, ntis_stat$OOS_oR2,
           eqis_stat$OOS_oR2)
OS_R2 <- round(OS_R2*100, 2)

dRMSE <- c(dfy_stat$dRMSE, infl_stat$dRMSE, svar_stat$dRMSE,
           de_stat$dRMSE, lty_stat$dRMSE, tms_stat$dRMSE,
           tbl_stat$dRMSE, dfr_stat$dRMSE, dp_stat$dRMSE,
           dy_stat$dRMSE, ltr_stat$dRMSE, ep_stat$dRMSE,
           bm_stat$dRMSE, ik_stat$dRMSE, ntis_stat$dRMSE,
           eqis_stat$dRMSE)
dRMSE <- round(dRMSE*100, 2)

gw_table <- as.data.frame(cbind(Variable, IS_R2, OS_R2, dRMSE), row.names=F)
}
#Table including both IS and OS R2, as well as change in RMSE
print(gw_table)

#Kitchen Sink Model

###############################################################################
#Forecasting with different models

#Subset data into training and test - use later
#train <- subset(ts_annual, start = 2, end = 120)
#test <- subset(ts_annual, start = 121, end = 151)

#Models on annual data (basic for now just to familiarize myself with the syntax & graphs)
an_rp_div <- ts_annual[,"rp_div"]
an_rp_div <- na.omit(an_rp_div)
#Stationarity of TS test
adf.test(an_rp_div) #p-val is < 0.01, series is stationary

#Training set - years 1873 - 2010 (may adjust time-frame)
train_rp_div <- subset(an_rp_div, start = 2, end = 139)

#Naive Model
fc_naive <- naive(train_rp_div, h = 11)
summary(fc_naive)
checkresiduals(fc_naive) #p-val: 2.013e-06
autoplot(forecast(fc_naive))
accuracy(fc_naive, an_rp_div)

#Simple Exponential Smoothing
fc_ses <- ses(train_rp_div, h = 11)
summary(fc_ses)
checkresiduals(fc_ses) #p-val: 0.03578
autoplot(forecast(fc_ses))
accuracy(fc_ses, an_rp_div)

#Holt Model
fc_holt <- holt(train_rp_div, h = 11)
summary(fc_holt)
checkresiduals(fc_holt) #p-val: 0.0121
autoplot(forecast(fc_holt))
accuracy(fc_holt, an_rp_div)

#ARIMA Model
fc_arima <- auto.arima(train_rp_div)
summary(fc_arima)
checkresiduals(fc_arima) #p-val: 0.05718, ARIMA(0,0,0)
autoplot(forecast(fc_arima, h = 11))
accuracy(fc_arima, an_rp_div) #Doesn't work since auto.arima doesn't return a class type "forecast". Working on fix.

#TBATS Model
fc_tbats <- tbats(train_rp_div)
summary(fc_tbats)
checkresiduals(fc_tbats) #p-val: 0.03516
autoplot(forecast(fc_tbats, h = 11))
accuracy(fc_tbats, an_rp_div) #Doesn't work since tbats() returns a class type "bats". Working on fix

#


