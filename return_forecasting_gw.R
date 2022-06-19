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

################################################################################
#Calculate/Transform annual variables to conform with Goyal-Welch data (from christophj.github.io)

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
annual <- annual[, rp_div   := logretdiv - logRfree]

#Time Series
ts_annual <- ts(annual, start=annual[1, yyyy], end=annual[nrow(annual), yyyy])
}

plot(ts_annual[, c("rp_div", "dp", "dy")])

################################################################################
#Statistics function (christophj.github.io)

get_statistics <- function(ts_df, indep, dep, h=1, start=1872, end=2021, est_periods_OOS = 20) {
  
  #### IS ANALYSIS
  
  #1. Historical mean model
  avg   <- mean(window(ts_df, start, end)[, dep], na.rm=TRUE)
  IS_error_N <- (window(ts_df, start, end)[, dep] - avg)
  
  #2. OLS model
  reg <- dyn$lm(eval(parse(text=dep)) ~ lag(eval(parse(text=indep)), -1), data=window(ts_df, start, end))
  IS_error_A <- reg$residuals
  ### 
  
  ####OOS ANALYSIS
  OOS_error_N <- numeric(end - start - est_periods_OOS)
  OOS_error_A <- numeric(end - start - est_periods_OOS)
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
#Plots 

dp_stat <- get_statistics(ts_annual, "dp", "rp_div", start=1872)
dp_stat$plotGG + ggtitle("Dividend-Price Ratio (dp)")

dy_stat <- get_statistics(ts_annual, "dy", "rp_div", start=1872)
dy_stat$plotGG + ggtitle("Dividend Yield (dy)")

ep_stat <- get_statistics(ts_annual, "ep", "rp_div", start=1872)
ep_stat$plotGG + ggtitle("Earnings Price Ratio (ep)")

de_stat <- get_statistics(ts_annual, "de", "rp_div", start=1872)
de_stat$plotGG + ggtitle("Dividend Payout Ratio (dp)")

dfy_stat <- get_statistics(ts_annual, "dfy", "rp_div", start = 1919)
dfy_stat$plotGG + ggtitle("Default Yield Spread (dfy)")

dfr_stat <- get_statistics(ts_annual, "dfr", "rp_div", start=1926)
dfr_stat$plotGG + ggtitle("Default return spread (dfr)")

eqis_stat <- get_statistics(ts_annual, "eqis", "rp_div", start=1927)
eqis_stat$plotGG + ggtitle("Percent Equity Issuing")

infl_stat <- get_statistics(ts_annual, "infl", "rp_div", start=1919)
infl_stat$plotGG + ggtitle("Inflation")

ik_stat <- get_statistics(ts_annual, "ik", "rp_div", start=1947)
ik_stat$plotGG + ggtitle("Investment-Capital Ratio")

lty_stat <- get_statistics(ts_annual, "lty", "rp_div", start=1919)
lty_stat$plotGG + ggtitle("Long-Term Yield")

ltr_stat <- get_statistics(ts_annual, "ltr", "rp_div", start=1926)
ltr_stat$plotGG + ggtitle("Long-Term Return")

svar_stat <- get_statistics(ts_annual, "svar", "rp_div", start = 1885)
svar_stat$plotGG + ggtitle("Stock variance") #still needs work

bm_stat <- get_statistics(ts_annual, "b.m", "rp_div", start = 1921)
bm_stat$plotGG + ggtitle("Book to Market ratio")
 
tbl_stat <- get_statistics(ts_annual, "tbl", "rp_div", start=1920)
tbl_stat$plotGG + ggtitle("Treasury Bill Rate")

tms_stat <- get_statistics(ts_annual, "tms", "rp_div", start = 1920)
tms_stat$plotGG + ggtitle("Term spread")

ntis_stat <- get_statistics(ts_annual, "ntis", "rp_div", start=1927)
ntis_stat$plotGG + ggtitle("Net Equity Expansion")

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

adf.test(an_rp_div)



