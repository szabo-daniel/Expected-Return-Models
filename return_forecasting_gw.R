#Daniel Szabo 

library(data.table)
library(ggplot2)
library(forecast)

rm(list = ls())

monthly <- read.csv("PredictorData2021 - Monthly.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
monthly <- as.data.table(monthly)
monthly$Index <- as.numeric(gsub(",","", monthly$Index))
monthly$yyyymm <- as.Date(paste0(as.character(monthly$yyyymm), "01"), format = "%Y%m%d")

quarterly <- read.csv("PredictorData2021 - Quarterly.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
quarterly <- as.data.table(quarterly)
quarterly$Index <- as.numeric(gsub(",","", quarterly$Index))
#Convert date here

annual <- read.csv("PredictorData2021 - Annual.csv", na.strings = "NaN", stringsAsFactors = F, header = T)
annual <- as.data.table(annual)
annual$Index <- as.numeric(gsub(",","",annual$Index))

#Basic exploratory plots of raw index returns
ggplot(monthly, aes(x = yyyymm, y = Index)) + geom_line()
ggplot(annual, aes(x = yyyy, y = Index)) + geom_line()



