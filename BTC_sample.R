rm(list=ls(all=TRUE))
setwd(dirname(parent.frame(2)$ofile))
source("src.R")

#######################
### Data Processing ###
#######################

currency <- "btc"
location <- "BTC.csv"

data <- initial_process(location)

to_diff <- c("AdrActCnt", 
             "FeeMeanUSD", 
             "HashRate", 
             "IssContPctAnn", 
             "NVTAdj", 
             "USDEUR",
             "VIX",
             "VolumeExch",
             "ExchangeRatio",
             "GoogleTrendsCrypto", 
             "GoogleTrends", 
             "Wiki")

data <- diff_process(data, to_diff)

to_keep <- c("retSD", 
             "AdrActCntDiff", 
             "FeeMeanUSDDiff", 
             "HashRateDiff", 
             "IssContPctAnnDiff", 
             "NVTAdjDiff", 
             "SP",
             "USDEURDiff",
             "VIXDiff",
             "VolumeExchDiff",
             "ExchangeRatioDiff",
             "GoogleTrendsCryptoDiff", 
             "GoogleTrendsDiff", 
             "WikiDiff")

data_retSD <- data$retSD[3:length(data$retSD)]
data <- final_process(data, to_keep)
data$retSD <- data_retSD

##################################
### Cusp Catastrophe Modeling ###
##################################

### Model 5:
modeln <- "05"
fund <- c("AdrActCntDiff", "FeeMeanUSDDiff", "NVTAdjDiff", "SP")
spec <- c("GoogleTrendsCryptoDiff", "VIXDiff", "VolumeExchDiff", "WikiDiff")
estimate_model_retSD(modeln, fund, spec, data, currency)