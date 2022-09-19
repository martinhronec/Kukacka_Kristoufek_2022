library(rstudioapi)
library(cusp)

rm(list=ls(all=TRUE)) # remove workspace
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # set working directory

#################
### Functions ###
#################

# read CSV data from location
initial_process <- function(loc) {
  data <- read.csv(loc, sep=';', dec=',')
  data <- data[complete.cases(data$AdrActCnt),] # remove observations with missing values
  
  return(data)
}

# stationarize chosen variables using first differences
diff_process <- function(data, var) {
  for (variable in var) {
    new_name <- paste(variable, "Diff", sep="")
    data[,new_name] <- c(0, diff(data[,variable]))
  }
  
  return(data)
}

# stationarize chosen variables using first log-differences
logdiff_process <- function(data, var) {
  for (variable in var) {
    new_name <- paste(variable, "LogDiff", sep="")
    data[,new_name] <- c(0, diff(log(data[,variable])))
  }
  
  return(data)
}

# stationarize variables using second log-differences
logdiffdiff_process <- function(data, var) {
  for (variable in var) {
    new_name <- paste(variable, "LogDiffDiff", sep="")
    data[,new_name] <- c(0, 0, diff(diff(log(data[,variable]))))
  }
  
  return(data)
}

# remove first two observations and normalize variables
final_process <- function(data, var) {
  return(data.frame(scale(data[3:dim(data)[1],var], center=TRUE, scale=TRUE)))
}

# estimate cusp catastrophe model
estimate_retSD_model <- function(data, fund, spec) {
  fund_form <- as.formula(paste("alpha", paste(fund, collapse=" + "), sep=" ~ "))
  spec_form <- as.formula(paste("beta", paste(spec, collapse=" + "), sep=" ~ "))
  
  fit <- cusp(y ~ retSD, fund_form, spec_form, data=data)
  print(summary(fit, logist=TRUE))
}

#######################
### Data Processing ###
#######################

location <- "data/BTC.csv" # dataset location
data <- initial_process(location) # read data from dataset location

# choose variables to stationarize using first differences
# variable SP is already stationary
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

# choose variables to stationarize using second log-differences
# variable HashRate must be stationarized for ETH => only uncomment the following two lines for ETH analysis
#to_logdiffdiff <- c("HashRate")
#data <- logdiff_process(data, to_logdiffdiff)

# select stationary versions of variables of interest
# variable SP is already stationary for BTC
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
data_retSD <- data$retSD[3:length(data$retSD)] # save original values of retSD
data <- final_process(data, to_keep) # normalize variables
data$retSD <- data_retSD # return original values of retSD to dataset

# rename variables
names(data) <- c("retSD",
                 "Addresses",
                 "Fees",
                 "Hashrate",
                 "Inflation",
                 "Velocity",
                 "SP500",
                 "USDEUR",
                 "VIX",
                 "Volume",
                 "ExchangeRatio",
                 "GoogleMarket",
                 "GoogleCurrency",
                 "Wiki")

#################################
### Cusp Catastrophe Modeling ###
#################################

# initial model estimation
fund <- c("Addresses", "Fees", "Hashrate", "Inflation", "Velocity", "SP500", "USDEUR") # fundamental variables
spec <- c("ExchangeRatio", "GoogleCurrency", "GoogleMarket", "VIX", "Volume", "Wiki") # speculative variables
estimate_retSD_model(data, fund, spec) # estimate model

# selection process according to the section "Estimation methodology and evaluation" in the paper

# final model estimation
fund <- c("Addresses", "Fees", "Velocity", "SP500") # fundamental variables
spec <- c("GoogleMarket", "VIX", "Volume", "Wiki") # speculative variables
estimate_retSD_model(data, fund, spec) # estimate model