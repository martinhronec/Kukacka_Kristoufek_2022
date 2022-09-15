library(cusp)

# loc - location of the dataset
# return - dataset from the location with missing values removed
initial_process <- function(loc, beg="None", end="None") {
  data <- read.csv(loc, sep=';', dec=',')
  data <- data[complete.cases(data$AdrActCnt),]
  
  if (is.factor(data$retSD) | is.character(data$retSD)) {
    data$retSD <- as.numeric(gsub(",", ".", as.character(data$retSD)))
  }
  
  if (is.factor(data$ExchangeRatio) | is.character(data$ExchangeRatio)) {
    data$ExchangeRatio <- as.numeric(gsub(",", ".", as.character(data$ExchangeRatio)))
  }
  
  if (is.character(data$Wiki)) {
    data$Wiki <- as.numeric(data$Wiki)
  }
  
  if (end != "None") {
    data$date <- as.POSIXlt(as.character(data$date), format="%D-%M-%Y")
    data_subset_1 <- (data$date >= as.POSIXlt(beg, format="%d/%m/%Y"))
    data <- data[data_subset_1,]
    data_subset_2 <- (data$date < as.POSIXlt(end, format="%d/%m/%Y"))
    data <- data[data_subset_2,]
  } else if (beg != "None") {
    data$date <- as.POSIXlt(as.character(data$date), format="%D-%M-%Y")
    data_subset <- (data$date >= as.POSIXlt(beg, format="%d/%m/%Y"))
    data <- data[data_subset,]
  }
  
  return(data)
}

# data - dataset containing variables to be stationarized
# var - names of variables to be stationarized
diff_process <- function(data, var) {
  for (variable in var) {
    new_name <- paste(variable, "Diff", sep="")
    data[,new_name] <- c(0, diff(data[,variable]))
  }
  
  return(data)
}

# data - dataset containing variables to be processed
# var - names of variables to be kept
final_process <- function(data, var) {
  return(data.frame(scale(data[3:dim(data)[1],var], center=TRUE, scale=TRUE)))
}

# data
# start
# dep
# fund
# spec
# name
estimate_retSD_model <- function(data, start, fund, spec, name) {
  end <- dim(data)[1]
  length <- end-start+1
  
  fund_form <- as.formula(paste("alpha", paste(fund, collapse=" + "), sep=" ~ "))
  spec_form <- as.formula(paste("beta", paste(spec, collapse=" + "), sep=" ~ "))
  
  fit <- cusp(y ~ retSD, fund_form, spec_form, data=data[start:end,])
  sum <- summary(fit, logist=TRUE)
  
  print(sum)
}

estimate_model_retSD <- function(num, fund, spec, data, cur, beg=1) {
  file <- paste(sep="", "results/", cur, "/", cur, "_model", num)
  estimate_retSD_model(data, beg, fund, spec, file)
}