## Program to compute CAPM for tickers in the file betas.csv and then optimize the portfolio
## File format  
##              Column 1: ticker
##              Column 2: betaVal
##  The rows are filled with each stock
##  Example:
## 	  ticker	betaVal
##   	MCD	    0.60
##   	MA	    1.13
##
##  John Lewis 12 October 2021
##  Updated 14 Oct 2021 to compute the tangent portfolio and the number of shares to buy
##
##  !!!! Must manually change the code to use mean return or CAPM expected returns in covtEstimator function
##

rm(list=ls())
getwd()

library(quantmod)
library(timeSeries)
library(fPortfolio)
library(caTools)

#Turn off scientific notation
options(scipen=999)  #turn it back on with scipen=0

#set total size of the portfolio
portfolioAmount <- 100000

# Both numbers below are 12 months out
SP500ExpectedReturn <- 0.23
riskFreeRate <- .00115

# Read the betas file.  It is read into a data frame
betasFile <- read.csv("betas.csv",header=TRUE)

# Convert tickers to string from factor
betasFile$ticker <- as.character(betasFile$ticker)

# Store the tickers in a tickerlist vector - as we have done in the past
tickerList <- betasFile$ticker

##The following 12 lines are from the optimizer code
closingPricesRead <- NULL
for (ticker in tickerList)
  closingPricesRead <- cbind(closingPricesRead,
                             getSymbols(ticker, from="2016-01-01", verbose=TRUE, auto.assign=FALSE)[,4]) # [,6] = keep the adjusted prices

# keep only the dates that have closing prices for all tickers
# Delete by rows - will not work for all instances
# closingPrices <- closingPricesRead[apply(closingPricesRead,2,function(x) all(!is.na(x))),]
#  Delete by columns
closingPrices <- closingPricesRead[ , ! apply( closingPricesRead , 2 , function(x) any(is.na(x)) ) ]

#Update tickerList with names of just the tickers with full data
tickerList <- sub("([^.]*).*", "\\1", colnames(closingPrices)) 

#
if (nrow(betasFile) > length(tickerList)) {
  betasFileAdjusted <- betasFile[betasFile$ticker %in% tickerList,]
  message("Some of your stocks did not have enough data and were deleted.")
} else {
  betasFileAdjusted <- betasFile
}


# Compute the monthly returns
returns <- as.timeSeries((tail(closingPrices,-1) / as.numeric(head(closingPrices,-1)))-1)

meanReturns <- colMeans(returns)

#Compute expected returns from CAPM.  You have the betas and returns.  For example:
#
# To show the latest returns for my company GE, I would enter:
tail(returns$GE.Close, n=1)  #n=1 forces R to display just the last value.  Otherwise, r will display the last 6 or so

# Since we are in R, we can compute the CAPM at one go using vectors and data frames
# and then store them in a new column in betasFile
betasFileAdjusted$ExpectedReturns <- riskFreeRate + betasFileAdjusted$betaVal * (SP500ExpectedReturn - riskFreeRate) 


# We could have put the results in a vector
expectedReturns <- riskFreeRate + betasFileAdjusted$betaVal * (SP500ExpectedReturn - riskFreeRate) 

# We can write out the results to a csv file
write.csv(betasFileAdjusted,"capm.csv")

#Create the estimator matrix for the optimization
covtEstimator <- function (x,data,spec) {
  x.mat = as.matrix(x)
  
  message("covtEstimator")

  #Using Means as expected Returns    
  list(mu=colMeans(x.mat),Sigma=MASS::cov.trob(x.mat)$cov)
  
  #Use computed expected returns
  #list(mu=expectedReturns,Sigma=MASS::cov.trob(x.mat)$cov)  
  
}

#setup min and max constraints
minMaxConstraints <- c("minW[1:length(tickerList)]=.01","maxW[1:length(tickerList)]=.30")


# Calculate Efficient Frontier with longonly constraints

defaultSpec <- portfolioSpec()
setRiskFreeRate(defaultSpec) <- riskFreeRate
setEstimator(defaultSpec) <- 'covtEstimator'
covtFrontier <- portfolioFrontier(returns, defaultSpec, constraints = "LongOnly")

#plot the efficient frontier
plot(covtFrontier,1)

#graph the weights of each security at each risk level computed
covtallocations <- getWeights(covtFrontier@portfolio) # get allocations for each instrument for each point on the efficient frontier
colnames(covtallocations) <- tickerList
barplot(t(covtallocations), col=rainbow(ncol(covtallocations)+2), legend=colnames(covtallocations))

constraintsFrontier <- portfolioFrontier(returns, constraints = minMaxConstraints)

#plot the efficient frontier
plot(constraintsFrontier,1)

#graph the weights of each security at each risk level computed
constraintsAllocations <- getWeights(constraintsFrontier@portfolio) # get allocations for each instrument for each point on the efficient frontier
colnames(constraintsAllocations) <- tickerList
barplot(t(constraintsAllocations), col=rainbow(ncol(constraintsAllocations)+2), legend=colnames(constraintsAllocations))


#display the correlations of the returns for the securities in the portfolio
cor(returns)

##
## Let's make life easier by computing the tangent portfolio (the only one in which we should invest)
## and compute the number of shares for each stock
##

#Now compute for the tangent portfolio
tangentPortfolio <- tangencyPortfolio(returns, defaultSpec, constraints = minMaxConstraints)

#display the weights
message("Weights of portfolio with min max constraints of:",minMaxConstraints)
getWeights(tangentPortfolio)

#compute the number of shares for each stock in the portfolio
tangentConstraintsShares <- (getWeights(tangentPortfolio) * portfolioAmount) / tail(closingPrices, n=1)

#display the number of shares
message("Number of shares for tangency portfolio with constraints")
tangentConstraintsShares

##
##Now compute the tangent portfolio without constraints
##
constraints <- "minW[1:length(tickerList)]=-1"  # Use this line to permit shorts
# Can you guess what will be in the next iteration 

tangentPortfolioLongOnly <- tangencyPortfolio(returns, defaultSpec,constraints = "LongOnly")

#display the weights
message("Weight of tangent portfolio constraints = LongOnly")
getWeights(tangentPortfolioLongOnly)

#compute the number of shares for each stock in the portfolio
tangentLongOnlyShares <- (getWeights(tangentPortfolioLongOnly) * portfolioAmount) / tail(closingPrices, n=1)

#display the number of shares
message("Number of shares for tangency portfolio with constraints = LongOnly")
tangentLongOnlyShares



