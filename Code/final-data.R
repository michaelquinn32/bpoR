#       File: final-data.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   This program contains all operations for getting the stock data. 
##########################################

# Get data
# 20th Century Fox, Microsoft, 3M, Hershey, GE, Google, Amazon, Risk free
# 1-3 Year Treasury Bonds iShares as a RF asset
port = c("FOX", "MSFT", "MMM", "HSY", "GE", "GOOGL", "AMZN", "SHY")
index = "^GSPC"         # SP500 Futures index

port.hist = getReturns(port,freq="week",
                       start="2010-01-01", end="2011-12-31")

port.test = getReturns(port,freq="week",
                       start="2012-01-01", end="2013-12-31")

baseline.hist = getReturns(index,freq="week",
                           start="2010-01-01", end="2011-12-31")

baseline.test = getReturns(index,freq="week",
                           start="2012-01-01", end="2013-12-31")

# 10 years of returns for "historical" means and variances
hist.prices=NULL
for(p in port)
{
  prices = get.hist.quote(p,quote="Adj",start="2001-01-01",
                          end="2013-12-31",compression="w")
  if(is.null(hist.prices))
  {
    hist.prices = prices
    colnames(hist.prices) = p
  }else hist.prices = cbind(hist.prices, prices)
}

colnames(hist.prices) = port

# Convert to Returns
hist.returns = diff(log(hist.prices))
hist.returns = exp(hist.returns)-1

#simplifies things with only complete cases
hist.returns= hist.returns[complete.cases(hist.returns),]
hist.means=apply(hist.returns,2,mean,na.rm=TRUE)
hist.cov = cov(hist.returns, use="complete.obs")