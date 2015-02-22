#       File: final-describe.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   This program generates descriptive statistics for the stock data
##########################################

# Descriptive Stats last six months
end = 1
start = end + 23
Means = apply(port.hist$R[start:end,],2,mean)
StDev = apply(port.hist$R[start:end,],2,sd)
Total = apply(port.hist$R[start:end,],2,sum) 
Cov = cov(port.hist$R[start:end,])
n = length(port)

# Plot of stock returns
market = baseline.hist$R[start:end,]
market = c(1,market)
market = cumsum(market)
hist.dates = as.Date(rownames(port.hist$R[(start+1):end,]))
return.out = port.hist$R[start:end,]
return.out = rbind(1,return.out)
return.out = apply(return.out,2,cumsum)
port.out = data.frame(SP500 = market, return.out, Date=hist.dates)
hist.plot = melt(port.out,id="Date")

# Make Plot
p.returns = ggplot(hist.plot,aes(x=Date,y=value))
p.returns = p.returns + geom_line(aes(group=variable,colour=variable)) + 
  ggtitle("Cumulative Returns, Previous 3 Months") +
  labs(y="Cumulative Return") + 
  scale_colour_discrete(name="Ticker Name") +
  theme(legend.position="bottom") + 
  guides(col = guide_legend(nrow = 2))
print(p.returns)

# Basic Stats, historical
stocks.table = data.frame(rbind(52*Means, sqrt(52)*StDev, 
                                52*Means/(sqrt(52)*StDev)))
row.names(stocks.table) = c("Annualized Return", "Annualized St. Dev.", 
                            "Sharpe Ratio")
stocks.table[1:2,] = 100*stocks.table[1:2,]

# Basic Stats, Test port
# Looking forward to get the means
New.Means=apply(port.test$R,2,mean)
New.StDev = apply(port.test$R,2,sd)
New.Total = apply(port.test$R,2,sum) 
New.Cov = cov(port.test$R)

test.table = data.frame(rbind(52*New.Means, sqrt(52)*New.StDev, 
                                52*New.Means/(sqrt(52)*New.StDev)))
row.names(test.table) = c("Annualized Return", "Annualized St. Dev.", 
                            "Sharpe Ratio")
test.table[1:2,] = 100*test.table[1:2,]
