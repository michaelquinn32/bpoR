#       File: final-naive.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   This program generates a "naive" optimal portfolio from last
#                   three months' data. 
##########################################

# Descriptive Stats last three months
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
p = ggplot(hist.plot,aes(x=Date,y=value))
p = p + geom_line(aes(group=variable,colour=variable)) + ggtitle("Cumulative Returns for Each Stock, Previous 3 Months") +
  labs(y="Cumulative Return") + scale_colour_discrete(name="Ticker Name")
print(p)

# Basic Stats, Naive Estimates
stocks.table = data.frame(rbind(52*Means, sqrt(52)*StDev, 
                                52*Means/(sqrt(52)*StDev)))
row.names(stocks.table) = c("Annualized Return", "Annualized St. Dev.", 
                            "Sharpe Ratio")
stocks.table[1:2,] = 100*stocks.table[1:2,]
stocks.table = round(stocks.table,4)

# Make Naive Port
eff = make.port(Means, Cov,.5,-.5)
eff.optimal.point = eff[eff$sharpe==max(eff$sharpe),]

# Efficient Frontier and performance
p = eff.plot(eff, eff.optimal.point); print(p)
perf = port.performance(eff.optimal.point)

# Assessing performance
Basic = port.summary(perf)