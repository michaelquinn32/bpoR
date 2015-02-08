#       File: final-foresight.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   This program generates a "foresight" optimal portfolio. 
#                   Here, we assume that the analyst knows future means and variances
##########################################

# Looking forward to get the means
New.Means=apply(port.test$R,2,mean)
New.Cov = cov(port.test$R)
new.eff = make.port(New.Means, New.Cov,.5,-.5)
new.eff.optimal.point = new.eff[new.eff$sharpe==max(new.eff$sharpe),]

nperf = port.performance(new.eff.optimal.point)
lines(x=nperf$tlab, y =nperf$tret, type='l', col=3)
legend("topleft", legend = c("S&P 500", "Basic Portfolio", "Foresight Portfolio"), 
       lty=1, col=c(1,2,3))

p = eff.plot(eff, new.eff.optimal.point); print(p)

# Summarizing for final tables
Market = c(52*mean(baseline.test$R), sqrt(52)*sd(baseline.test$R), 
           52*mean(baseline.test$R)/(sqrt(52)*sd(baseline.test$R)))

Foresight = port.summary(nperf)
