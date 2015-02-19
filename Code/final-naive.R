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

# Make Naive Port
eff = make.port(Means, Cov,.5,-.5)
eff.optimal.point = eff[eff$sharpe==max(eff$sharpe),]

# Efficient Frontier and performance
p.eff.naive = eff.plot(eff, eff.optimal.point); print(p.eff.naive)
perf = port.performance(eff.optimal.point)

# Assessing performance
Basic = port.summary(perf)