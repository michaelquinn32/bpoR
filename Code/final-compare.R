#       File: final-compare.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   Various scripts for comparing the results of the previous
#                   scripts.
##########################################

# Final Comparison
compare = rbind(Market=Market, 
                Basic= Basic, 
                Foresight = Foresight, 
                "Unkown Mean" = Bayes.Mean,
                "Noninformative Prior" = Bayes.ni,
                "Informative Prior" = Bayes.ip)

compare = round(100*compare,2)
colnames(compare) = c("Expected Return", "Risk", "Annualized Sharpe")

# Comparing Weights
final.weights = rbind(Baseline= eff.optimal.point, 
                      Foresight = new.eff.optimal.point, 
                      "Unkown Mean" = b1.optimal.point,
                      "Noninformative Prior" = b2.optimal.point,
                      "Informative Prior" = b3.optimal.point)

# Cleaning up
final.weights = round(100*final.weights[,1:8],2)

# Simulated probabilities
market = rev(baseline.test$R)
market[1] = baseline.test$R[1]+1
market = cumsum(market)
final.returns = data.frame(Market = market,
                           b1 = b1.perf$tret,
                           b2 = b2.perf$tret,
                           b3 = b3.perf$tret,
                           naive = perf$tret,
                           Foresight = nperf$tret, 
                           Date = rev(as.Date(row.names((baseline.test$R)))))

ret.out = melt(final.returns, id.vars="Date")

p.cumret = ggplot(ret.out, aes(x=Date,y=value))
p.cumret = p.cumret + geom_line(aes(group=variable, colour =variable)) + 
  scale_colour_discrete(name="Test Portfolio",
                        labels=c("Market","Unknown Mean", "Noninformative Prior",
                                 "Informative Prior", "Baseline","Foresight")) +
  ggtitle("Cumulative Returns for Each Tested Portfolio") +
  labs(y="Cumulative Return") +
  theme(legend.position="bottom") + guides(col = guide_legend(nrow = 2)) +
  labs(y="Cumulative Return")
print(p.cumret)