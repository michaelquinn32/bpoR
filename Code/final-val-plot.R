#       File: final-val-plot.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   A set of plots for the validation stricts that came before. 
##########################################

# Distribution plots
colnames(out) = c(colnames(w.matrix),"Market")
dist.plots = melt(out)
df.out = as.data.frame(out)
name = colnames(out)[1]

p = ggplot(df.out, aes(y=Baseline, x=Market)) + geom_point(alpha=.5) +
  geom_density2d() + annotate("segment", x=0.75,y=0.75,xend=2,yend=2, 
                              colour="blue")
print(p)

plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle(paste0("Densities for Simulated Portfolios, 104 Weeks in Test Period\n",
                 "P(>Market = ", round(probs[1],4),")"))
print(p)

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",guide=FALSE) + 
  ggtitle("Difference in Densities")
print(p)

name = colnames(out)[2]
plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle("Densities for Simulated Portfolios, 104 Weeks in Test Period")
print(p)

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",name="Portfolio") + 
  ggtitle("Difference in Densities")
print(p)

name = colnames(out)[3]
plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle("Densities for Simulated Portfolios, 104 Weeks in Test Period")
print(p)

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",name="Portfolio") + 
  ggtitle("Difference in Densities")
print(p)

name = colnames(out)[4]
plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle("Densities for Simulated Portfolios, 104 Weeks in Test Period")
print(p)

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",name="Portfolio") + 
  ggtitle("Difference in Densities")
print(p)

colnames(df.out)[5] = "IP"
p = ggplot(df.out, aes(y=IP, x=Market)) + geom_point(alpha=.5) + geom_density2d() + 
  annotate("segment", x=0.75,y=0.75,xend=2,yend=2, colour="blue") +
  ylab("Bayesian, Informative Prior") + 
  ggtitle("Joint Distribution")
print(p)

name = colnames(out)[5]
plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle("Densities")
print(p)

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
p = ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",name="Portfolio") + 
  ggtitle("Difference in Densities")
print(p)