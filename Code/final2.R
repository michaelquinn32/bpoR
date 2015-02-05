#       File: final.rnw
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying codefor a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
##########################################

library(stockPortfolio) # Stock data
library(ggplot2)        # Better plots
library(reshape2)       # For plots
library(quadprog)       # Used to solve Markowitz problem
library(MCMCpack)       # More distributions
library(mvtnorm)        # Multivariate Normal Distribution
library(tseries)
library(zoo)            # Required by tseries
library(xtable)

# Portfolio Optimizer
make.port = function(means, covariance, max.allocation=.5,min.allocation=0,
                          risk.premium.up=.5, risk.increment=.005)
{
  n = ncol(Cov)
  
  # Matrices to solve with quadprog
  Amat = matrix (1, nrow=n)
  bvec = 1
  meq  = 1
  
  # Min Constraints
  Amat = cbind(1, diag(n))
  bvec = c(bvec, rep(min.allocation, n))
  
  # Max Constraints
  Amat = cbind(Amat, -diag(n))
  bvec = c(bvec, rep(-max.allocation, n))
  
  # Set up the number of points to estimate port frontier
  loops = risk.premium.up / risk.increment + 1
  loop = 1
  
  # Initialize a matrix to contain allocation and statistics
  eff = matrix(nrow=loops, ncol=n+3)
  
  # Now I need to give the matrix column names
  colnames(eff) = c(colnames(covariance), "Std.Dev", "Exp.Return", "sharpe")
  
  # Loop through the quadratic program solver
  for (i in seq(from=0, to=risk.premium.up, by=risk.increment))
    {
    dvec = means * i # This moves the solution along the EF
    sol = solve.QP(covariance, dvec=dvec, Amat=Amat, bvec=bvec, meq=meq)
    eff[loop,"Std.Dev"] = sqrt(sum(sol$solution*colSums((covariance*sol$solution))))
    eff[loop,"Exp.Return"] = as.numeric(sol$solution %*% means)
    eff[loop,"sharpe"] = eff[loop,"Exp.Return"] / eff[loop,"Std.Dev"]
    eff[loop,1:n] = sol$solution
    loop = loop + 1
  }
  
  return(as.data.frame(eff))
}

# Plot port and efficient Frontier
eff.plot=function(eff, eff.optimal.point)
{
  ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_line(col="Red") +
    geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
               size=5) +
    annotate(geom="text", x=eff.optimal.point$Std.Dev,
             y=eff.optimal.point$Exp.Return,
             label=paste("Risk: ",
                         round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
                         round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                         round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
             hjust=0, vjust=1.2) +
    ggtitle("Efficient Frontier and Optimal Portfolio, Basic Parameters") +
    labs(x="Risk (standard deviation of portfolio)", y="Return")
}

# Portfolio performance
port.performance = function(eff.optimal.point)
{
  n = dim(eff.optimal.point)[2] - 3
  weights = as.matrix(eff.optimal.point[1,1:n])
  out = port.test$R%*%t(weights)
  tlab = sort(as.Date(row.names(out)),decreasing=FALSE)
  out = rev(out)
  out[1]= 1 + out[1]
  tret = cumsum(out)
  return(list(tret = tret, tlab = tlab, weights = weights))
}

# Quick summary
port.summary = function(performance)
{
  return(c(52*mean(port.test$R%*%t(performance$weights)), 
           sqrt(52)*sd(port.test$R%*%t(performance$weights)),
           52*mean(port.test$R%*%t(performance$weights))/ 
             (sqrt(52)*sd(port.test$R%*%t(performance$weights)))))
}

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
ggplot(hist.plot,aes(x=Date,y=value, group=variable,colour=variable)) +
  geom_line() + ggtitle("Cumulative Returns for Each Stock, Previous 3 Months") +
  labs(y="Cumulative Return") + scale_colour_discrete(name="Ticker Name")

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
eff.plot(eff, eff.optimal.point)
perf = port.performance(eff.optimal.point)

plot(baseline.test,ylim=c(.9,2))
lines(x=perf$tlab, y =perf$tret, type='l', col=2)

#Motivating Example, later means help?
New.Means=apply(port.test$R,2,mean)
New.Cov = cov(port.test$R)
new.eff = make.port(New.Means, New.Cov,.5,-.5)
new.eff.optimal.point = new.eff[new.eff$sharpe==max(new.eff$sharpe),]

nperf = port.performance(new.eff.optimal.point)
lines(x=nperf$tlab, y =nperf$tret, type='l', col=3)
legend("topleft", legend = c("S&P 500", "Basic Portfolio", "Insight Portfolio"), 
       lty=1, col=c(1,2,3))

eff.plot(eff, new.eff.optimal.point)

# Summarizing for final tables
Market = c(52*mean(baseline.test$R), sqrt(52)*sd(baseline.test$R), 
           52*mean(baseline.test$R)/(sqrt(52)*sd(baseline.test$R)))

Basic = port.summary(perf)
Insight = port.summary(nperf)

# Bayesian Mean estimate, Gibbs Sampler, Known Variance
m = start-end
mh = as.numeric(dim(hist.returns)[1]) # Adjustment to prior cov
l0 = 1/mh*hist.cov                      # Historical variance
m0 = apply(hist.returns,2,mean)  # historical means
m0 = m0 + abs(m0*.2)             # pos adj. -- Analyst predicts growth
s = 1/m*Cov                       # sample Cov for mu
xbar = Means                    # sample means

mun = solve(solve(l0) + m*solve(s)) %*% (solve(l0) %*% 
                            m0 + m*solve(s) %*% xbar)
ln = solve(solve(l0) + m*solve(s))

gibbs.known.norm = function(burn.in, samp.size,mun, ln)
{
  k = length(mun)
  mu.sample = matrix(0,nrow=samp.size,ncol=k)
  mus = rep(0,k)
  betas = NULL
  for(i in 1:(burn.in + samp.size))
  {
    for(j in 1:k)
    {
      betas = ln[j,-j] %*% solve(ln[-j,-j])
      mj = mun[j] + betas %*% (mus[-j] - mun[-j])
      lj = ln[j,j] - ln[j,-j] %*% solve(ln[-j,-j]) %*% ln[-j,j]
      
      mus[j] = rnorm(1,mj,sqrt(lj))
    }
    if(i>burn.in)
    {
      mu.sample[i-burn.in,] = mus
    }
  }
  return(list(sample=mu.sample, mus=apply(mu.sample,2,mean)))
}

Bayes.Means = gibbs.known.norm(burn.in=5000,samp.size=10000,mun,ln)
b1.eff = make.port(Bayes.Means$mus, Cov,.5,-.5)
b1.optimal.point = b1.eff[b1.eff$sharpe==max(b1.eff$sharpe),]

# Efficient Frontier and performance
eff.plot(b1.eff, b1.optimal.point)
b1.perf = port.performance(b1.optimal.point)
Bayes.Mean =port.summary(b1.perf) 

# Restimate Cov and Mus, treat both as unknown, non-informative prior

# Sum of squares matrix
S = matrix(0,nrow=dim(port.hist$R)[2],ncol=dim(port.hist$R)[2])

for(i in end:start)
{
  S = S + (port.hist$R[i,]-Means) %*% t(port.hist$R[i,]-Means)
}

gibbs.unknown.ni = function(burn.in, samp.size,S,m,xbar)
{
  mus = rep(0,length(xbar))
  sigmas = matrix(0, nrow=dim(S)[1], ncol=dim(S)[2])
  mu.sample = matrix(0,nrow=samp.size,ncol=length(xbar))
  sigma.sample = matrix(0, nrow=dim(S)[1], ncol=dim(S)[2])
  
  for(i in 1:(burn.in + samp.size))
  {
    sigmas = riwish(m-1,S)
    mus = mvrnorm(n=1,mu=xbar,Sigma=sigmas/m)
      
    if(i>burn.in)
    {
      mu.sample[i-burn.in,] = mus
      sigma.sample = sigma.sample + sigmas
    }
  }
  return(list(mu.sample=mu.sample, mus=apply(mu.sample,2,mean),
              sigmas=1/samp.size * sigma.sample))
}

Bayes.unknown.ni = gibbs.unknown.ni(burn.in=5000,samp.size=10000,S,m,Means)

cov.out = Bayes.unknown.ni$sigmas
row.names(cov.out) = port
colnames(cov.out) = port
b2.eff = make.port(Bayes.unknown.ni$mus, cov.out,.5,-.5)
b2.optimal.point = b2.eff[b2.eff$sharpe==max(b2.eff$sharpe),]

# Efficient Frontier and performance
eff.plot(b2.eff, b2.optimal.point)
b2.perf = port.performance(b2.optimal.point)
Bayes.ni = port.summary(b2.perf)

# Both Unknown, Informative Prior
k0 = mh
m = start-end
v0 = k0-1
# m0 = apply(hist.returns,2,mean)  # historical means
# m0 = m0 + abs(m0*.2)             # pos adj. -- Analyst predicts growth
# l0 = 1/mh*hist.cov  # Historical variance
# s = 1/m*Cov                       # sample Cov for mu
# xbar = Means                    # sample means 

mun = k0/(k0 + m) * m0 + m/(k0 + m) *xbar
kn = k0 + m
vn = v0 + m
ln = l0 + S + k0 * m/(k0 + m)* (xbar - m0) %*% t(xbar - m0)

gibbs.unknown.ip = function(burn.in, samp.size,vn,ln,mun,kn)
{
  mus = rep(0,length(mun))
  sigmas = matrix(0, nrow=dim(ln)[1], ncol=dim(ln)[2])
  mu.sample = matrix(0,nrow=samp.size,ncol=length(mun))
  sigma.sample = matrix(0, nrow=dim(ln)[1], ncol=dim(ln)[2])
  
  for(i in 1:(burn.in + samp.size))
  {
    sigmas = riwish(vn,solve(ln))
    mus = mvrnorm(n=1,mu=mun,Sigma=sigmas/kn)
    
    if(i>burn.in)
    {
      mu.sample[i-burn.in,] = mus
      sigma.sample = sigma.sample + sigmas
    }
  }
  return(list(mu.sample=mu.sample, mus=apply(mu.sample,2,mean),
              sigmas=1/samp.size * sigma.sample))
}
  
Bayes.unknown.ip = gibbs.unknown.ip(burn.in=5000, samp.size=10000,vn,ln,mun,kn)

cov.out = Bayes.unknown.ip$sigmas
row.names(cov.out) = port
colnames(cov.out) = port
b3.eff = make.port(Bayes.unknown.ip$mus, cov.out,.5,-.5)
b3.optimal.point = b3.eff[b3.eff$sharpe==max(b3.eff$sharpe),]

# Efficient Frontier and performance
eff.plot(b3.eff, b3.optimal.point)
b3.perf = port.performance(b3.optimal.point)
Bayes.ip = port.summary(b3.perf)

# Final Comparison
compare = rbind(Market=Market, 
                Basic= Basic, 
                Insight = Insight, 
                "Bayesian, Unkown Mean" = Bayes.Mean,
                "Bayesian, Noninformative Prior" = Bayes.ni,
                "Bayesian, Informative Prior" = Bayes.ip)

compare = round(100*compare,2)
colnames(compare) = c("Annualized Return", "Annualized Risk", "Annualized Sharpe")

# Comparing Weights
final.weights = rbind(Baseline= eff.optimal.point, 
                      Insight = new.eff.optimal.point, 
                      "Bayesian, Unkown Mean" = b1.optimal.point,
                      "Bayesian, Noninformative Prior" = b2.optimal.point,
                      "Bayesian, Informative Prior" = b3.optimal.point)

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
                       insight = nperf$tret, 
                       Date = rev(as.Date(row.names((baseline.test$R)))))

ret.out = melt(final.returns, id.vars="Date")

ggplot(ret.out, aes(x=Date,y=value, group=variable, colour =variable)) + 
  geom_line() + scale_colour_discrete(name="Test Portfolio",
                       labels=c("Market","Unknown Mean", "Noninformative Prior",
                                "Informative Prior", "Baseline","Insight")) +
  ggtitle("Cumulative Returns for Each Tested Portfolio") +
  labs(y="Cumulative Return")
  
# Simulating distributions of returns calculating probabilities
# Weights-matrix
w.matrix = t(rbind(eff.optimal.point[1:8],
                 new.eff.optimal.point[1:8],
                 b1.optimal.point[1:8],
                 b2.optimal.point[1:8],
                 b3.optimal.point[1:8]))

colnames(w.matrix) = row.names(final.weights)

sim.means = c(New.Means, Market=mean(baseline.test$R))
sim.Cov = cov(cbind(port.test$R, Market=baseline.test$R))

# 5000 Samples
n = 5000
m = dim(port.test$R)[1]
out= matrix(0, nrow = n, ncol = dim(w.matrix)[2]+1)
contribution = matrix(0, nrow = n * dim(w.matrix)[1], ncol = dim(w.matrix)[2])

for(i in 1:n)
{
  sample = rmvnorm(m,sim.means,sim.Cov)
  market = sample[,9]
  market[1] = market[1] + 1
  
  # Contribution
  cmat = matrix(0, nrow= dim(w.matrix)[1], ncol = dim(w.matrix)[2])
  
  for(j in 1: dim(w.matrix)[2])
  {
    cmat[,j] =  diag(w.matrix[,j]) %*% t(sample[,-9]) %*% 
      +     matrix(1, nrow = m, ncol=1)
  }
  
  # ready for output
  ports = sample[,-9] %*% w.matrix
  tot = apply(ports,2,sum)
  
  contribution[((i-1)*dim(w.matrix)[1] + 1):(i*dim(w.matrix)[1]),] = 
    cmat %*% solve(diag(tot))
  out[i,] = c(tot + 1,Market=sum(market))
}

# Add an index to contribution

colnames(contribution) = colnames(w.matrix)
idx = rep(row.names(w.matrix),n)

cont.means = aggregate(contribution,list(var=idx),mean)
row.names(cont.means) = cont.means[,1]
cont.means = cont.means[,-1]

cont.sds = aggregate(contribution,list(var=idx),sd)
row.names(cont.sds) = cont.sds[,1]
cont.sds = cont.sds[,-1]

# Sample Stats
sim.r.means = apply(out,2,mean)
names(sim.r.means) = c(colnames(w.matrix),"Market")
sim.r.cov = cov(out)
colnames(sim.r.cov) = c(colnames(w.matrix),"Market")
row.names(sim.r.cov) = c(colnames(w.matrix),"Market")
sd.r = sqrt(diag(sim.r.cov))

# Calculate Probs
# P(X > Y) = P(Y - X < 0)
probs = rep(0, dim(out)[2]-1)
names(probs) = c(colnames(w.matrix))
for(i in 1:(dim(out)[2]-1))
{
  diff = c(-1,1)
  d = c(i,6)
  m = sim.r.means[d]
  v = sim.r.cov[d,d]
  m1 = m %*% diff
  v1 = diff %*% v %*% diff
  probs[i] = pnorm(0,mean=m1,sd=sqrt(v1))
}

# Distribution plots
colnames(out) = c(colnames(w.matrix),"Market")
dist.plots = melt(out)
df.out = as.data.frame(out)
name = colnames(out)[1]

ggplot(df.out, aes(y=Baseline, x=Market)) + geom_point(alpha=.5) +
  geom_density2d() + annotate("segment", x=0.75,y=0.75,xend=2,yend=2, 
                              colour="blue")

plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle(paste0("Densities for Simulated Portfolios, 104 Weeks in Test Period\n",
                   "P(>Market = ", round(probs[1],4),")"))

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",guide=FALSE) + 
  ggtitle("Difference in Densities")

name = colnames(out)[2]
plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle("Densities for Simulated Portfolios, 104 Weeks in Test Period")

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",name="Portfolio") + 
  ggtitle("Difference in Densities")

name = colnames(out)[3]
plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle("Densities for Simulated Portfolios, 104 Weeks in Test Period")

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",name="Portfolio") + 
  ggtitle("Difference in Densities")

name = colnames(out)[4]
plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle("Densities for Simulated Portfolios, 104 Weeks in Test Period")

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",name="Portfolio") + 
  ggtitle("Difference in Densities")

colnames(df.out)[5] = "IP"
ggplot(df.out, aes(y=IP, x=Market)) + geom_point(alpha=.5) + geom_density2d() + 
  annotate("segment", x=0.75,y=0.75,xend=2,yend=2, colour="blue") +
  ylab("Bayesian, Informative Prior") + 
  ggtitle("Joint Distribution")

name = colnames(out)[5]
plots = dist.plots[dist.plots$Var2 %in% c(name,"Market"),]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_discrete(name="Portfolio") + 
  ggtitle("Densities")

plots = dist.plots[dist.plots$Var2 %in% name,]
plots[,3] = dist.plots[dist.plots$Var2 %in% "Market",3] -
  dist.plots[dist.plots$Var2 %in% name,3]
ggplot(plots, aes(x=value, fill=Var2)) + geom_density(alpha=.3) +
  scale_fill_manual(values="blue",name="Portfolio") + 
  ggtitle("Difference in Densities")
