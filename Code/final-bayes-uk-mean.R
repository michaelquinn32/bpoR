#       File: final-Bayes-uk-mean.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   This program estimates the parameters for an optimal portfolio.
#                   It is Bayesian, assuming an unknown mean.
##########################################

# Bayesian Mean estimate, Gibbs Sampler, Known Variance
m = start-end
mh = as.numeric(dim(hist.returns)[1]) # Adjustment to prior cov
l0 = 1/mh*hist.cov                      # Historical variance
m0 = apply(hist.returns,2,mean)  # historical means
m0 = m0 + abs(m0*.2)             # pos adj. -- Analyst predicts growth
s = 1/m*Cov                       # sample Cov for mu
xbar = Means                    # sample means

mun = solve(solve(l0) + m*solve(s)) %*% (solve(l0) %*% m0 + m*solve(s) %*% xbar)
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
p = eff.plot(b1.eff, b1.optimal.point); plot(p)
b1.perf = port.performance(b1.optimal.point)
Bayes.Mean =port.summary(b1.perf) 