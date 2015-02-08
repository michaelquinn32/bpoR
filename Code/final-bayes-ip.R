#       File: final-Bayes-ip.R
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
#                   It is Bayesian, assuming unknown mean and variance.
#                   The prior is informative.
##########################################

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
p = eff.plot(b3.eff, b3.optimal.point); print(p)
b3.perf = port.performance(b3.optimal.point)
Bayes.ip = port.summary(b3.perf)