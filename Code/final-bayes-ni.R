#       File: final-Bayes-ni.R
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
#                   The prior is non-informative.
##########################################

# Sum of squares matrix
S = matrix(0,nrow=dim(port.hist$R)[2],ncol=dim(port.hist$R)[2])

for(i in end:start)
{
  S = S + (port.hist$R[i,]-Means) %*% t(port.hist$R[i,]-Means)
}

# A function for the gibbs sampler
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

# Run the sampler
Bayes.unknown.ni = gibbs.unknown.ni(burn.in=5000,samp.size=10000,S,m,Means)

# Prepping for the portfolio optimization
cov.out = Bayes.unknown.ni$sigmas
row.names(cov.out) = port
colnames(cov.out) = port
b2.eff = make.port(Bayes.unknown.ni$mus, cov.out,.5,-.5)
b2.optimal.point = b2.eff[b2.eff$sharpe==max(b2.eff$sharpe),]

# Efficient Frontier and performance
p = eff.plot(b2.eff, b2.optimal.point); plot(p)
b2.perf = port.performance(b2.optimal.point)
Bayes.ni = port.summary(b2.perf)