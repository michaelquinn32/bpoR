#       File: final-validate.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   Validating results from the previous experiment with 
#                   simulated stock data. 
##########################################

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

# Validation results
validate.table = cbind(Return = sim.r.means, Risk = sd.r, "P > Market " = c(probs,NA))
