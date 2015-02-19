#       File: final-opt.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   This program contains functions for generating the optimal portfolio and plotting 
#                   the efficient frontier. 
##########################################


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
  p = ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + geom_line(col="Red") +
    geom_point(data=eff.optimal.point, aes(x=Std.Dev, y=Exp.Return, label=sharpe),
               size=5) +
    annotate(geom="text", x=eff.optimal.point$Std.Dev,
             y=eff.optimal.point$Exp.Return,
             label=paste("Risk: ",
                         round(eff.optimal.point$Std.Dev*100, digits=3),"\nReturn: ",
                         round(eff.optimal.point$Exp.Return*100, digits=4),"%\nSharpe: ",
                         round(eff.optimal.point$sharpe*100, digits=2), "%", sep=""),
             hjust=0, vjust=1.2) +
    ggtitle("Efficient Frontier and Optimal Portfolio") +
    labs(x="Risk (standard deviation of portfolio)", y="Return")
    return(p)
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
