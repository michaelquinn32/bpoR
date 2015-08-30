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
make.port <- function(means, 
                      covariance, 
                      max.allocation = .5,
                      min.allocation = 0,
                      risk.premium.up = .5, 
                      risk.increment=.005) {
# A function to optimize a portfolio, using quadratic programming
# Inputs:
#       means           a vector of stock means
#       covariance      the stocks' covariance matrix
#       max.allocation  a parameter that controls the maximum weight in the final portfolio
#       min.allocation  a parameter that controls the minimum weight in the final portfolio 
#       risk.premium.up the maximum risk taken
#       risk.increment  the step size
# Outputs:
#   The stock portfolio's efficent frontier 
    
    # Get the number of stocks
    n <- ncol(covariance)
  
    # Matrices to solve with quadprog, with max and min constraints
    Amat <- cbind(1, diag(n), -diag(n))
    bvec <- c(1, rep(min.allocation, n), rep(-max.allocation, n))
    meq <- 1
  
    # Set up the iterator
    iters <- seq(from=0, to=risk.premium.up, by=risk.increment) %>% setNames(nm = .)
    dvec <- lapply(iters, function(i) means * i)
    
    # Get the solutions
    sols <- lapply(dvec, solve.QP, Dmat = covariance, Amat = Amat, bvec = bvec, meq = meq)
    
    # Summarize and return the results
    ######################################
    # Generate a list of summary functions
    funs <- list("Std.Dev" = function(x) sqrt(x %*%  covariance %*% x),
                 "Exp.Return" = function(x) x %*% means,
                 "sharpe" = function(x) x %*% means / 
                     sqrt(x %*%  covariance %*% x))
    
    # Apply to the solutions list
    summ <- lapply(funs, function(f) lapply(sols, 
                    function(s) f(s$solution))) %>% 
        lapply(as.numeric) %>%
        do.call(what = "cbind")
    
    # Combine into a results data frame
    results <- lapply(sols, function(s) s$solution) %>% 
        do.call(what = "rbind") %>% 
        data.frame(summ)
    
    return(results)
}

# Plot port and efficient Frontier
eff.plot <- function(eff, eff.optimal.point) {
  p <- ggplot(eff, aes(x=Std.Dev, y=Exp.Return)) + 
      geom_line(col="Red") +
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
    labs(x = "Risk (standard deviation of portfolio)", y = "Return")
    return(p)
}

baseline_cs <- function(x) {
# Takes a cumulative sum, with the beginning indexed at 1
    x[1] <- x[1] + 1
    cumsum(x)
}

# Portfolio performance
port.performance <- function(eff.optimal.point, test_port = port.test) {
  n <- dim(eff.optimal.point)[2] - 3
  weights <- eff.optimal.point[1,1:n] %>% as.numeric()
  tret <- test_port %>% extract2("R") %>% `%*%`(weights) %>% rev() %>% baseline_cs
  tlab <- sort(as.Date(row.names(port.test$R)),decreasing=FALSE)
  return(list(tret = tret, tlab = tlab, weights = weights))
}

# Quick summary
port.summary <- function(performance, test_port = port.test) {
    c(52 * mean(test_port$R %*% performance$weights), 
      sqrt(52) * sd(test_port$R %*%  performance$weights),
      52 * mean(test_port$R %*%  performance$weights)/ 
      (sqrt(52) * sd(test_port$R%*% performance$weights)))
}
