#       File: final-key.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   This program executes all operations for the script.
#                 
#                   Be sure to set the appropriate working directory
##########################################

##########################################
# Set the appropriate working directory !!!!
##########################################

setwd('~/Git/Bayesian_Portfolio_Paper/Code')

# Required libraries

library(stockPortfolio) # Stock data
library(ggplot2)        # Better plots
library(reshape2)       # For plots
library(quadprog)       # Used to solve Markowitz problem
library(MCMCpack)       # More distributions
library(mvtnorm)        # Multivariate Normal Distribution
library(tseries)
library(zoo)            # Required by tseries
library(xtable)

source('final-data.R')
source('final-opt.R')
source('final-describe.R')
source('final-naive.R')
source('final-foresight.R')
source('final-bayes-uk-mean.R')
source('final-bayes-ni.R')
source('final-bayes-ip.R')
source('final-compare.R')
source('final-validate.R')
source('final-val-plot.R')
