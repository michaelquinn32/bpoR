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

# Required libraries

library(stockPortfolio) # Stock data
library(ggplot2)        # Better plots
library(reshape2)       # For plots
library(quadprog)       # Used to solve Markowitz problem
library(MCMCpack)       # More distributions
library(mvtnorm)        # Multivariate Normal Distribution
library(tseries)
library(zoo)            # Required by tseries
library(xtable)         # For table output
library(plyr)           # For the function "each"
library(dplyr)          # For data manipulation
library(magrittr)       # Pipes!

source('code/final-data.R')
source('code/final-opt.R')
source('code/final-describe.R')
source('code/final-naive.R')
source('code/final-foresight.R')
source('code/final-bayes-uk-mean.R')
source('code/final-bayes-ni.R')
source('code/final-bayes-ip.R')
source('code/final-compare.R')
source('code/final-validate.R')
source('code/final-val-plot.R')
