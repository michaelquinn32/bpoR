#       File: final-describe.R
#       Author: Michael Quinn
#       Date Created: February 4, 2014
#       
#       Summary:
#                   This is the accompanying code for a paper submitted to the 
#                    Central Asia Business Journal.
#                   It is a self-contained document and downloads all needed data from the internet
#                   See abstract for more information
#
#                   This program generates descriptive statistics for the stock data
##########################################

# Descriptive Stats last six months and test portfolio
end <- 1
start <- end + 23
funs <- list(Means = mean, StDev = sd, Total = sum, N = length)

ss <- port.hist %>% 
    extract2("R") %>% 
    extract(start:end, ) 

c_summ_hist <- lapply(X = funs, FUN = function(f) apply(ss, 2, f)) %>% attach()
c_summ_new <- lapply(X = funs, FUN = function(f) apply(port.test$R, 2, f)) 
Cov <- cov(ss)
Cov.New <- cov(port.test$R)

# Plot of stock returns
##############################
# Get the market returns
market <- baseline.hist %>% extract2("R") %>% extract(start:end,) %>% c(1,.) %>% cumsum()

# Get the quote dates
hist.dates <- port.hist %>% extract2("R") %>% rownames() %>% extract((start + 1) : end) %>%
    as.Date()

# Get the portfolio returs
return.out <- port.hist %>% extract2("R") %>% 
    extract(start:end,) %>% rbind(1, .) %>% apply(2, cumsum)

# Put together for plotting
hist.plot <- data.frame(SP500 = market, return.out, Date=hist.dates) %>% melt(id = "Date")

# Make Plot
p.returns <- ggplot(hist.plot,aes(x=Date,y=value)) +
    geom_line(aes(group=variable,colour=variable)) + 
    ggtitle("Cumulative Returns, Previous 3 Months") +
    labs(y="Cumulative Return") + 
    scale_colour_discrete(name="Ticker Name") +
    theme(legend.position="bottom") + 
    guides(col = guide_legend(nrow = 2)) 

print(p.returns)

# Basic Stats, historical
stocks.table <- data.frame("Annualized Return" =  52 * c_summ_hist$Means * 100, 
    "Annualized St. Dev." = sqrt(52) * c_summ_hist$StDev * 100, 
    "Sharpe Ratio" = 52 * c_summ_hist$Means /(sqrt(52)* c_summ_hist$StDev),
    check.names = FALSE) %>% 
    t()

# Basic Stats, Test port
# Looking forward to get the means
test.table = data.frame("Annualized Return" =  52 * c_summ_new$Means * 100, 
    "Annualized St. Dev." = sqrt(52) * c_summ_new$StDev * 100,
    "Sharpe Ratio" = 52 * c_summ_new$Means /(sqrt(52)* c_summ_new$StDev),
    check.names = FALSE) %>% 
    t()
