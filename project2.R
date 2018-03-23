# You need to set your own directory
setwd("~/Desktop/optimization/hw/project/project2")

# read in the data
data = read.csv("N100StkPrices.csv", header = TRUE)


# clean up data
data = na.omit(data)
ticker = data$TICKER

# spun off MDLZ
delete = seq(1, dim(data)[1])[ticker == "MDLZ"]
data = data[-delete, ]

date = apply(as.matrix(data$date), MARGIN = 1, FUN = "toString")
date = as.Date(date, "%Y%m%d")
ticker = data$TICKER
price = data$PRC
shares = data$SHROUT


# Accounting for changes in ticker names



# KFT changed to KRFT in Oct 2012.
ticker[ticker == "KFT"] = "KRFT"

# SXCI changed to CTRX in Jul 2012.
ticker[ticker == "SXCI"] = "CTRX"

# HANS changed to MNST in Jan 2012.
ticker[ticker == "HANS"] = "MNST"




# convert prices to a matrix, arranged by rows of dates and columns of tickers
unique_dates = sort(unique((date)))
unique_tickers = sort(unique(ticker))

priceMat = matrix(NA, length(unique_dates), length(unique_tickers))
sharesMat = matrix(0, length(unique_dates), length(unique_tickers))

for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_dates, date[ticker == tic])
  
  priceMat[idx, i] = price[ticker == tic]
  sharesMat[idx, i] = shares[ticker == tic]
}

rownames(priceMat) = as.character(unique_dates)
rownames(sharesMat) = as.character(unique_dates)


(list = c("data", "delete", "i", "idx", "price", "shares", "tic", "ticker", "date"))



# Read Monthly Data -------------------------------------------------------

# read in the data
mdata = read.csv("N100Monthly.csv", header = TRUE, stringsAsFactors = FALSE)

# clean up data
mdate = apply(as.matrix(mdata$date), MARGIN = 1, FUN = "toString")
mdate = as.Date(mdate, "%Y%m%d")

mticker = mdata$TICKER
mprice = mdata$PRC
mshares = mdata$SHROUT
mticker[mticker == "FOXA"] = "NWSA"


unique_mdates = sort(unique((mdate)))
unique_mtickers = sort(unique(mticker))

idx = is.element(unique_mtickers, unique_tickers)

# if (!all(idx)) {
#   print("Warning: Some tickers seem to be missing")
# }

monthlyPriceMat = matrix(NA, length(unique_mdates), length(unique_tickers))

for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_mdates, mdate[mticker == tic])
  monthlyPriceMat[idx, i] = mprice[mticker == tic]
}


("mdata", "i", "idx", "mprice", "mshares", "mticker", "tic", "mdate")


# Starting Questions -------------------------------------------------------

# Q1
n = ncol(priceMat)
d = nrow(priceMat)
returnMat = matrix(NA, d, n) #d-1 by n matrix for daily returns
rownames(returnMat) = unique_dates
colnames(returnMat) = unique_tickers

for (i in 1:n){
  returnMat[2:d,i] = (priceMat[2:d,i] - priceMat[1:d-1,i])/priceMat[1:d-1,i]
}

View(returnMat)
# Q2
corMat = cor(returnMat, use = "pairwise.complete.obs") #use non-na values


# Q3
library(lpSolve)
constructFund <- function(rho, q, priceMat, sharesMat, unique_tickers, unique_dates){
  n = length(unique_tickers)
  d = length(unique_dates)
  c = c(as.vector(rho),rep(0,n))
  A = matrix(0, n^2+n+1, n^2+n)
  A[1,(n^2+1):(n^2+n)] = rep(1,n)
  for (i in 1:n){
    A[(i+1), (n*(i-1)+1):(n*i)] = rep(1,n)
  }
  A[(n+2):(n^2+n+1), 1:n^2] = diag(1,n^2)
  A[(n+2):(n^2+n+1), (n^2+1):(n^2+n)] = matrix(rep(diag(-1,n),n), nrow=n^2, byrow=T)
  b = c(q, rep(1,n), rep(0,n^2))
  dir = c(rep('=',(n+1)),rep('<=',n^2))
  s <- lp('max', c, A, dir, b, all.bin=TRUE)
  share_last = sharesMat[d,]
  price_last = priceMat[d,]
  mkt_cap = share_last*price_last
  stk_similar = matrix(0,n,n)
  for (i in 1:n){
    stk_similar[i,] = mkt_cap[i]*s$solution[(n*(i-1)+1):(n*i)]
  }
  weights = colSums(stk_similar)
  weights_adj = weights/sum(weights)
  return(weights_adj)
}


# Q4
q = 25
invest = 1000000
weights = constructFund(corMat,q,priceMat, sharesMat, unique_tickers, unique_dates)


weights_dollar = weights*invest

portfolio_share = weights_dollar/priceMat[d,]
nasdaq_share = invest/2660.93

value_nasdaq = nasdaq_share*c(2731.53, 2738.58, 2818.69, 2887.44, 2981.76, 2909.60, 3090.19, 3073.81, 3218.20, 3377.73, 3487.82, 3592)
value_portfolio = colSums(apply(monthlyPriceMat, 1, function(x) x*portfolio_share))

return_nasdaq = (value_nasdaq - invest)/invest
return_portfolio = (value_portfolio - invest)/invest

compare = data.frame(c(1:12), return_nasdaq, return_portfolio)
colnames(compare) = c('month', 'nasdaq', 'portfolio')


plot(compare$month, compare$portfolio, col = 'red', type = 'b', 
     xlab = 'month', ylab = 'return', xlim = c(0,12), ylim = c(0, 0.4))
lines(compare$month, compare$nasdaq, col = 'blue', type = 'b', lty=2)
legend("topleft",legend = c('portfolio','nasdaq'), col = c('red','blue'), lty = 1:2, cex = 0.8)


# Q5
#given the available info, maybe we could use mkt cap to measure similarity?
similarityMat <- function(priceMat, sharesMat, unique_tickers,unique_dates){
  mkt_cap = priceMat*sharesMat
  corMat = cor(mkt_cap, use = "pairwise.complete.obs")
  return(corMat)
}
rho = similarityMat(priceMat,sharesMat,unique_tickers, unique_dates)

weights2 = constructFund(rho,q,priceMat, sharesMat, unique_tickers, unique_dates)
weights_dollar2 = weights2*invest
portfolio_share2 = weights_dollar2/priceMat[d,]
value_portfolio2 = colSums(apply(monthlyPriceMat, 1, function(x) x*portfolio_share2))
return_portfolio2 = (value_portfolio2 - invest)/invest
compare$portfolio2 = return_portfolio2

plot(compare$month, compare$portfolio, col = 'red', type = 'b', pch = 16,
     xlab = 'month', ylab = 'return', xlim = c(1,12), ylim = c(0, 0.4),xaxt="n")
lines(compare$month, compare$portfolio2, col = 'dark green', type = 'b', lty=3, pch = 16)
lines(compare$month, compare$nasdaq, col = 'blue', type = 'b', lty=2, pch = 16)
ticks<-1:12
axis(1,at=ticks,labels=ticks)
legend("topleft",legend = c('portfolio1 (similar return)','portfolio2 (similar market cap)','nasdaq'), col = c('red','dark green','blue'), lty = c(1,3,2), pch = c(16,16,16),cex = 0.75)

View(compare)
to.csv