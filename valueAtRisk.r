library(PerformanceAnalytics)
library(TTR)

yahoo.read <- function(url) {
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,5)]
  df$Date <- as.Date(as.character(df$Date))
  result=xts(df[,2],df[,1])
  return(result)
}
ticker=c("AAPL","IBM","GOOG","BP","XOM","COST","GS")
weight=c(0.15,0.2,0.2,0.15,0.1,0.15,0.05);sum(weight)
urls <- paste("http://real-chart.finance.yahoo.com/table.csv?s=",ticker,"&a=07&b=24&c=2010&d=07&e=24&f=2017&g=d&ignore=.csv",sep="")

pxData=list()
for(i in 1:length(urls)) pxData[[ticker[i]]]=yahoo.read(urls[i])
pxData=do.call(merge,pxData)
dReturnStock=ROC(pxData,type = "discrete")
PortfoluioValue=xts(rowSums(dReturnStock*matrix(weight,nrow=nrow(dReturnStock),ncol=ncol(dReturnStock),byrow = TRUE)),index(dReturnStock))

#i. Historical VaR??
var=VaR(dReturnStock["2016-01-01::2016-12-31",],p = 0.95,weights = weight,method = "historical",portfolio_method = "component")
cvar=ES(dReturnStock["2016-01-01::2016-12-31",],p = 0.95,weights = weight,method = "historical",portfolio_method = "component")
#ii Expected Mean, Cov, Parameteric VaR??
var=VaR(dReturnStock["2016-01-01::2016-12-31",],p = 0.95,weights = weight,method = "modified",portfolio_method = "component")
cvar=ES(dReturnStock["2016-01-01::2016-12-31",],p = 0.95,weights = weight,method = "modified",portfolio_method = "component")

#iii Optimal Portfolio https://www.r-bloggers.com/portfolio-optimization-using-r-and-plotly/
library(PortfolioAnalytics)
library(quantmod)
library(zoo)
library(plotly)
port <- portfolio.spec(assets = ticker)
port <- add.constraint(port, type = "weight_sum", min_sum=0.5, max_sum = 1)
port <- add.constraint(port, type = "box", min=0, max = 0.5)
rportfolios <- random_portfolios(port, permutations = 5000, rp_method = "sample")
# Get minimum variance portfolio
minvar.port <- add.objective(port, type = "risk", name = "var")
# Optimize - calculate optimzation
rebalanceDate=as.POSIXct(c(paste("2016",c(2:12),"01",sep="-"),"2017-01-01"))-86400
minvar.opt <- lapply(rebalanceDate,function(v) optimize.portfolio(dReturnStock[paste(as.character(v-60*86400),"::",as.character(v),sep="")], minvar.port, optimize_method = "random",rp = rportfolios))
