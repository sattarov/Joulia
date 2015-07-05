library(tseries)
library(copula)


#load data from yahoo.finance
daimler_prices <- get.hist.quote("DAI.DE", quote="Adj", start="2009-12-25", retclass="zoo")
adidas_prices <- get.hist.quote("ADS.DE", quote="Adj", start="2012-12-25", retclass="zoo")
bmw_prices <- get.hist.quote("BMW.DE", quote="Adj", start="2012-12-25", retclass="zoo")

length(daimler_prices)
#transform data into log-returns
log_daimler <- diff(log(daimler_prices))
log_adidas <- diff(log(adidas_prices))
log_bmw <- diff(log(bmw_prices))


#transform data into pseudo-observations, values lie in [0,1]
daimler_pobs <- pobs(log_daimler)
adidas_pobs <- pobs(log_adidas)
bmw_pobs <- pobs(log_bmw)

