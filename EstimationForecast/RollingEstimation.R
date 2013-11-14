# Rolling Estimation Example

library(rugarch)
library(parallel)

data(sp500ret)

cl <- makePSOCKcluster(10)

spec <- ugarchspec(variance.model=list(model = 'eGARCH'), distribution.model='jsu')

roll <- ugarchroll(spec=spec, data=sp500ret, n.start=1000, 
                   refit.every=100, refit.window='moving', solver='hybrid', 
                   calculate.VaR=TRUE, VaR.alpha=c(0.01, 0.05), 
                   cluster=cl, keep.coef=TRUE)

show(roll)

stopCluster(cl)

report(roll, type = 'VaR', VaR.alpha = 0.05, conf.level = 0.95)

# plot(roll)