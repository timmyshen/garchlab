require(rugarch)
require(ggplot2)
require(xts)

data(sp500ret)

model <- ugarchspec(variance.model=list(model = "eGARCH", garchOrder = c(1, 1)),
                    mean.model=list(armaOrder = c(0, 0), include.mean = FALSE),
                    distribution.model="norm")

modelroll <- ugarchroll(spec=model, data=sp500ret, n.ahead=1,
                        forecast.length=500, n.start=NULL,
                        refit.every=1, refit.window=c("moving"),window.size=2500,
                        calculate.VaR=FALSE, VaR.alpha=c(0.01, 0.05),
                        keep.coef=TRUE)

modelroll@forecast$density -> d
head(d)

dates <- as.Date(rownames(d))
sigma.zoo <- zoo(x=d$Sigma, order.by=dates)

sigma.xts <- xts(x=d$Sigma, order.by=dates)
real.xts <- xts(x=d$Realized, order.by=dates)

sigma2.xts <- xts(x=d$Sigma^2, order.by=dates)
real2.xts <- xts(x=d$Realized^2, order.by=dates)

sigma2.annual.xts <- xts(x=d$Sigma^2*250, order.by=dates)
sigma.annual.xts <- xts(x=d$Sigma*sqrt(250), order.by=dates)

# coef(modelroll) -> c

sumLogLikelihood <- 0.0
for (i in 1:500) {
  loglikelihood <- dnorm(x=d$Realized[i], mean=0, sd=d$Sigma[i], log=TRUE)
  sumLogLikelihood <- sumLogLikelihood + loglikelihood
}