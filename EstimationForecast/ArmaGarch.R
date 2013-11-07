# ARMA(1, 1)-GARCH(1, 1)
# Estimation and forecast using rugarch
# by Jesper Hyberl Pedersen (2013)

library ("rugarch")

model <- ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model=list(armaOrder = c(1, 1), include.mean = TRUE),
                    distribution.model="norm")

model2 <- ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(2, 2)),
                     mean.model=list(armaOrder = c(2, 2), include.mean = TRUE),
                     distribution.model="sstd")

# model2@model$pars

model.fixed <- ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1, 1)),
                          mean.model=list(armaOrder = c(1, 1), include.mean = TRUE),
                          distribution.model="norm",
                          fixed.pars=list(beta1=0.86))

# model.fixed@model$pars

data(sp500ret)

# library("tseries")
# sp500.prices <- get.hist.quote(instrument="^GSPC", quote="AdjClose",
#                                origin="1899-12-30", compression="d",
#                                retclass="zoo", quiet=FALSE, drop=FALSE)
# 
# sp500 <-as.data.frame(sp500.prices)
# N <- length(sp500[, 1])
# sp500.returns <- 100 * (log(sp500[2:N,]) - log(sp500[1:(N-1),]))

modelfit <- ugarchfit(spec=model, data=sp500ret)

# typeof(modelfit)
# class(modelfit)
# str(modelfit)
# slotNames(x=modelfit)
# modelfit@fit$hessian
# modelfit@fit$coef
# modelfit@fit$fitted.values

# coef(modelfit)
# infocriteria(modelfit)
# sigma(modelfit)
# fitted(modelfit)
# residuals(modelfit)

VaR <- quantile(x=modelfit, probs=0.01)
VaR.alt <- modelfit@fit$sigma * qnorm(p=0.01) + modelfit@fit$fitted.values

modelfor <- ugarchforecast(fitORspec=modelfit, data=NULL, n.ahead=10, n.roll=0, out.sample=0)
# slotNames(modelfor)
# plot(modelfor)
modelfor.50 <- ugarchforecast(fitORspec=modelfit, data=NULL, n.ahead=50, n.roll=0, out.sample=0)
# plot(modelfor.50@forecast$seriesFor)
# uncmean(modelfit)

model00 <- ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model=list(armaOrder = c(0, 0)),
                    distribution.model="norm")
modelfit00 <- ugarchfit(spec=model00, data=sp500ret, out.sample=2)

# ?? modelfit00@model$modeldata$T

modelfor00 <- ugarchforecast(fitORspec=modelfit00, data=NULL, n.ahead=1, n.roll=2, out.sample=2)

# sigma(modelfor00)
# fitted(modelfor00)

mu <- coef(modelfit00)["mu"]
lastTwoReturns <- sp500ret[5522:5523,]
e5521 <- as.vector(residuals(modelfit00)[5521])
e5522 <- lastTwoReturns[1] - mu
e5523 <- lastTwoReturns[2] - mu

coeffs <- coef(modelfit00)
.fgarch <- function(e, sigma0, coeffs) {
  omega <- coeffs["omega"]
  alpha <- coeffs["alpha1"]
  beta <- coeffs["beta1"]
  sigma1 <- sqrt(omega + alpha * e^2 + beta * sigma0^2)
  names(sigma1) <- NULL
  return(sigma1)
}

sigma5521 <- as.vector(sigma(modelfit00)[5521])

sigma5522 <- .fgarch(e5521, sigma0=sigma5521, coeffs=coeffs)
sigma5522
sigma(modelfor00)[1]

sigma5523 <- .fgarch(e5522, sigma0=sigma5522, coeffs=coeffs)
sigma5523
sigma(modelfor00)[2]

sigma5524 <- .fgarch(e5523, sigma0=sigma5523, coeffs=coeffs)
sigma5524
sigma(modelfor00)[3]

# quantile(x=modelfor00, 0.05)


# 6. Rolling forecast with reestimation -----------------------------------

# model <- ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1, 1)),
#                     mean.model=list(armaOrder = c(1, 1)),
#                     distribution.model="norm")

modelroll <- ugarchroll(spec=model, data=sp500ret, n.ahead=1,
                        forecast.length=100, n.start=NULL,
                        refit.every=50, refit.window=c("recersive"),
                        calculate.VaR=TRUE, VaR.alpha=c(0.01, 0.05),
                        keep.coef=TRUE)

# str(modelroll@forecast)

VaR.roll <- modelroll@forecast$VaR[, "alpha(1%)"]
return.roll <- modelroll@forecast$VaR[, "realized"]
Hit <- return.roll < VaR.roll
# sum(Hit)

q_st <- qnorm(0.025)
sigma <- modelroll@forecast$density[, "Sigma"]
mu <- modelroll@forecast$density[, "Mu"]
Var.0025 <- q_st * sigma + mu

model.sstd <- ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model=list(armaOrder = c(1, 1)),
                    distribution.model="sstd")

modelroll.sstd <- ugarchroll(spec=model, data=sp500ret, n.ahead=1,
                        forecast.length=100, n.start=NULL,
                        refit.every=50, refit.window=c("recersive"),
                        calculate.VaR=TRUE, VaR.alpha=c(0.01, 0.05),
                        keep.coef=TRUE)

skew.estimate <- modelroll.sstd@forecast$density[, "Skew"]

