require(rugarch)

data(sp500ret)

model <- ugarchspec(variance.model=list(model = "sGARCH", garchOrder = c(1, 1)),
                    mean.model=list(armaOrder = c(1, 1)),
                    distribution.model="norm")

modelroll <- ugarchroll(spec=model, data=sp500ret, n.ahead=1,
                        forecast.length=100, n.start=NULL,
                        refit.every=50, refit.window=c("recursive"),
                        calculate.VaR=TRUE, VaR.alpha=c(0.01, 0.05),
                        keep.coef=TRUE)
