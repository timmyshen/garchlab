# forecast and log likelihood

# Set working directory ---------------------------------------------------

LOCAL_PATH <- "C:\\bshen\\BenqingShenDev\\R\\garchlab"
setwd(LOCAL_PATH)

# Require libraries -------------------------------------------------------

library("quantmod")
library("rugarch")

# Load Historical data ----------------------------------------------------

getSymbols(Symbols="SPX", from="1990-01-02", to="2013-09-27")
spx.level <- SPX[,6]
spx.return <- dailyReturn(x=spx.level)[2: length(spx.level), ]

# Model Spec --------------------------------------------------------------

spec <- ugarchspec(
  mean.model=list(armaOrder = c(0,0), include.mean = FALSE))

# Single day example ------------------------------------------------------
window.size <- 250 * 5
i <- 3458
data <- spx.return[i : (i+window.size-1), ]
forcPeriodData <- spx.return[(i + window.size) : (i + window.size + 20 - 1), ]

fit <- ugarchfit(spec=spec, data=data, )

forc <- ugarchforecast(fitORspec=fit, n.ahead=20)
forc.sigma <- sigma(forc)

loglikelihood <- 0.0
for(i in 1:20) {
  return.point <- as.numeric(forcPeriodData[i])
  loglikelihood <- loglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[i], log=TRUE)
}

# Single day example, bigger window ------------------------------------------------------
window.size <- 250 * 10
i <- 3458
data <- spx.return[i : (i+window.size-1), ]
forcPeriodData <- spx.return[(i + window.size) : (i + window.size + 20 - 1), ]

fit <- ugarchfit(spec=spec, data=data, )

forc <- ugarchforecast(fitORspec=fit, n.ahead=20)
forc.sigma <- sigma(forc)

loglikelihood <- 0.0
for(i in 1:20) {
  return.point <- as.numeric(forcPeriodData[i])
  loglikelihood <- loglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[i], log=TRUE)
}

# Single day example, bigger window2 ------------------------------------------------------
window.size <- 250 * 10
i <- 2210
data <- spx.return[i : (i+window.size-1), ]
forcPeriodData <- spx.return[(i + window.size) : (i + window.size + 20 - 1), ]

fit <- ugarchfit(spec=spec, data=data, )

forc <- ugarchforecast(fitORspec=fit, n.ahead=20)
forc.sigma <- sigma(forc)

loglikelihood <- 0.0
for(i in 1:20) {
  return.point <- as.numeric(forcPeriodData[i])
  loglikelihood <- loglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[i], log=TRUE)
}


# Rolling window calibration ----------------------------------------------
window.size <- 250 * 5

likelihoods <- vector()
forcloglikelihoods <- vector()

for (i in 1 : (length(spx.return) - window.size - 20))
{
  data <- spx.return[i : (i+window.size-1), ]
  fit <- ugarchfit(spec=spec, data=data)
  
  if(fit@fit$convergence != 0) {
    next
  }
  
  forcPeriodData <- spx.return[(i + window.size) : (i + window.size + 20 - 1), ]
  forc <- ugarchforecast(fitORspec=fit, n.ahead=20)
  forc.sigma <- sigma(forc)
  
  forcloglikelihood <- 0.0
  for(i in 1:20) {
    return.point <- as.numeric(forcPeriodData[i])
    forcloglikelihood <- forcloglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[i], log=TRUE)
  }
  
  forcloglikelihoods <- c(forcloglikelihoods, forcloglikelihood)
  likelihoods <- c(likelihoods, likelihood(fit))
}


# Rolling window calibration, bigger window ----------------------------------------------

window.size <- 250 * 10
likelihoods <- vector()
forcloglikelihoods <- vector()

for (i in 1 : (length(spx.return) - window.size - 20))
{
  data <- spx.return[i : (i+window.size-1), ]
  fit <- ugarchfit(spec=spec, data=data)
  
  if(fit@fit$convergence != 0) {
    next
  }
  
  forcPeriodData <- spx.return[(i + window.size) : (i + window.size + 20 - 1), ]
  forc <- ugarchforecast(fitORspec=fit, n.ahead=20)
  forc.sigma <- sigma(forc)
  
  forcloglikelihood <- 0.0
  for(i in 1:20) {
    return.point <- as.numeric(forcPeriodData[i])
    forcloglikelihood <- forcloglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[i], log=TRUE)
  }
  
  forcloglikelihoods <- c(forcloglikelihoods, forcloglikelihood)
  likelihoods <- c(likelihoods, likelihood(fit))
}
  
# Rolling window calibration, bigger window, predicting daily ----------------------------------------------

window.size <- 250 * 10
likelihoods <- vector()
forcloglikelihoods <- vector()

for (i in 1 : (length(spx.return) - window.size - 20))
{
  data <- spx.return[i : (i+window.size-1), ]
  fit <- ugarchfit(spec=spec, data=data)
  
  if(fit@fit$convergence != 0) {
    next
  }
  
  forcPeriodData <- spx.return[(i + window.size), ]
  forc <- ugarchforecast(fitORspec=fit, n.ahead=1)
  forc.sigma <- sigma(forc)
  
  forcloglikelihood <- 0.0
  
  return.point <- as.numeric(forcPeriodData[1])
  forcloglikelihood <- forcloglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[1], log=TRUE)
  
  
  forcloglikelihoods <- c(forcloglikelihoods, forcloglikelihood)
  likelihoods <- c(likelihoods, likelihood(fit))
}

# Rolling window calibration, small window, predicting daily ----------------------------------------------

window.size <- 250 * 5
likelihoods <- vector()
forcloglikelihoods <- vector()

for (i in 1 : (length(spx.return) - window.size - 20))
{
  data <- spx.return[i : (i+window.size-1), ]
  fit <- ugarchfit(spec=spec, data=data)
  
  if(fit@fit$convergence != 0) {
    next
  }
  
  forcPeriodData <- spx.return[(i + window.size), ]
  forc <- ugarchforecast(fitORspec=fit, n.ahead=1)
  forc.sigma <- sigma(forc)
  
  forcloglikelihood <- 0.0
  
  return.point <- as.numeric(forcPeriodData[1])
  forcloglikelihood <- forcloglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[1], log=TRUE)
  
  
  forcloglikelihoods <- c(forcloglikelihoods, forcloglikelihood)
  likelihoods <- c(likelihoods, likelihood(fit))
}

# Rolling window calibration, large window, predicting daily ----------------------------------------------

getSymbols(Symbols="SPX", from="1980-01-02", to="2013-09-27")
spx.level <- SPX[,6]
spx.return <- dailyReturn(x=spx.level)[2: length(spx.level), ]

window.size <- 250 * 20
likelihoods <- vector()
forcloglikelihoods <- vector()

for (i in 1 : (length(spx.return) - window.size - 20))
{
  data <- spx.return[i : (i+window.size-1), ]
  fit <- ugarchfit(spec=spec, data=data)
  
  if(fit@fit$convergence != 0) {
    next
  }
  
  forcPeriodData <- spx.return[(i + window.size), ]
  forc <- ugarchforecast(fitORspec=fit, n.ahead=1)
  forc.sigma <- sigma(forc)
  
  forcloglikelihood <- 0.0
  
  return.point <- as.numeric(forcPeriodData[1])
  forcloglikelihood <- forcloglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[1], log=TRUE)
  
  
  forcloglikelihoods <- c(forcloglikelihoods, forcloglikelihood)
  likelihoods <- c(likelihoods, likelihood(fit))
}

# Rolling window calibration, 15y window, predicting daily ----------------------------------------------

window.size <- 250 * 15
likelihoods <- vector()
forcloglikelihoods <- vector()

for (i in 1 : (length(spx.return) - window.size - 20))
{
  data <- spx.return[i : (i+window.size-1), ]
  fit <- ugarchfit(spec=spec, data=data)
  
  if(fit@fit$convergence != 0) {
    next
  }
  
  forcPeriodData <- spx.return[(i + window.size), ]
  forc <- ugarchforecast(fitORspec=fit, n.ahead=1)
  forc.sigma <- sigma(forc)
  
  forcloglikelihood <- 0.0
  
  return.point <- as.numeric(forcPeriodData[1])
  forcloglikelihood <- forcloglikelihood + dnorm(x=return.point, mean=0, sd=forc.sigma[1], log=TRUE)
  
  
  forcloglikelihoods <- c(forcloglikelihoods, forcloglikelihood)
  likelihoods <- c(likelihoods, likelihood(fit))
}
