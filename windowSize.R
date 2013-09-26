
# Set working directory ---------------------------------------------------

LOCAL_PATH <- "C:\\bshen\\BenqingShenDev\\R\\garchlab"
setwd(LOCAL_PATH)

# Require libraries -------------------------------------------------------

library("quantmod")
library("rugarch")

# Load Historical data ----------------------------------------------------

getSymbols(Symbols="SPX", from="1990-01-01", to="2013-09-20")
spx.level <- SPX[,6]
spx.return <- dailyReturn(x=spx.level, type='log')
# plot(spx.return)

# Window size -------------------------------------------------------------

window.size <- 250 * 5

# Model Spec --------------------------------------------------------------

spec <- ugarchspec(
  mean.model=list(armaOrder = c(0,0), include.mean = FALSE))

# Single day example ------------------------------------------------------

i <- 2
data <- spx.return[i : (i+window.size-1), ]
fit <- ugarchfit(spec=spec, data=data)
info.crit <- infocriteria(fit)
akaike <- info.crit[1, 1]
ll <- likelihood(fit)
std.ll <- ll / window.size

# Rolling window calibration ----------------------------------------------

coef.df <- data.frame()
likelihoods <- vector()
for (i in 2 : (length(spx.return)-window.size+1))
{
data <- spx.return[i : (i+window.size-1), ]
fit <- ugarchfit(spec=spec, data=data)
coef.df <- rbind(coef.df, coef(fit))
likelihoods <- c(likelihoods, likelihood(fit))
}

# Scenario analysis -------------------------------------------------------

i <- length(spx.return) - 11 - window.size
data0 <- spx.return[i : (i+window.size-1), ]
fit <- ugarchfit(spec=spec, data=data0)
beta.previous <- coef(fit)["beta1"]

i <- (i+1)
data1 <- spx.return[i : (i+window.size-1), ]
fit <- ugarchfit(spec=spec, data=data1)
beta.current <- coef(fit)["beta1"]

i <- (i+1)
data2 <- spx.return[i : (i+window.size-1), ]
fit <- ugarchfit(spec=spec, data=data2)
beta.latter <- coef(fit)["beta1"]

beta.change.one <- (beta.current - beta.previous) / beta.previous
beta.change.two <- (beta.latter - beta.current) / beta.current


i <- length(spx.return) - 11 - window.size
data1.extend <- spx.return[i : (i+window.size), ]
fit <- ugarchfit(spec=spec, data=data1.extend)
beta.current.extend <- coef(fit)["beta1"]
beta.change.one.extend <- (beta.current.extend - beta.previous) / beta.previous

# Scenario analysis 2 -----------------------------------------------------

end.date <- "2008-09-18"
end.date.date <- as.Date(x=end.date)
# start.date.date <- end.date.date - 250 + 1
# data.date.range <- as.Date(start.date.date:end.date.date)
# spx.return[end.date.date]
i <- length(spx.return) - 11 - window.size * 2
data3 <- spx.return[i : (i+window.size-1), ]
fit <- ugarchfit(spec=spec, data=data3)
beta.previous3 <- coef(fit)["beta1"]

i <- (i+1)
data4 <- spx.return[i : (i+window.size-1), ]
fit <- ugarchfit(spec=spec, data=data4)
beta.current4 <- coef(fit)["beta1"]

i <- (i+1)
data5 <- spx.return[i : (i+window.size-1), ]
fit <- ugarchfit(spec=spec, data=data5)
beta.latter5 <- coef(fit)["beta1"]

beta.change.three <- (beta.current4 - beta.previous3) / beta.previous3
beta.change.four <- (beta.latter5 - beta.current4) / beta.current4

i <- length(spx.return) - 11 - window.size * 2
data4.extend <- spx.return[i : (i+window.size * 1.5), ]
fit <- ugarchfit(spec=spec, data=data4.extend)
beta.current4.extend <- coef(fit)["beta1"]
beta.change.three.extend <- (beta.current4.extend - beta.previous3) / beta.previous3

# Expanding window calibration --------------------------------------------
coef.df <- data.frame()
likelihoods <- vector()
for (i in 2 : (length(spx.return)-window.size+1))
{
  data <- spx.return[2 : (i+window.size-1), ]
  fit <- ugarchfit(spec=spec, data=data)
  coef.df <- rbind(coef.df, coef(fit))
  likelihoods <- c(likelihoods, likelihood(fit))
}
beta.appending.window <- coef.df[,3]
plot(1:2201, beta.appending.window, type='l')
beta.appending.changes <- (beta.appending.window[2:2201] - beta.appending.window[1:2200])/beta.appending.window[1:2200]

# Stick with fixed size rolling window ------------------------------------

# Which size is the best --------------------------------------------------

dataFullLen <- length(spx.return)

fit.aic <- vector()
fit.beta <- vector()
ll <- vector()
std.ll <- vector()

for (j in 1 : 20)
{
  window.size <- 250 * j
  data <- spx.return[(dataFullLen - window.size + 1) : dataFullLen]
  fit <- ugarchfit(spec=spec, data=data)
  info.crit <- infocriteria(fit)
  fit.aic <- c(fit.aic, info.crit[1,1])
  fit.beta <- c(fit.beta, coef(fit)["beta1"])
  ll <- c(ll, likelihood(fit))
  std.ll <- c(std.ll, likelihood(fit)/window.size)
}

# Which size is the best 2 ------------------------------------------------
dataFullLen <- length(spx.return)
i.seq <- seq(from=1, to=252, by=1)
# 252 scenarios

fit.aic <- matrix(nrow=252, ncol=20, data=0)
fit.beta <- matrix(nrow=252, ncol=20, data=0)
ll <- matrix(nrow=252, ncol=20, data=0)
std.ll <- matrix(nrow=252, ncol=20, data=0)

for (i in i.seq)
{
  return.data <- spx.return[1 : (dataFullLen-i)]
  for (j in 1 : 20)
  {
    dataLen <- length(return.data)
    window.size <- 250 * j
    data <- return.data[(dataLen - window.size + 1) : dataLen]
    fit <- ugarchfit(spec=spec, data=data)
    info.crit <- infocriteria(fit)
    fit.aic[i, j] <- info.crit[1,1]
    fit.beta[i, j] <- coef(fit)["beta1"]
    ll[i, j] <- likelihood(fit)
    std.ll[i, j] <- likelihood(fit)/window.size
  }
}
