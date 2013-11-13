# http://quanttrader.info/public/testForCoint.html

require(zoo)
require(tseries)

# gld <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=GLD&ignore=.csv", stringsAsFactors=F)
# gdx <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=GDX&ignore=.csv", stringsAsFactors=F)
gld <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=GLD&d=07&e=10&f=2009&ignore=.csv", stringsAsFactors=F)
gdx <- read.csv("http://ichart.finance.yahoo.com/table.csv?s=GDX&d=07&e=10&f=2009&ignore=.csv", stringsAsFactors=F)

# The first column contains dates.  The as.Date
# function can convert strings into Date objects.
gld_dates <- as.Date(gld[,1])
gdx_dates <- as.Date(gdx[,1])

# The seventh column contains the adjusted close.
# We use the zoo function to create zoo objects from that data.
# The function takes two arguments: a vector of data and
# a vector of dates.
gld <- zoo(gld[,7], gld_dates)
gdx <- zoo(gdx[,7], gdx_dates)

# The merge function can combine two zoo objects,
# computing either their intersection (all=FALSE)
# or union (all=TRUE).
t.zoo <- merge(gld, gdx, all=FALSE)

merged_dates <- index(t.zoo)

# At this point, t.zoo is a zoo object with two columns: gld and gdx.
# Most statistical functions expect a data frame for input,
# so we create a data frame here.
t <- as.data.frame(t.zoo)

# Tell the user what dates are spanned by the data.
#
cat("Date range is", format(start(t.zoo)), "to", format(end(t.zoo)), "\n")

# The lm function builds linear regression models using OLS.
# We build the linear model, m, forcing a zero intercept,
# then we extract the model's first regression coefficient.
#
m <- lm(gld ~ gdx + 0, data=t)
beta <- coef(m)[1]

cat("Assumed hedge ratio is", beta, "\n")

# Now compute the spread
#
sprd <- t$gld - beta*t$gdx

sprd.zoo <- zoo(sprd, order.by=merged_dates)

# Setting alternative="stationary" chooses the appropriate test.
# Setting k=0 forces a basic (not augmented) test.  See the
# documentation for its full meaning.
#
ht <- adf.test(sprd, alternative="stationary", k=0)
cat("ADF p-value is", ht$p.value, "\n")

# The ht object contains the p-value from the ADF test.
# The p-value is the probability that the spread is NOT
# mean-reverting.  Hence, a small p-value means it is very
# improbable that the spread is NOT mean-reverting
# (got that?).
#
if (ht$p.value < 0.05) {
  cat("The spread is likely mean-reverting.\n")
} else {
  cat("The spread is not mean-reverting.\n")
}

