library(fImport)
library(nlme)
library(Hmisc)
library(tseries)
library(nortest)

# Let's get the values of the indexes
# WARNING---- ---- --->>>>> PUT THE VECTOR UPSIDE DOWN --- in this case it doesn't matter as we are
# using correlations.
# We get CEMEXCPO.MX and ICA.MX

cemex <- yahooSeries("CEMEXCPO.MX", from="2003-01-01", to="2012-09-07", frequency="daily")
names(cemex) <- c("MXX.Open", "MXX.High", "MXX.Low", "MXX.Close", "MXX.Volume", "MXX.Adj.Close")
closing_values_cemex <- cemex$MXX.Close
returns_cemex <- log(closing_values_cemex[2:length(closing_values_cemex)] / closing_values_cemex[1:(length(closing_values_cemex)-1)])

#plot(cemex)
#plot(closing_values_cemex, type="l")
#plot(returns_cemex)

ica <- yahooSeries("ICA.MX", from="2003-01-01", to="2012-09-07", frequency="daily")
names(ica) <- c("MXX.Open", "MXX.High", "MXX.Low", "MXX.Close", "MXX.Volume", "MXX.Adj.Close")
closing_values_ica <- ica$MXX.Close
returns_ica <- log(closing_values_ica[2:length(closing_values_ica)] / closing_values_ica[1:(length(closing_values_ica)-1)])
#plot(ica)
#plot(closing_values_ica, type="l")
#plot(returns_ica)

# Create the matrix mi_matrix to be able to apply the Cholesky decomposition and use one of the matrix to generate the prediction:

mi_matrix <- matrix(c(1, cor(returns_ica, returns_cemex, use="na.or.complete"), cor(returns_ica, returns_cemex, use="na.or.complete"), 1), 2, 2)
hola <- chol(mi_matrix)
t(hola)
t(hola) %*% hola

g <- rnorm(2)

mu <- c(mean(returns_cemex), mean(returns_ica))

mu + t(hola) %*% g
