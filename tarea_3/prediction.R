library(fImport)
library(nlme)
library(Hmisc)
library(tseries)
library(nortest)

# Let's get the values of the indexes
# using correlations.
# We get ICA.MX y URBI.MX

ica <- yahooSeries("ICA.MX", from="2011-01-01", to="2012-09-07", frequency="daily")
names(ica) <- c("Open", "High", "Low", "Close", "Volume", "Adj.Close")
closing_values_ica <- ica$Close
returns_ica <- log(closing_values_ica[2:length(closing_values_ica)] / closing_values_ica[1:(length(closing_values_ica)-1)])


#plot(closing_values_ica, type="l")
#plot(returns_ica, type="l")


urbi <- yahooSeries("URBI.MX", from="2011-01-01", to="2012-09-07", frequency="daily")
names(urbi) <- c("Open", "High", "Low", "Close", "Volume", "Adj.Close")
closing_values_urbi <- urbi$Close
returns_urbi <- log(closing_values_urbi[2:length(closing_values_urbi)] / closing_values_urbi[1:(length(closing_values_urbi)-1)])


#plot(closing_values_urbi, type="l")
#plot(returns_urbi, type="l")

# Create the matrix mi_matrix to be able to apply the Cholesky decomposition and use one of the matrix to generate the prediction:

mi_matrix <- matrix(c(1, cor(returns_ica, returns_urbi, use="pairwise.complete.obs", method="pearson"), cor(returns_ica, returns_urbi, use="pairwise.complete.obs", method="pearson"), 1), 2, 2)

mi_matrix

decomp<- chol(mi_matrix)

L <- t(decomp)

L 

L %*% t(L)

g <- rnorm(2)
g
mu <- c(mean(returns_ica), mean(returns_urbi))
mu
mu + L %*% g
