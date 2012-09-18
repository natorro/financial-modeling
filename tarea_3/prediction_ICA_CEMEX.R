library(fImport)
#library(nlme)
#library(Hmisc)
#library(tseries)
#library(nortest)

# Let's get the values of the indexes
# using correlations.
# We get ICA.MX y URBI.MX

ica <- yahooSeries("ICA.MX", from="2003-01-01", to="2012-09-07", frequency="daily")
names(ica) <- c("Open", "High", "Low", "Close", "Volume", "Adj.Close")
closing_values_ica <- ica$Close[length(ica$Close):1]
returns_ica <- log(closing_values_ica[2:length(closing_values_ica)] / closing_values_ica[1:(length(closing_values_ica)-1)])


#plot(closing_values_ica, type="l")
#plot(returns_ica, type="l")


urbi <- yahooSeries("CEMEXCPO.MX", from="2003-01-01", to="2012-09-07", frequency="daily")
names(urbi) <- c("Open", "High", "Low", "Close", "Volume", "Adj.Close")
closing_values_urbi <- urbi$Close[length(urbi$Close):1]
returns_urbi <- log(closing_values_urbi[2:length(closing_values_urbi)] / closing_values_urbi[1:(length(closing_values_urbi)-1)])


#plot(closing_values_urbi, type="l")
#plot(returns_urbi, type="l")

# Create the matrix mi_matrix to be able to apply the Cholesky decomposition and use one of the matrix to generate the prediction:
length(returns_urbi)
length(returns_ica)
mi_matrix <- matrix(c(1, cor(returns_ica, returns_urbi, use="pairwise.complete.obs", method="pearson"), cor(returns_ica, returns_urbi, use="pairwise.complete.obs", method="pearson"), 1), 2, 2)

mi_matrix

decomp<- chol(mi_matrix)

L <- t(decomp)

L 

L %*% t(L)
mu <- c(mean(returns_ica), mean(returns_urbi))
mu
holaa <- 1:1000
holab <- 1:1000

for (j in 1:1000){
	promediosa <- 1:100
	promediosb <- 1:100
	for (i in 1:100){
		g <- rnorm(2)
		a <- mu + L %*% g
		promediosa <- a[1, 1]
		promediosb <- a[2, 1]
	}

	holaa[j] <- mean(promediosa)
	holab[j] <- mean(promediosb)
	
}


hist(holaa)
hist(holab)