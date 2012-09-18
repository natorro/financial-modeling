
set.seed(10)
r <- 0.07
mu <- 0.06
v <- 0.000098
alpha <- 0.00007056
kappa <- 0.40
gamma <- 0.8 
xi <-0.0025 
rho <- 0.5

vs <- 1:5
rs <- 1:5


for (i in 1:5){
	dz.1 <- rnorm(1)
	dz.2 <- (rho * dz.1) + ((1 + rho^2) * rnorm(1))
	dv <- (gamma * (alpha - v)) + (xi * sqrt(v) * dz.2)
	v <- v + dv
	dr <- (kappa*(mu - r)) + (sqrt(v)*dz.1)
	r <- r + dr
	vs[i] <- v
	rs[i] <- r
}



n <- 5
index <- 1:n
pagos <- 1:n
saldo_inicial <- 1000000
pago <- 243891
saldo_insoluto <- saldo_inicial
rates <- rs
mis_pagos <- 1:n

for(j in 1:n) {
	if (rates[j] < 0.06){
		pago <- pago + min(100000, saldo_insoluto)
	}
		
	saldo_insoluto <- saldo_insoluto - (pago - (saldo_insoluto * rates[j]))

	mis_saldos[j] <- saldo_insoluto
}


#rm(list = ls())
#pagos <- 1:5
#n <- 5
#saldo_inicial <- 1000000
#pago <- 243891
#saldo_insoluto <- saldo_inicial
#rates <- rep(0.07, n)
#mis_saldos <- 1:n
#for(j in 1:n) {
#	saldo_insoluto <- saldo_insoluto - (pago - (saldo_insoluto * rates[j]))
#	mis_saldos[j] <- saldo_insoluto
#}
