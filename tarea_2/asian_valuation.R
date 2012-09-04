for (m in c(10, 100, 1000, 10000)){
#	m <- 100
	S0 <- 100
	K <- 100
	r <- 0.04
	sigma <- 0.2
	h <- 200
	T_values <- 0.5/h
	random_numbers <- rep(NA, m)
	
	var_i <- rep(NA, m)
	mean_i <- rep(NA, m)
	var_i[1] <- 0
	mean_i[1] <- 0

	for(i in 1:m){
		S0 <- 100
		random_walk <- rnorm(h)

		for(j in 1:h){
			random_walk[j] <- S0 * exp( (r-((sigma^2)/2))*T_values + sigma*random_walk[j]*sqrt(T_values))
			S0 <- random_walk[j]
		}
		random_numbers[i] <- mean(random_walk)

# Uncomment only if you can see the trajectories
		if(i == 1 && m == 10) plot(random_walk, type="l", ylim=c(0, 200), xlab="t", ylab="S(t)")
		else if (m == 100) lines(random_walk)
		
	}

		lim_inf <- mean(random_numbers) - ((1.96 * sqrt(var(random_numbers)))/sqrt(m))
		lim_sup <- mean(random_numbers) + ((1.96 * sqrt(var(random_numbers)))/sqrt(m))
		
		if (m == 10)
		print("Resultados para M de la Aproximación, Límite inferior, Límite superior y Longitud\n")
		print(paste(m, "-", mean(random_numbers), "-", lim_inf, "-", lim_sup, "-", lim_sup-lim_inf))
}
