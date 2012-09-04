for (m in c(10, 100, 1000, 10000, 100000)){
	S0 <- 100 
	K <- 110
	r <- 0.04
	sigma <- 0.4

	var_i <- rep(NA, m)
	mean_i <- rep(NA, m)

	var_i[1] <- 0
	mean_i[1] <- 0

	random_numbers <- rnorm(m)

	function_numbers <- S0 * exp( (r-((sigma^2)/2)) + sigma*random_numbers)
	for (i in 1:m){
		random_numbers[i] <- max(random_numbers[i], 0)
	}

	for(i in 2:m){
		mean_i[i] <- mean_i[i-1] + (function_numbers[i]-mean_i[i-1])/i
		var_i[i] <- (1- (1/(i-1)))*var_i[i-1] + (i * ((mean_i[i]-mean_i[i-1])^2))
	}
		lim_inf <- mean_i[m] - ((1.96 * sqrt(var_i[m]))/sqrt(m))
		lim_sup <- mean_i[m] + ((1.96 * sqrt(var_i[m]))/sqrt(m))
		if (m == 10)
			print("Resultados para M de la Aproximación, Límite inferior, Límite superior y Longitud\n")
		print(paste(m, "-", mean_i[m], "-", lim_inf, "-", lim_sup, "-", lim_sup-lim_inf))
	
}






