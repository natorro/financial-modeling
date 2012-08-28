
m <- 4
for (n in c(10, 100, 1000, 10000, 100000, 1000000)){
	var_i <- rep(NA, n)
	mean_i <- rep(NA, n)
	random_numbers <- runif(n)

	mean_i[1] <- 0
	var_i[1] <- 0
	
	# The function to evaluate is the function we apply to the random_numbers vector
	function_numbers <- exp(random_numbers)

	for(i in 2:n){
		mean_i[i] <- mean_i[i-1] + (function_numbers[i]-mean_i[i-1])/i
		var_i[i] <- (1- (1/(i-1)))*var_i[i-1] + (i * ((mean_i[i]-mean_i[i-1])^2))
	}
	
		
	lim_inf <- mean_i[n] - ((1.96 * sqrt(var_i[n]))/sqrt(n))
	lim_sup <- mean_i[n] + ((1.96 * sqrt(var_i[m]))/sqrt(n))
	if (n == 10)
		print("Resultados para N de la Aproximación, Límite inferior, Límite superior y Longitud\n")
	print(paste(n, "-", mean_i[n], "-", lim_inf, "-", lim_sup, "-", lim_sup-lim_inf))

}
