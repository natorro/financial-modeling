#####  a) 
m <- 10

for (m in c(10, 100, 1000, 10000, 100000, 1000000)){
	var_i <- rep(NA, m)
	mean_i <- rep(NA, m)
	random_numbers <- runif(m)

	mean_i[1] <- 0
	var_i[1] <- 0

	# The function to evaluate is the function we apply to the random_numbers vector
	function_numbers <- 2/(4 + (3*sin(random_numbers)))

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

######   b)

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

##### c) 

for (m in c(10, 100, 1000, 5000, 10000, 100000, 1000000)){
	var_i <- rep(NA, m)
	mean_i <- rep(NA, m)
	random_matrix = matrix(runif(20*m), nrow=m)
#	function_numbers = matrix(NA, 20*m, m)

	mean_i[1] <- 0
	var_i[1] <- 0

	# The function to evaluate is the function we apply to the random_numbers vector
	random_matrix <- (pi/2) * sin(pi*random_matrix)
	for (i in 1:m)
		random_matrix[i, 1] <- prod(random_matrix[i, ])

	for(i in 2:m){
		mean_i[i] <- mean_i[i-1] + (random_matrix[i, 1]-mean_i[i-1])/i
		var_i[i] <- (1- (1/(i-1)))*var_i[i-1] + (i * ((mean_i[i]-mean_i[i-1])^2))
	}
	
		
	lim_inf <- mean_i[m] - ((1.96 * sqrt(var_i[m]))/sqrt(m))
	lim_sup <- mean_i[m] + ((1.96 * sqrt(var_i[m]))/sqrt(m))
	if (m == 10)
		print("Resultados para M de la Aproximación, Límite inferior, Límite superior y Longitud\n")
	print(paste(m, "-", mean_i[m], "-", lim_inf, "-", lim_sup, "-", lim_sup-lim_inf))

}


