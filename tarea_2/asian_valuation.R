# We try different sizes of n, in this cas, 10, 100, 1000 and 10000.
for (m in c(10, 100, 1000, 10000)){

	S0 <- 100
	K <- 100
	r <- 0.04
	sigma <- 0.2
	h <- 200
	T_values <- 0.5/h
	random_numbers <- rep(NA, m)
	
	for(i in 1:m){
		# Initialize the value of the underlying at each new path
		S0 <- 100
		# Random vector for the new path
		random_walk <- rnorm(h)

		for(j in 1:h){
			random_walk[j] <- S0 * exp( (r-((sigma^2)/2))*T_values + sigma*random_walk[j]*sqrt(T_values))
			S0 <- random_walk[j]
		}
		#To valuate, we just need the mean of it
		random_numbers[i] <- mean(random_walk)

        # Uncomment only if you can see the trajectories
		if(i == 1 && m == 100) plot(random_walk, type="l", ylim=c(0, 200), xlab="t", ylab="S(t)")
		else if (m == 100) lines(random_walk)
		
	}
	# Get the limits of the confidence interval
	lim_inf <- mean(random_numbers) - ((1.96 * sqrt(var(random_numbers)))/sqrt(m))
	lim_sup <- mean(random_numbers) + ((1.96 * sqrt(var(random_numbers)))/sqrt(m))
		
	if (m == 10)
	print("Results for M of the approximation, lower and upper limits and range\n")
	print(paste("m=", m, "mean=", mean(random_numbers), "lower=", lim_inf, "upper", lim_sup, "range=", lim_sup-lim_inf))
}
