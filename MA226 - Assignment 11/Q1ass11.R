geometric_brownian<-function(sample, mu, sigma){

	set.seed(59)		#53, //59// are good seeds; #47 is an excellent seed.
	num_points <- 1000
	total <- num_points*sample

	Z<-rnorm(total)
	S<-vector()
	S5 <- vector() 

	count<-1
	col<-0

	variance<-sigma^2
	Interval<-5/num_points

	max<-vector()
	min<-vector()

	#svg("Q1(4).svg")

	while(count <= total){

		S[1] <- 5
		temp_count<-0
		t<-1

		while(temp_count < num_points){

			S[t+1] <- S[t] * exp((mu-(variance/2))*Interval + (sigma*Z[count]*sqrt(Interval)))
			t<-t+1
			count<-count+1
			temp_count<-temp_count+1
		}
		
		col<-col+1
		
		S5[col] <- S[1000]

		max[col] <- max(S)
		min[col] <- min(S)

		#if(col==1){

			#plot(S, type = "l", col = col, ylim = c(0, 10), xaxt = "n", xlab = "Time (t) ----------------------->", ylab = "S(t)   [Geometric Brownian Motion Variate] ----------------------->", main = paste("Combined Plot : \n10 Sample GEOMETRIC BROWNIAN MOTION Paths\nTime Interval = [0,5]"), sub = bquote("\n\n"~bold(mu)==.(mu)~", "~sigma==.(sigma)))

			#axis(side = 1, at = c(0,1000,2000,3000,4000,5000), labels = c(0,1,2,3,4,5))
		#}

		#else{
			#par(new = TRUE)
			#lines(S, type = "l", col = col, xlab = "", ylab = "", ylim = c(0,  10), lwd = 0.7)
		#}
		
		#grid()

	}

	#dev.off()

	S5_mean_theoretical <- S[1]*exp(mu*5)
	S5_var_theoretical <- (S[1]^2)*exp(2*mu*5)*(exp(variance*5) - 1)

	print(max(max))
	print(min(min))

	#plot(density(S5))
	mean = log(S[1]) + (mu - variance/2)*5
	var = variance*5

	#plot(density(S5), col = "blue", main = paste("Density plots of the Actual and Simulated Distributions [Lognormal]"), xlab = "X --------------->", ylab = "Desnity --------------->", sub = bquote("\n\n"~bold(mu)==.(mu)~", "~sigma==.(sigma)))
	#par(new = TRUE)
	
	#curve(dlnorm(x, meanlog = mean, sdlog = sqrt(var), log = FALSE), 0.35, 2.35, col = "red", axes = FALSE, xlab = "", ylab = "")
	#(0.33, 2.37), (7.7, 45), (0, 4), (0, 80)
	#legend('topleft', legend = c("Emperical", "Theoretical"), lty = 1, col = c("blue", "red"))

	#grid()

	print(paste("Expected Value of S[5] (Experimental) : ", mean(S5)), quote = FALSE)
	print(paste("Expected Value of S[5] (Theoretical) : ", S5_mean_theoretical), quote = FALSE)
	print(paste("Variance of S[5] (Experimental) : ", var(S5)), quote = FALSE)
	print(paste("Variance of S[5] (Theoretical) : ", S5_var_theoretical), quote = FALSE)
}

geometric_brownian(10, 0.3, 0.2)	#(0.3, -0.3) ; (0.1, 0.2)
