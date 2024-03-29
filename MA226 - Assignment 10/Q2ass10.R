brownian_motion<-function(num){		#num is the number of sample paths required

	set.seed(345989)
	Z<-rnorm(5000*num)
	X<-vector()
	
	count <- 1
	Interval<-5/5000
	col<-0
	sum_x2<-0
	sum_x5<-0
	sigma<-0.3
	mu<-0.06

	svg("Q2.svg")			#Scalable Vector Graphics (SVG) Format

	while(count <= (num*5000)){

		temp_count<-0
		t<-1
		X[1]<-5

		while(temp_count < 5000){

			X[t+1] <- X[t] + sigma*sqrt(Interval)*Z[count] + mu*(Interval)
			temp_count<-temp_count+1
			t<-t+1
			count<-count+1
		}
	
		sum_x2<-sum_x2 + X[2000]
		sum_x5<-sum_x5 + X[5000]
	
		if(col == 0){

			plot(X, type = "l", col = col, ylim = c(2, 8), xaxt = "n", xlab = "Time (t) ----------------------->", ylab = "X(t)   [Brownian Motion Variate] ----------------------->", main = paste("Combined Plot of 10 Sample BROWNIAN MOTION Paths [X(0) = 5]\nTime Interval = [0,5]"), sub = bquote("\n\n"~bold(mu)==.(0.06)~", "~sigma==.(0.3)))

			axis(side = 1,at = c(0,1000,2000,3000,4000,5000),labels = c(0,1,2,3,4,5))
		}
	
		else
			lines(X, type = "l", col = col, lwd = 0.7)
		
		
		par(new=TRUE)
		
		grid()
		col<-col+1
	}

	dev.off()	

	print(paste("Expected Value of X[2] : ", sum_x2/10), quote = FALSE)
	print(paste("Expected Value of X[5] : ", sum_x5/10), quote = FALSE)
}

brownian_motion(10)


