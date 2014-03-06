brownian_motion<-function(num){		#num is the number of sample paths required

	set.seed(345989)
	Z<-rnorm(5000*num)
	W<-vector()
	
	count <- 1
	Interval<-5/5000
	col<-0
	sum_w2<-0
	sum_w5<-0
	mylabels = c(1, 2, 3, 4, 5)

	svg("Q1.svg")			#Scalable Vector Graphics (SVG) Format

	while(count <= (num*5000)){

		temp_count<-0
		t<-1
		W[1]<-0

		while(temp_count < 5000){

			W[t+1] <- W[t] + sqrt(Interval)*Z[count]
			temp_count<-temp_count+1
			t<-t+1
			count<-count+1
		}
	
		sum_w2<-sum_w2 + W[2000]
		sum_w5<-sum_w5 + W[5000]
		
		if(col == 0){

			plot(W, type = "l", col = col, ylim = c(-7, 7), xaxt = "n", xlab = "Time (t) ----------------------->", ylab = "W(t)   [Brownian Motion Variate] ----------------------->", main = paste("Combined Plot of 10 Sample STANDARD BROWNIAN MOTION Paths\nTime Interval = [0,5]"), sub = bquote("\n\n"~bold(mu)==.(0)~", "~sigma==.(0)))

			axis(side = 1, at = c(0,1000,2000,3000,4000,5000), labels = c(0,1,2,3,4,5))
		}
	
		else
			lines(W, type = "l", col = col, lwd = 0.7)
		
		
		par(new=TRUE)
		
		grid()
		col<-col+1
	}

	dev.off()	

	print(paste("Expected Value of W[2] : ", sum_w2/10), quote = FALSE)
	print(paste("Expected Value of W[5] : ", sum_w5/10), quote = FALSE)
}

brownian_motion(10)


