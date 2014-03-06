generateDISACCREJ<-function(num){

a<-c(16807, 40692)
m<-c(((2^31)-1), 2147483399)
x0<-c(5,5)
u<-array(2)
y<-array(2)
countnum<-0
countaccept<-0
arraysummary<-array()
q = c(0.2, 0.2, 0.2, 0.2, 0.2)
p = c(0.05, 0.25, 0.45, 0.15, 0.10)
countfreq<-numeric(5)
M<-2.25

y[1]<-(a[1]*x0[1])%%m[1]
y[2]<-(a[2]*x0[2])%%m[2]
u[1]<-y[1]/m[1]
u[2]<-y[2]/m[2]

	while(countaccept < num){
	
		index<-1
		prob<-0
		
		while(index <= 5){
		
			prob<-prob+q[index]
			if(u[1] < prob)
				break
				
			index<-index+1
		}
		
		if(u[2]<=(p[index]/(M*q[index]))){
			#print(paste("Accepted : ", index), quote = FALSE)
			countaccept<-countaccept+1
			countfreq[index]<-countfreq[index]+1
			arraysummary[countnum]<-index
		}
		
		countnum<-countnum+1
		y[1]<-(a[1]*y[1])%%m[1]
		y[2]<-(a[2]*y[2])%%m[2]
		u[1]<-y[1]/m[1]
		u[2]<-y[2]/m[2]
	}
	print(paste("Number of values generated for testing : ", countnum), quote = FALSE)
	print(paste("Number of values accepted : ", countaccept), quote = FALSE)
	print(paste("Probability of Acceptance (Experimental) : ", countaccept/countnum), quote = FALSE)
	print(paste("Probability of Acceptance (Theoretical) : ", 1/M), quote = FALSE)
	print(summary(arraysummary))
	print(countfreq)
	plot(countfreq, xlab = "X (Values of the Random numbers)----------->", ylab = "FREQUENCY (Of the Random Numbers)----------->", col = rgb(1,0,0,1), main = paste("HISTOGRAM OF FREQUENCIES (Using Rejection Sampling) : \nNumber of Random numbers generated = ", num))
	dev.copy(jpeg, "Q3b.jpeg")
	dev.off()
}

generateDISACCREJ(10)
