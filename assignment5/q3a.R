generateDIS<-function(num){

a<-40692
m<-2147483399
x0<-11
countnum<-0
countfreq<-numeric(5)
arraysummary<-array()

y<-(a*x0)%%m
u<-y/m
p<-c(0.05, 0.25, 0.45, 0.15, 0.10)

	while(countnum < num){
	
		prob<-0
		index<-1
		while(index<=5){
		
			prob<-prob+p[index]
			if(u<prob)
				break
				
			index<-index+1
		}
		
		#print(index)
		countnum<-countnum+1
		countfreq[index]<-countfreq[index]+1
		arraysummary[countnum]<-index
		y<-(a*y)%%m
		u<-y/m
	}
	print(summary(arraysummary))	
	print(countfreq)
	plot(countfreq, xlab = "X (Values of the Random numbers)----------->", ylab = "FREQUENCY (Of the Random Numbers)----------->", col = rgb(1,0,0,1), main = paste("HISTOGRAM OF FREQUENCIES (Using Rejection Sampling) : \nNumber of Random numbers generated = ", num))
	dev.copy(jpeg, "Q3a.jpeg")
	dev.off()
}

generateDIS(10)
		
		
