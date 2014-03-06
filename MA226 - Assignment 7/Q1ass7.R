extend<-function(x, n, end){

	for(i in end:n)
	    x[i]<-0

	return(x)
}

generate_geometric<-function(num){

p<-0.4

a<-40692
m<-2147483399
y<-1133
countnum<-0
max<-(-1)
arraysummary<-array()
countfreq<-array()

	while(countnum < num){
		y<-(a*y)%%m
		u<-y/m
		x<-floor(log(u)/log(1-p))+1

		if(max < x)
		    max<-x

		len<-length(countfreq)
		if(len < x)
		    countfreq<-extend(countfreq, x, len)

		print(x)
		countnum<-countnum+1
		arraysummary[countnum]<-x
		countfreq[x]<-countfreq[x]+1
	}

	print(summary(arraysummary))
	print(var(arraysummary))
	#print(countfreq)

	for(i in 1:max)
	    countfreq[i]<-countfreq[i]/num

	#plot(countfreq, type = 'o', pch = 21, col = "red", bg = "blue", main = paste("Probability Mass Function (PMF)\nGeometric Variate - Discrete (p = 0.4)"), xlab = "X (Random Number) --------> ", ylab = "P(X = x)   [pmf] -------->")
	#grid()
	#dev.copy(jpeg, "Geometric(3).jpeg")
	#dev.off()
}

generate_geometric(50)


