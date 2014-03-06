extend<-function(x, n, end){

	for(i in end:n)
	    x[i]<-0

	return(x)
}

increcursive<-function(p, i){

	return((2*p)/(i+1))
}

generate_poisson<-function(num){

a<-40692
m<-2147483399
y<-23
countnum<-0
max<-(-1)
countfreq<-array()
arraysummary<-array()

	while(countnum<num){

		y<-(a*y)%%m
		u<-y/m
		i<-0
		p<-exp(-2)
		F<-p

		while(u>=F){

			p<-increcursive(p, i)
			F<-F+p
			i<-i+1
		}

		if(max < i)
		    max<-i

		len<-length(countfreq)
		if(len < i)
		    countfreq<-extend(countfreq, i, len)

		print(i)
		countfreq[i]<-countfreq[i]+1
		countnum<-countnum+1
		arraysummary[countnum]<-i
	}
	print(summary(arraysummary))
	print(var(arraysummary))
	print(countfreq)

	for(i in 1:max)
	    countfreq[i]<-countfreq[i]/num

	#plot(countfreq, type = 'o', pch = 21, col = "red", bg = "blue", main = paste("Probability Mass Function (PMF)\nPoisson Variate - Discrete (p = 0.4)"), xlab = "X (Random Number) --------> ", ylab = "P(X = x)   [pmf] -------->")
	#grid()
	#dev.copy(jpeg, "Poisson(3).jpeg")
	#dev.off()

	#plot(ecdf(arraysummary), pch = 21, col = "red", main = paste("Cumulative Distribution Function (CDF)\nPoisson Variate - Discrete (Mean = 2)"), xlab = "X (Random Number) --------> ", ylab = "P(X <= x)   [cdf] -------->")
	#grid()
	#dev.copy(jpeg, "Poisson_cdf(1).jpeg")
	#dev.off()

}
		
generate_poisson(50)
