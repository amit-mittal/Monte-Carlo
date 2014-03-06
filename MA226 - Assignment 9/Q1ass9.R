get_confidence<-function(num){

	set.seed(12139)
	u<-runif(num)

	y<-vector()
	alpha<-0.05

	y<-exp(sqrt(u))

	#set.seed(321437)
	#normal<-rnorm(num)
	#quant<-quantile(normal, 1-(alpha/2))

	quant<-1.96
	variance<-var(y)
	expectn<-mean(y)
	stddev<-sqrt(variance)

	leftend<-(expectn - (quant*stddev)/sqrt(num))
	rightend<-(expectn + (quant*stddev)/sqrt(num))

	print(paste("The 95% confidence interval (for I) for M =", num, " is [", leftend, ",", rightend, "]"), quote = FALSE)
}

for(i in 2:5)
    get_confidence(10^i)
