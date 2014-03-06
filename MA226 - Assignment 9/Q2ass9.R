get_confidence_antithetic<-function(num){

	set.seed(12139)
	u<-runif(num)

	y<-vector()
	alpha<-0.05

  	y<-exp(sqrt(u))
	var_old<-var(y)/num

	y<-0.5*(exp(sqrt(u))+exp(sqrt(1-u)))
	var_new<-var(y)/num

	var_reduction<-(var_old-var_new)*(100/var_old)

	print(paste("New variance : ", var_new), quote = FALSE)
	print(paste("Old variance : ", var_old), quote = FALSE)
	print(paste("Variance reduction : ", var_reduction, "%"), quote = FALSE)

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
    get_confidence_antithetic(10^i)
