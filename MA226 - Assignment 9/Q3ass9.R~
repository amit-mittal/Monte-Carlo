generate_control_variate<-function(num){

	set.seed(2343)
	u<-runif(num)

	Y<-sqrt(u)
	X<-exp(Y)
	var_old<-var(X)/num

	#c<-(-cov(X,Y)/var(Y))		#This is the value of 'c' obtained from the sample covariance and variance
	c<- -1.858145825		#This is the theoretical value obtained by integration; Do not calculate 'c' from sample covariance and 					#variance; 

	expec_Y<-mean(Y)
	
	W<-X+(c*(Y - expec_Y))
	var_new<-var(W)/num

	var_reduction<-(var_old-var_new)*(100/var_old)

	print(paste("New variance : ", var_new), quote = FALSE)
	print(paste("Old variance : ", var_old), quote = FALSE)
	print(paste("Variance reduction : ", var_reduction, "%"), quote = FALSE)

	quant<-1.96
	variance<-var(W)
	expectn<-mean(W)
	print(mean(W))
	stddev<-sqrt(variance)

	leftend<-(expectn - (quant*stddev)/sqrt(num))
	rightend<-(expectn + (quant*stddev)/sqrt(num))

	print(paste("The 95% confidence interval (for I) for M =", num, " is [", leftend, ",", rightend, "]"), quote = FALSE)
}

for(i in 2:5)
    generate_control_variate(10^i)
