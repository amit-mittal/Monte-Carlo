generatemarsaglia<-function(num){

a<-c(16807,950706376)
m<-c((2^31)-1, (2^31)-1)
y<-c(11,11)
u<-array(2)
z<-array(2)
countnum<-0
countaccept<-0
#rej<-(1-(pi/4))

	while(countaccept < num){

		X<-2
		while(X > 1){

			y[1]<-(a[1]*y[1])%%m[1]
			y[2]<-(a[2]*y[2])%%m[2]
			u[1]<-y[1]/m
			u[2]<-y[2]/m
			u[1]<-(2*u[1])-1
			u[2]<-(2*u[2])-1
	
			X<-(u[1]^2 + u[2]^2)
			countnum<-countnum+2
		}

		Y<-sqrt((-2*log(X))/X)
		z[1] = u[1]*Y
       	 	z[2] = u[2]*Y
		countaccept<-countaccept+2
		print(z[1])
		print(z[2])
	}
	#r<-countnum-countaccept
	#rper<-r/countnum

	#print(paste("This is exp : ", rper), quote = FALSE)
	#print(rej)
}

generatemarsaglia(500)
	
