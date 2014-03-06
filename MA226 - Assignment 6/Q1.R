generateboxmuller<-function(num){

a<-c(16807,950706376)
m<-c((2^31)-1, (2^31)-1)
y<-c(11,11)
u<-array(2)
z<-array(2)
arrayhistz<-array()
countnum<-0

   while(countnum < num){
	y[1]<-(a[1]*y[1])%%m[1]
	y[2]<-(a[2]*y[2])%%m[2]
	u[1]<-y[1]/m
	u[2]<-y[2]/m

	R<-(-2)*log(u[1])
	V<-(2*pi*u[2])
	z[1]<-sqrt(R)*cos(V)
	z[2]<-sqrt(R)*sin(V)
	
	countnum<-countnum+1
	arrayhistz[countnum]<-z[1]
	countnum<-countnum+1
	arrayhistz[countnum]<-z[2]
	
	print(z[1])
	print(z[2])
   }
   
   #print(paste("The Variance of random numbers : ", var(arrayhistz)), quote = FALSE)
   #print("The summary of the random numbers : ", quote = FALSE)
   #print(summary(arrayhistz))
  #hist(arrayhistz, 100, xlab = "Random Numbers (Z) ------->", ylab = "Frequency of the Random Numbers ------->", col = rgb (1,0,1,0.1), main = paste("HISTOGRAM OF FREQUENCIES \nNORMAL DISTRIBUTION (Box-Muller Method) : \nNumber of values generated = ", num, "[100 INTERVALS]"))
  #grid()
  #dev.copy(jpeg, "Q1a(6).jpeg")
  #dev.off()
   
}

generatemarsaglia<-function(num){

a<-c(16807,950706376)
m<-c((2^31)-1, (2^31)-1)
y<-c(11,11)
u<-array(2)
z<-array(2)
arrayhistz<-array()
countnum<-0
countaccept<-0
rej<-(1-(pi/4))

   while(countaccept < num){
	y[1]<-(a[1]*y[1])%%m[1]
	y[2]<-(a[2]*y[2])%%m[2]
	u[1]<-y[1]/m
	u[2]<-y[2]/m
	u[1]<-(2*u[1])-1
	u[2]<-(2*u[2])-1
	
	X<-(u[1]^2 + u[2]^2)
	
	if(X <= 1){
	   Y<-sqrt((-2*log(X))/X)
	   z[1] = u[1]*Y
	   z[2] = u[2]*Y
	   countaccept<-countaccept+1
	   arrayhistz[countaccept] = z[1]
	   countaccept<-countaccept+1
	   arrayhistz[countaccept] = z[2] 
	   print(z[1])
	   print(z[2]) 
	}
	
	countnum<-countnum+2	
   }
   
   rejected<-countnum-countaccept
   print(paste("Number of values generated : ", countnum), quote = FALSE)
   print(paste("Number of values accepted : ", countaccept), quote = FALSE)
   print(paste("Number of values rejected : ", rejected), quote = FALSE)
   print(paste("Proportion of numbers rejected (Experimental) : ", rejected/countnum), quote = FALSE)
   print(paste("Proportion of numbers rejected (Theoretical) : ", rej), quote = FALSE)
   #print(paste("The Variance of random numbers : ", var(arrayhistz)), quote = FALSE)
   #print("The summary of the random numbers : ", quote = FALSE)
   #print(summary(arrayhistz))
   #hist(arrayhistz, 100, xlab = "Random Numbers (Z) ------->", ylab = "Frequency of the Random Numbers ------->", col = rgb(1, 0, 1, 0.1), main = paste("HISTOGRAM OF FREQUENCIES \nNORMAL DISTRIBUTION (Marsaglia-Bray Method) : \nNumber of values generated = ", num, "[100 INTERVALS]"))
   #grid()
   #dev.copy(jpeg, "Q1a1(6).jpeg")
   #dev.off()
}	


#generateboxmuller(100)
generatemarsaglia(10000)



