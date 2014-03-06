generate_weibull_1<-function(u){
	
	answer<-(-log(1-u))
	return(sqrt(answer))
}

generate_weibull_2<-function(u){
		
	answer<-(-log(1-u))
	return(answer^(2/3))
}

generate_composition<-function(num){

a<-c(950706376, 16807)
m<-c((2^31)-1, (2^31)-1)
y<-c(23, 23)
u<-array(2)
countnum<-0
count_weibull<-numeric(2)
p<-0.4
arrayhist<-array()

	while(countnum < num){

		y[1]<-(a[1]*y[1])%%m[1]
		y[2]<-(a[2]*y[2])%%m[2]
		u[1]<-y[1]/m[1]
		u[2]<-y[2]/m[2]

		if(u[2] < p){
		    x<-generate_weibull_1(u[1])
		    count_weibull[1]<-count_weibull[1]+1
		}

		else{
		    x<-generate_weibull_2(u[1])
		    count_weibull[2]<-count_weibull[2]+1
		}

		print(x)
		countnum<-countnum+1
		arrayhist[countnum]<-x
	}

	hist(arrayhist, 15, xlab = "U (Random Numbers) ---------->", ylab = "Frequencies (Of the random numbers generated) ---------->", col = rgb(0,0,1,0.1), main =  paste("HISTOGRAM OF FREQUENCIES\nComposition of Weibull Distributions\nNumber of values generated : ", num, "[15 Intervals]"))
	grid() 
	dev.copy(jpeg, "combo.jpeg")
	dev.off()

	#print(summary(arrayhist))
	#print(var(arrayhist))
	#print(count_weibull[1])
	#print(count_weibull[2])
	#print(paste("Probability of generating from 1st Weibull Distribution with (Beta = 2) : ", count_weibull[1]/num), quote = FALSE)
	#print(paste("Probability of generating from 2nd Weibull Distribution with (Beta = 1.5) : ", count_weibull[2]/num), quote = FALSE)
}

generate_composition(50)
		
