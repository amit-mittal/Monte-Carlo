conditional_normal<-function(num){

print("Enter the value of rho : ")
rho<-scan(n=1)

a<-c(950706376, 16807)
m<-c((2^31)-1, (2^31)-1)
y<-c(7, 1133)
u<-array(2)
z<-array(2)
x<-array(2)
mu1<-5
mu2<-8
sd1<-1
sd2<-2

arraysummary_x1<-array()
arraysummary_x2<-array()
countnum<-0
	
	while(countnum < num){
	
		y[1]<-(a[1]*y[1])%%m[1]
		y[2]<-(a[2]*y[2])%%m[2]
		u[1]<-y[1]/m[1]
		u[2]<-y[2]/m[2]
		
		R<-sqrt(-2*log(u[2]))
		V<-(2*pi*u[1])
		
		z[1]<-R*cos(V)
		z[2]<-R*sin(V)
		
		x[1]<-mu1+(sd1*z[1])

		mueff<-(mu2 + (rho*sd2*((x[1]-mu1)/sd1)))
		sdeff<-sqrt(sd2*sd2*(1-(rho^2)))

		x[2]<-mueff+(sdeff*z[2])		

		countnum<-countnum+1
		arraysummary_x1[countnum]<-x[1]
		arraysummary_x2[countnum]<-x[2]
	}
	
	#hist(arraysummary_x1, 50)
	print(summary(arraysummary_x1))
	print(summary(arraysummary_x2))
	print(var(arraysummary_x1))
	print(var(arraysummary_x2))
	print(cor(arraysummary_x1, arraysummary_x2))

	#library(MASS)
	#bivn.kde <- kde2d(arraysummary_x1, arraysummary_x2, n = 100)	# Doing a 2D kernel density estimate

	#contour(bivn.kde, nlevels = 10, vfont=c('sans serif', 'bold'), labcex = 0.65, col = "black", lwd = 1.5, main = paste("Contour Plot of Bivariate Normal Distribution (10 levels)\n[",num," values]"), xlab = "X (Random Variable X1) ------------>", ylab = "Y (Random Variable X2) ------------>", sub = bquote("\n"~bold(mu[1])==.(5)~", "~sigma[1]==.(1)~", "~mu[2]==.(8)~", "~sigma[2]==.(2)~", "~rho==.(rho)))	#Plotting the contours
	#grid() 

	#dev.copy(jpeg, "contour(q3)5.jpeg")
	#dev.off()

	#persp(bivn.kde, phi = 30, theta = 30, d = 3, lphi = 45, ltheta = 30, ticktype = "detailed", nticks = 6, border = "black", shade = 0.1, xlab = "\nX (Random Variable X1) ------------>", ylab = "\nY (Random Variable X2) ------------>", zlab = "\n\n\n\nProbability - \nJoint Density f(x,y) ------------>\n\n", expand = 0.5, main = paste("Probability Density Plot For a Bivariate Normal Distribution\n [",num," values]"), sub = bquote(bold(mu[1])==.(5)~", "~sigma[1]==.(1)~", "~mu[2]==.(8)~", "~sigma[2]==.(2)~", "~rho==.(rho))) 
	#dev.copy(jpeg, "persp(q3)2.jpeg")
	#dev.off()
}

conditional_normal(1000)
