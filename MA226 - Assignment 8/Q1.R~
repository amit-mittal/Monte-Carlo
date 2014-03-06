multivariate_normal<-function(num){

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
		
		x[1]<-5+z[1]
		x[2]<-8+(2*rho*z[1])+(2*sqrt(1-(rho^2))*z[2])
		
		countnum<-countnum+1
		arraysummary_x1[countnum]<-x[1]
		arraysummary_x2[countnum]<-x[2]
	}
	
	print(summary(arraysummary_x1))
	print(summary(arraysummary_x2))
	print(var(arraysummary_x1))
	print(var(arraysummary_x2))
	print(cor(arraysummary_x1, arraysummary_x2))

	#library(MASS)
	#bivn.kde <- kde2d(arraysummary_x1, arraysummary_x2, n = 100)	# Doing a kernel density estimate
	#contour(bivn.kde, nlevels = 10, vfont=c('sans serif', 'bold'), labcex = 0.65, col = "black", lwd = 1.5, main = paste("Contour Plot of Bivariate Normal Distribution (10 levels)"), xlab = "X (Random Variable X1) ------------>", ylab = "Y (Random Variable X2) ------------>", sub = bquote("\n"~bold(mu[1])==.(5)~", "~sigma[1]==.(1)~", "~mu[2]==.(8)~", "~sigma[2]==.(2)~", "~rho==.(rho)))	#Plotting the contours
	#grid() 
	
	#dev.copy(jpeg, "contour.jpeg")
	#dev.off()

	#persp(bivn.kde, phi = 30, theta = 30, d = 3, lphi = 45, ltheta = 30, ticktype = "detailed", nticks = 6, border = "black", shade = 0.1, xlab = "\nX (Random Variable X1) ------------>", ylab = "\nY (Random Variable X2) ------------>", zlab = "\n\n\n\nProbability - \nJoint Density f(x,y) ------------>\n\n", expand = 0.5, main = paste("Probability Density Plot For a Bivariate Normal Distribution"), sub = bquote(bold(mu[1])==.(5)~", "~sigma[1]==.(1)~", "~mu[2]==.(8)~", "~sigma[2]==.(2)~", "~rho==.(rho))) 
	#dev.copy(jpeg, "persp5.jpeg")
	#dev.off()

	 #plot(ecdf(arraysummary_x1), verticals = TRUE, lwd = 2, pch = 46, col = "blue", xlab = "X1 (Random Variable) ---------------->", ylab = "Cumulative Distribution Function of X1 : F(x1) ---------------->", xaxs = "i", yaxs = "i", main = paste("Plot of MARGINAL CDF :\n Bivariate Normal Distribution [1000 values]\nCorrelation Coefficient (rho) = ", rho))

	#plot(ecdf(arraysummary_x2), verticals = TRUE, lwd = 2, pch = 46, col = "blue", xlab = "X2 (Random Variable) ---------------->", ylab = "Cumulative Distribution Function of X2 : F(x2) ---------------->", xaxs = "i", yaxs = "i", main = paste("Plot of MARGINAL CDF :\n Bivariate Normal Distribution [1000 values]\nCorrelation Coefficient (rho) = ", rho))
   
   	 #grid()
  	 #legend('topleft', legend = c("Emperical", "Theoretical"), lty = 1, col = c("blue", "red"))
   
  	 #par(new=TRUE)
   
  	 #curve(pnorm(x, mean = 8, sd = 2), xaxs = "i", yaxs = "i", lower.tail = FALSE, log.p = FALSE, 1.1, 14, col = "red", lwd = 2, axes = FALSE, xlab = "", ylab = "")

	#curve(pnorm(x, mean = 5, sd = 1), xaxs = "i", yaxs = "i", lower.tail = FALSE, log.p = FALSE, 1.6, 8.3, col = "red", lwd = 2, axes = FALSE, xlab = "", ylab = "")
	#dev.copy(jpeg, "combined1x2.jpeg")
	#dev.off()

}

multivariate_normal(1000)
		
