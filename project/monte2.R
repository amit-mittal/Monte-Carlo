generate_LCG <- function(a, m, x){
	x_new = (a*x)%%m
	return(x_new)
}

generate_rand <- function(x, m){
	return(x/m)
}

generate_exp <- function(lambda, random){
	val = -log(random)/lambda
	return(val)
}

generate_F <- function(lambda, random){
	val = lambda - 1 + sqrt(((1+lambda)**2) - (4*random*lambda))
	val = val/(2*lambda)
	val = -log(val)/lambda
	return(val)
}

method_InverseTransform <-function(lambda){
	a = 39373
	m = (2**31) - 1
	seed = 5
	x = seed
	bound = 10000
	count = 1
	X = array(bound)
	
	while(count<=bound){
		x = generate_LCG(a, m, x)
		rand = generate_rand(x, m)
		f = generate_F(lambda, rand)

		X[count] = f;

		cat(sprintf("X = %g\trand = %g\n", X[count], rand))

		count = count + 1;
	}

	hist(X, 100,freq = TRUE, include.lowest = TRUE, right = TRUE, density = NULL,
		angle = 45, col = '#1E90FF', border = NULL,
		main = paste("Histogram of frequencies for F and total values
		generated=",bound), xlab = "Random Numbers" ,
		axes = TRUE, plot = TRUE, labels = FALSE);
	dev.copy(jpeg,paste("F.jpg"));
	dev.off();
}

method_AcceptanceRejection <-function(lambda){

}


lambda = 2

method_InverseTransform(lambda)